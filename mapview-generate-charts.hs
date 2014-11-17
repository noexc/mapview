module Main where

import Control.Applicative
import Control.Lens
import Data.Monoid (mempty)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Options.Applicative hiding (Failure, Parser, Success)
import Text.Trifecta hiding (line)

import KD8ZRC.Mapview.Parser
import KD8ZRC.Mapview.Types

main :: IO ()
main = execParser opts >>= runMain
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> progDesc "Process the raw telemetry log and generate charts from it"
     <> header "mapview-generate-charts - Generate charts from raw telemetry")

runMain :: CLIOptions -> IO ()
runMain c = mapM_ (snd <$> runConfig c >>=) charts
  where
    charts = [altitudeChart, temperatureChart]

-- | Reparse all data from the raw RTTY log, discarding any failed parses.
readData :: TelemetryOptions -> IO [IO RTTYLine]
readData p = do
  dataLines <- lines <$> readFile (rawLogPath p)
  return $ unsafeSuccessExtract <$> filter isSuccess (parseString parseLine mempty <$> dataLines)
  where
    isSuccess (Success _) = True
    isSuccess _           = False
    unsafeSuccessExtract (Success s) = s
    unsafeSuccessExtract _           = error "Attempted to extract Success from Failure"

altitudeChart :: TelemetryOptions -> IO ()
altitudeChart t = do
  -- TODO: Pass parse data instead, so we don't read from the file a bunch of
  -- times.
  parses <- sequence <$> readData t
  p <- parses >>= plot'
  toFile def "charts/altitude.svg" $ do
    layout_title .= "Altitude"
    p
  where
    plot' :: [RTTYLine] -> IO (EC (Layout Int Double) ())
    plot' parses = do
      let datapoints = zip [1..] (map _altitude parses)
      return $ plot (line "meters" [datapoints])

temperatureChart :: TelemetryOptions -> IO ()
temperatureChart t = do
  -- TODO: Pass parse data instead, so we don't read from the file a bunch of
  -- times.
  parses <- sequence <$> readData t
  p <- parses >>= plot'
  toFile def "charts/temperature.svg" $ do
    layout_title .= "Temperature"
    p
  where
    cExtract (Celsius c) = c
    plot' :: [RTTYLine] -> IO (EC (Layout Int Double) ())
    plot' parses = do
      let datapoints = zip [1..] (map (cExtract . _temperature) parses)
      return $ plot (line "°C" [datapoints])
