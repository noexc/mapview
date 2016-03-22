module Main where

import Control.Lens
import Control.Monad
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Options.Applicative hiding (Failure, Parser, Success)
import qualified Options.Applicative (Parser)
import Text.Trifecta hiding (line)

import KD8ZRC.Mapview.Parser
import KD8ZRC.Mapview.Types

data ChartOptions = ChartOptions { wiki :: Bool } deriving (Show)

data FullOptions = FullOptions CLIOptions ChartOptions

chartOptions :: Options.Applicative.Parser ChartOptions
chartOptions = ChartOptions <$> switch ( short 'w'
                                      <> long "wiki-markup"
                                      <> help "Render wiki gallery markup" )

main :: IO ()
main = execParser opts >>= runMain
  where
    opts = info (helper <*> (FullOptions <$> parseOptions <*> chartOptions))
      ( fullDesc
     <> progDesc "Process the raw telemetry log and generate charts from it"
     <> header "mapview-generate-charts - Generate charts from raw telemetry")

runMain :: FullOptions -> IO ()
runMain opts@(FullOptions cli chartOpts) = do
  telemOpts <- snd <$> runConfig cli
  telemetry <- sequence <$> readData telemOpts
  when (wiki chartOpts) $ putStrLn "<gallery>"
  mapM_ (\x -> x opts telemetry) charts
  when (wiki chartOpts) $ putStrLn "</gallery>"
  where
    charts = [altitudeChart]

-- | Reparse all data from the raw telemetry log, discarding any failed parses.
readData :: ConfigFileOptions -> IO [IO TelemetryLine]
readData tel = do
  dataLines <- lines <$> readFile (rawLogPath tel)
  return $ unsafeSuccessExtract <$> filter isSuccess (parseString parseLine mempty <$> dataLines)
  where
    isSuccess (Success _) = True
    isSuccess _           = False
    unsafeSuccessExtract (Success s) = s
    unsafeSuccessExtract _           = error "Attempted to extract Success from Failure"

altitudeChart :: FullOptions -> IO [TelemetryLine] -> IO ()
altitudeChart (FullOptions _ _) parses = do
  p <- parses >>= plot'
  putStrLn "File:Altitude.svg|Altitude data"
  toFile def "charts/altitude.svg" $ do
    layout_title .= "Altitude"
    p
  where
    plot' :: [TelemetryLine] -> IO (EC (Layout Int Double) ())
    plot' parses' = do
      let datapoints = zip [1..] (map _altitude parses')
      return $ plot (line "meters" [datapoints])

--temperatureChart :: FullOptions -> IO [TelemetryLine] -> IO ()
--temperatureChart (FullOptions _ _) parses = do
--  p <- parses >>= plot'
--  putStrLn "File:Temperature.svg|Temperature data (°C)"
--  toFile def "charts/temperature.svg" $ do
--    layout_title .= "Temperature"
--    p
--  where
--    cExtract (Celsius c) = c
--    plot' :: [TelemetryLine] -> IO (EC (Layout Int Double) ())
--    plot' parses' = do
--      let datapoints = zip [1..] (map (cExtract . _temperature) parses')
--      return $ plot (line "°C" [datapoints])
