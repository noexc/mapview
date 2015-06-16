{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as C8L
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Text as T
import Data.Thyme.Clock
import GHC.IO.Handle
import Options.Applicative hiding (Failure, Parser, Success)
import Shelly hiding (time)
import qualified System.IO.Strict as S
import Text.Trifecta
import Prelude

import KD8ZRC.Mapview.Parser
import KD8ZRC.Mapview.Types

main :: IO ()
main = execParser opts >>= runMain
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> progDesc "Read telemetry data from a command-line modem"
     <> header "mapview-telemetryparser - Read telemetry data from a modem" )

runMain :: CLIOptions -> IO ()
runMain c = snd <$> runConfig c >>= readTelemetry

readTelemetry :: ConfigFileOptions -> IO ()
readTelemetry p = shelly $ runHandle (fromText . T.pack $ modem p) (map T.pack $ modemFlags p) (writeJson p)

recordCoordinates :: ConfigFileOptions -> Coordinates -> IO ()
recordCoordinates p latest = do
  old <- S.readFile (historyPath p)
  let cList = A.decode (C8L.pack old) :: Maybe [Coordinates]
  writeFile (historyPath p) (C8L.unpack $ A.encode (latest : fromMaybe [] cList))

writeJson :: ConfigFileOptions -> Handle -> Sh ()
writeJson p h = do
  liftIO $ hSetBuffering h NoBuffering
  line' <- liftIO $ hGetLine h
  unless (null line') $ do
      liftIO $ do
        putStrLn $ "RECEIVED LINE: " ++ line'
        appendFile (rawLogPath p) (line' ++ "\n")
      case parseString parseLine mempty line' of
        Failure _ -> return ()
        Success telemetryLine'' -> do
          currentDay <- liftIO getCurrentTime
          telemetryLine' <- liftIO telemetryLine''
          let telemetryLine = time . _utctDay .~ currentDay ^. _utctDay $ telemetryLine'
          liftIO $ do
            putStrLn "...which parsed into:"
            putStrLn $ "------------------------------------------------"
            print telemetryLine
            putStrLn $ "------------------------------------------------"
            writeFile (workingPath p) (C8L.unpack $ A.encode telemetryLine)
            when (telemetryLine ^. crc . to isCRCMatch) $
              recordCoordinates p (telemetryLine ^. coordinates)
  writeJson p h
