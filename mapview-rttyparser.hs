{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as C8L
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import qualified Data.Text as T
import Data.Thyme.Clock
import GHC.IO.Handle
import Options.Applicative hiding (Failure, Parser, Success)
import Shelly hiding (time)
import qualified System.IO.Strict as S
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import Text.Trifecta

import KD8ZRC.Mapview.Parser
import KD8ZRC.Mapview.Types

main :: IO ()
main = execParser opts >>= runMain
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> progDesc "Read RTTY telemetry from minimodem"
     <> header "mapview-rttyparser - Read RTTY telemetry from minimodem" )

runMain :: CLIOptions -> IO ()
runMain c = snd <$> (runConfig c) >>= readRTTY

readRTTY :: TelemetryOptions -> IO ()
readRTTY p = shelly $ runHandle "minimodem" (map T.pack $ minimodemFlags p) (writeJson p)

recordCoordinates :: TelemetryOptions -> Coordinates -> IO ()
recordCoordinates p latest = do
  old <- S.readFile (historyPath p)
  let cList = A.decode (C8L.pack old) :: Maybe [Coordinates]
  writeFile (historyPath p) (C8L.unpack $ A.encode (latest : fromMaybe [] cList))

writeJson :: TelemetryOptions -> Handle -> Sh ()
writeJson p h = do
  liftIO $ hSetBuffering h NoBuffering
  line' <- liftIO $ hGetLine h
  when (not (null line')) $ do
      liftIO $ putStrLn $ "RECEIVED LINE: " ++ line'
      liftIO $ appendFile (rawLogPath p) (line' ++ "\n")
      case parseString parseLine mempty line' of
        Failure e -> liftIO $ putDoc e
        Success rttyLine'' -> do
          currentDay <- liftIO getCurrentTime
          rttyLine' <- liftIO rttyLine''
          let rttyLine = time . _utctDay .~ currentDay ^. _utctDay $ rttyLine'
          liftIO $ putStrLn $ "...which parsed into: " ++ show rttyLine
          liftIO $ writeFile (workingPath p) (C8L.unpack $ A.encode rttyLine)
          liftIO $ recordCoordinates p (rttyLine ^. coordinates)
  writeJson p h
