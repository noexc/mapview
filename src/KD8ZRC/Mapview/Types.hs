{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module KD8ZRC.Mapview.Types where

import Control.Lens
import qualified Data.Configurator as Cfg
import qualified Data.Configurator.Types as Cfg
import Data.List (dropWhileEnd)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist)

data TelemetryOptions =
  TelemetryOptions { historyPath :: String
                   , rawLogPath :: String
                   , workingPath :: String
                   , minimodemFlags :: [String]
                   }

makeLenses ''TelemetryOptions

data CLIOptions =
  CLIOptions String deriving (Show)

makeLenses ''CLIOptions

parseOptions :: Parser CLIOptions
parseOptions =
  CLIOptions <$> strOption (long "conf"
                            <> short 'c'
                            <> metavar "CONFIG_FILE"
                            <> value "mapview.conf")

-- | Parse config values into a common structure ('TelemetryOptions') and
-- return it after creating any necessary directories.
runConfig :: CLIOptions -> IO (Cfg.Config, TelemetryOptions)
runConfig (CLIOptions configFile') = do
  config <- Cfg.load [Cfg.Required configFile']
  historyPath' <- (Cfg.require config "telemetry.coordinates-history") :: IO String
  createDirectoryIfMissing True (baseDir historyPath')

  rawLogPath' <- (Cfg.require config "telemetry.raw-log") :: IO String
  createDirectoryIfMissing True (baseDir rawLogPath')

  workingPath' <- (Cfg.require config "telemetry.working-coordinates") :: IO String
  createDirectoryIfMissing True (baseDir workingPath')

  flags <- (Cfg.lookupDefault ["-r", "-q", "rtty"] config "telemetry.minimodem-flags") :: IO ([String])
  let opts = TelemetryOptions historyPath' rawLogPath' workingPath' flags
  return (config, opts)

createMissingDirectories :: TelemetryOptions -> IO ()
createMissingDirectories (TelemetryOptions h r w _) = do
  mapM_ createFileIfMissing [h, r, w]
  where
    createFileIfMissing p = do
      doesExist <- doesFileExist p
      if doesExist
        then return ()
        else writeFile p ""

baseDir :: String -> String
baseDir = init . dropWhileEnd (/= '/')
