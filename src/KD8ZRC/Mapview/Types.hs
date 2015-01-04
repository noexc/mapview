{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module KD8ZRC.Mapview.Types where

import Control.Lens
import Control.Monad (mzero, unless)
import qualified Data.Aeson as A
import qualified Data.Configurator as Cfg
import qualified Data.Configurator.Types as Cfg
import Data.List (dropWhileEnd)
import qualified Data.Text as T
import Data.Thyme.Clock
import Data.Thyme.Format.Aeson ()
import Linear.V3
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist)

type Latitude  = Double
type Longitude = Double
type Meters    = Double

data Coordinates = Coordinates {
    _latitude  :: Latitude
  , _longitude :: Longitude
  } deriving Show
makeLenses ''Coordinates

data CoordinatesList = CoordinatesList {
    coordinatesList :: [Coordinates]
    } deriving Show

newtype MagField = MagField { _values :: V3 Integer } deriving (Show)
makeLenses ''MagField

newtype Celsius = Celsius { _degrees :: Double } deriving (Show)
makeLenses ''Celsius

data TelemetryLine = TelemetryLine {
    _callsign   :: T.Text
  , _coordinates :: Coordinates
  , _altitude   :: Meters
  , _time       :: UTCTime
  , _magnetic   :: MagField
  , _temperature :: Celsius
  } deriving Show
makeLenses ''TelemetryLine

-- TODO: lens-aeson?
instance A.ToJSON Coordinates where
  toJSON (Coordinates lat lon) =
    A.object
    [ "lat" A..= lat
    , "lon" A..= lon
    ]

instance A.ToJSON MagField where
  toJSON (MagField (V3 x y z)) =
    A.object
    [ "x" A..= x
    , "y" A..= y
    , "z" A..= z
    ]

instance A.FromJSON Coordinates where
  parseJSON (A.Object v) = Coordinates <$>
                             v A..: "lat"
                         <*> v A..: "lon"
  parseJSON _            = mzero

instance A.ToJSON TelemetryLine where
  toJSON (TelemetryLine _ coord alt t mag (Celsius c)) =
    A.object
    [ "coordinates"    A..= coord
    , "altitude"       A..= alt
    , "time"           A..= t
    , "magnetic_field" A..= mag
    , "temperature"    A..= c
    ]

data ConfigFileOptions =
  ConfigFileOptions { historyPath :: String
                   , rawLogPath  :: String
                   , workingPath :: String
                   , modem       :: String
                   , modemFlags  :: [String]
                   }

makeLenses ''ConfigFileOptions

data CLIOptions =
  CLIOptions String deriving (Show)

makeLenses ''CLIOptions

parseOptions :: Parser CLIOptions
parseOptions =
  CLIOptions <$> strOption (long "conf"
                            <> short 'c'
                            <> metavar "CONFIG_FILE"
                            <> value "mapview.conf")

-- | Parse config values into a common structure ('ConfigFileOptions') and
-- return it after creating any necessary directories.
runConfig :: CLIOptions -> IO (Cfg.Config, ConfigFileOptions)
runConfig (CLIOptions configFile') = do
  config <- Cfg.load [Cfg.Required configFile']
  historyPath' <- Cfg.require config "telemetry.coordinates-history"
  createDirectoryIfMissing True (baseDir historyPath')

  rawLogPath' <- Cfg.require config "telemetry.raw-log"
  createDirectoryIfMissing True (baseDir rawLogPath')

  workingPath' <- Cfg.require config "telemetry.working-coordinates"
  createDirectoryIfMissing True (baseDir workingPath')

  modemCommand <- Cfg.lookupDefault "minimodem" config "telemetry.modem-command"
  flags <- Cfg.lookupDefault ["-r", "-q", "rtty"] config "telemetry.modem-flags"
  let opts = ConfigFileOptions
             historyPath'
             rawLogPath'
             workingPath'
             modemCommand
             flags
  return (config, opts)

createMissingDirectories :: ConfigFileOptions -> IO ()
createMissingDirectories (ConfigFileOptions h r w _ _) =
  mapM_ createFileIfMissing [h, r, w]
  where
    createFileIfMissing p = do
      doesExist <- doesFileExist p
      unless doesExist $ writeFile p ""

baseDir :: String -> String
baseDir = init . dropWhileEnd (/= '/')
