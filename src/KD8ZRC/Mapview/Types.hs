{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module KD8ZRC.Mapview.Types where

import Control.Lens
import Control.Monad (mzero)
import qualified Data.Aeson as A
import qualified Data.Configurator as Cfg
import qualified Data.Configurator.Types as Cfg
import Data.List (dropWhileEnd)
import qualified Data.Text as T
import Data.Thyme.Clock
import Data.Thyme.Format.Aeson ()
import Options.Applicative
import System.Directory (createDirectoryIfMissing)

type Latitude  = Double
type Longitude = Double
type Meters    = Double

data Coordinates = Coordinates {
    _latitude  :: Latitude
  , _longitude :: Longitude
  } deriving (Eq, Show)
makeLenses ''Coordinates

data CoordinatesList = CoordinatesList {
    coordinatesList :: [Coordinates]
    } deriving (Eq, Show)

newtype Celsius = Celsius { _degrees :: Double } deriving (Eq, Show)
makeLenses ''Celsius

newtype TelemetryCRC = TelemetryCRC Integer deriving (Eq, Show)
newtype CalculatedCRC = CalculatedCRC Integer deriving (Eq, Show)
data CRCConfirmation = CRCMatch Integer | CRCMismatch TelemetryCRC CalculatedCRC deriving (Eq, Show)

mkCRCConfirmation :: TelemetryCRC -> CalculatedCRC -> CRCConfirmation
mkCRCConfirmation t@(TelemetryCRC t') c@(CalculatedCRC c') =
  if t' == c'
  then CRCMatch t'
  else CRCMismatch t c

data TelemetryLine = TelemetryLine {
    _callsign    :: T.Text
  , _coordinates :: Coordinates
  , _altitude    :: Meters
  , _time        :: UTCTime
  , _temperature :: Celsius
  , _crc         :: CRCConfirmation
  } deriving (Eq, Show)
makeLenses ''TelemetryLine

-- TODO: lens-aeson?
instance A.ToJSON Coordinates where
  toJSON (Coordinates lat lon) =
    A.object
    [ "lat" A..= lat
    , "lon" A..= lon
    ]

instance A.ToJSON CRCConfirmation where
  toJSON (CRCMatch n) =
    A.object
    [ "match" A..= True
    , "crc"   A..= n
    ]
  toJSON (CRCMismatch (TelemetryCRC tc) (CalculatedCRC cc)) =
    A.object
    [ "match"    A..= False
    , "received" A..= tc
    , "expected" A..= cc
    ]

instance A.FromJSON Coordinates where
  parseJSON (A.Object v) = Coordinates <$>
                             v A..: "lat"
                         <*> v A..: "lon"
  parseJSON _            = mzero

instance A.ToJSON TelemetryLine where
  toJSON (TelemetryLine _ coord alt t (Celsius c) crc') =
    A.object
    [ "coordinates"    A..= coord
    , "altitude"       A..= alt
    , "time"           A..= t
    , "temperature"    A..= c
    , "crc"            A..= crc'
    ]

data ConfigFileOptions =
  ConfigFileOptions { historyPath :: String
                    , rawLogPath  :: String
                    , workingPath :: String
                    , modem       :: String
                    , modemFlags  :: [String]
                    , gpsdHistory :: String
                    } deriving (Eq, Show)

data CLIOptions =
  CLIOptions String deriving (Eq, Show)

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

  gpsdPath' <- Cfg.require config "gpsd.coordinates-history"
  createDirectoryIfMissing True (baseDir gpsdPath')

  let opts = ConfigFileOptions
             historyPath'
             rawLogPath'
             workingPath'
             modemCommand
             flags
             gpsdPath'

  return (config, opts)

  where
    baseDir = init . dropWhileEnd (/= '/')
