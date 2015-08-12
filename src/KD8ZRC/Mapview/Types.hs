{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module : KD8ZRC.Mapview.Types
-- Copyright : (C) 2015 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Provides fundamental types intrinsic to the rest of the mapview library.
-- This shouldn't contain types specific to any particular use of the library
-- (suggested types can be found in the KD8ZRC.Mapview.Utility.* set of
-- modules).
----------------------------------------------------------------------------
module KD8ZRC.Mapview.Types where

import Control.Lens
--import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
--import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Configurator as Cfg
import qualified Data.Configurator.Types as Cfg
import Data.List (dropWhileEnd)
import Data.Thyme.Format.Aeson ()
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import qualified Text.Trifecta as Tr

-- | The 'MV' type is a monad transformer which carries around our
-- 'MapviewConfig', making it accessible to all callback hooks.
type MV t a = ReaderT (MapviewConfig t) IO a

-- | The type for callbacks which occur immediately after a packet is received,
-- before even parsing occurs.
newtype PacketLineCallback t =
  PacketLineCallback { getPacketLineCallback :: String -> MV t () }

-- | The configuration for this instance of Mapview. The @t@ parameter is the
-- type that the telemetry parser parses into, if it is successfully able to
-- parse the data.
data MapviewConfig t =
  MapviewConfig { _mvParser :: forall m. (Monad m, Tr.DeltaParsing m) => m t
                  -- ^ Determines how a packet is parsed.
                , _mvPacketLineCallback :: [PacketLineCallback t]
                  -- ^ Determines what to do immediately after a packet is
                  -- received, before it is parsed.
                , _mvDownlinkSpawn :: MV t ()
                  -- ^ As soon as Mapview is launched, this callback is run and
                  -- should begin the process of listening for telemetry data.
                }
makeLenses ''MapviewConfig






-- Old stuff, kept here to keep the build working - will gradually be nuked as
-- the v3 rework takes place. See #20.

data ConfigFileOptions =
  ConfigFileOptions { historyPath     :: String
                    , rawLogPath      :: String
                    , workingPath     :: String
                    , modem           :: String
                    , modemFlags      :: [String]
                    , gpsdCoordinates :: String
                    } deriving (Eq, Show)
makeLenses ''ConfigFileOptions

newtype CLIOptions =
  CLIOptions String deriving (Eq, Show)
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

  gpsdPath' <- Cfg.require config "gpsd.coordinates"
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
