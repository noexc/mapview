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
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Text.PrettyPrint.ANSI.Leijen
import qualified Text.Trifecta as Tr

-- | The 'MV' type is a monad transformer which carries around our
-- 'MapviewConfig', making it accessible to all callback hooks.
type MV t a = ReaderT (MapviewConfig t) IO a

-- | The type for callbacks which occur immediately after a packet is received,
-- before even parsing occurs.
newtype TelemetryReceivedCallback s t =
  TelemetryReceivedCallback {
    getTelemetryReceivedCallback :: s -> MV t ()
  }

-- | The type for callbacks which occur immediately after a packet is parsed.
data ParsedPacketCallback t =
    ParseSuccessCallback { getParseSuccessCallback :: t -> MV t () }
  | ParseFailureCallback { getParseFailureCallback :: Doc -> MV t () }

data ModemStdoutConfiguration t = ModemStdoutConfiguration {
    _command :: T.Text
    -- ^ The modem command to run (e.g. @minimodem@ or @fldigi-shell@)
  , _args :: [T.Text]
    -- ^ Flags/arguments to pass to the modem command
  , _parsedCallbacks :: [ParsedPacketCallback t]
    -- ^  Callbacks to run after a parsing attempt
  , _parser :: forall m. (Tr.DeltaParsing m, Tr.Errable m) => m t
    -- ^ How to parse
  , _onRaw :: [TelemetryReceivedCallback BS.ByteString t]
    -- ^ What to do on as /soon/ as we get a telemetry line (before parsing)
  }
-- This breaks atm:

-- | The configuration for this instance of Mapview. The @t@ parameter is the
-- type that the telemetry parser parses into, if it is successfully able to
-- parse the data.
data MapviewConfig t =
  MapviewConfig { _telemetry :: MV t ()
                  -- ^ Determines how to listen for packets/telemetry. Called as
                  -- soon as MapView is launched, and should be long-running.
                }

makeLenses ''MapviewConfig
makeLenses ''ModemStdoutConfiguration
