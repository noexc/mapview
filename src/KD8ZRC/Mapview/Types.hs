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
import Text.PrettyPrint.ANSI.Leijen
import qualified Text.Trifecta as Tr

-- | The 'MV' type is a monad transformer which carries around our
-- 'MapviewConfig', making it accessible to all callback hooks.
type MV t a = ReaderT (MapviewConfig t) IO a

-- | The type for callbacks which occur immediately after a packet is received,
-- before even parsing occurs.
newtype PacketLineCallback t =
  PacketLineCallback { getPacketLineCallback :: BS.ByteString -> MV t () }

-- | The type for callbacks which occur immediately after a packet is parsed.
data ParsedPacketCallback t =
    ParseSuccessCallback { getParseSuccessCallback :: t -> MV t () }
  | ParseFailureCallback { getParseFailureCallback :: Doc -> MV t () }

-- | The configuration for this instance of Mapview. The @t@ parameter is the
-- type that the telemetry parser parses into, if it is successfully able to
-- parse the data.
data MapviewConfig t =
  MapviewConfig { _mvParser :: forall m. (Monad m, Tr.DeltaParsing m, Tr.Errable m) => m t
                  -- ^ Determines how a packet is parsed.
                , _mvPacketLineCallback :: [PacketLineCallback t]
                  -- ^ Determines what to do immediately after a packet is
                  -- received, before it is parsed.
                , _mvDownlinkSpawn :: MV t ()
                  -- ^ As soon as Mapview is launched, this callback is run and
                  -- should begin the process of listening for telemetry data.
                , _mvParsedPacketCallback :: [ParsedPacketCallback t]
                  -- ^ These callbacks get called depending on whether or not a
                  -- parse was successful.
                }
makeLenses ''MapviewConfig
