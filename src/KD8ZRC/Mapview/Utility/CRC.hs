{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module : KD8ZRC.Mapview.Utility.CRC
-- Copyright : (C) 2015 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Provides standard implementations of CRC utility functions.
----------------------------------------------------------------------------
module KD8ZRC.Mapview.Utility.CRC where

import Control.Lens
--import KD8ZRC.Mapview.Types

-- | The CRC code that we received in the telemetry message from the downlink.
newtype TelemetryCRC a = TelemetryCRC a deriving (Eq, Show)
makeLenses ''TelemetryCRC

-- | The CRC code that we calculated for the message we received in the
-- downlink.
newtype CalculatedCRC a = CalculatedCRC a deriving (Eq, Show)
makeLenses ''CalculatedCRC

-- | The result after checking a message for a valid CRC.
data CRCConfirmation a =
    CRCMatch { _crcMatchValue :: a }
  | CRCMismatch {
      _crcTelemetryValue :: TelemetryCRC a,
      _crcCalculatedValue :: CalculatedCRC a
      } deriving (Eq, Show)
makeLenses ''CalculatedCRC

-- | Implementing this class indicates that we are able to take the CRC of the
-- given type (typically an already-parsed message).
class Eq b => HasCRC a b | a -> b where
  -- | How to pull the CRC out of our parsed packet. This can either be a
  -- simple projection if the CRC being checked is either:
  --
  -- 1. Converted to the correct form by the parser, or
  -- 2. In a ready-to-validate form straight off the downlink.
  --
  -- Alternatively, this function can do any necessary conversions itself.
  telemetryCRC :: a -> TelemetryCRC b

  -- | How to calculate the CRC ourselves, given a parsed packet. Typically,
  -- this consists of constructing a string (preferably an efficient one such
  -- as a ByteString) and applying some CRC hashing algorithm on it.
  calculatedCRC :: a -> CalculatedCRC b

  -- | Ensure that a 'TelemetryCRC' and a 'CalculatedCRC' contain the same value
  -- (up to their 'Eq' instance).
  validateCRC :: a -> CRCConfirmation b
  validateCRC a =
    if x == y then CRCMatch x else CRCMismatch t c
    where
      t@(TelemetryCRC x) = telemetryCRC a
      c@(CalculatedCRC y) = calculatedCRC a
