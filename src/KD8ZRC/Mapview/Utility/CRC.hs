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

validateCRC :: Eq a => TelemetryCRC a -> CalculatedCRC a -> CRCConfirmation a
validateCRC t@(TelemetryCRC x) c@(CalculatedCRC y) =
  if x == y then CRCMatch x else CRCMismatch t c

crcIsMatch :: CRCConfirmation a -> Bool
crcIsMatch (CRCMatch _) = True
crcIsMatch _ = False
