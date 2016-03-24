module KD8ZRC.Mapview.Parser where

import Control.Applicative
import qualified Data.ByteString as BR
import qualified Data.ByteString.Char8 as BRC
import Data.Char (digitToInt)
import Data.Digest.CRC16
import Data.List (dropWhileEnd, foldl')
import qualified Data.Text as T
import Data.Thyme.Format
import Data.Word
import System.Locale
import Text.Trifecta

import KD8ZRC.Mapview.Types

number :: TokenParsing m => Integer -> m Char -> m Integer
number base baseDigit =
  foldl' (\x d -> base * x + toInteger (digitToInt d)) 0 <$> some baseDigit

crcHaskellF :: Word16 -> Bool -> Word16 -> [Word8] -> Word16
crcHaskellF poly inverse initial = BR.foldl (crc16Update poly inverse) initial . BR.pack

-- TODO: Config-file polynomial and initial.
crcHaskell :: String -> CalculatedCRC
crcHaskell s =
  CalculatedCRC . fromIntegral $ crcHaskellF
    0x1021
    False
    0xffff
    [fromIntegral (fromEnum x) :: Word8 | x <- s]

-- | Input is in the following format:
--
-- >>> :W8UPD:41.09347:-80.74683:237.0:032553:656:-40:83:
parseLine :: (Monad m, DeltaParsing m, TokenParsing m) => m (IO TelemetryLine)
parseLine = do
  _ <- colon
  callsign' <- manyTill anyChar (try colon)

  lat' <- eitherToNum <$> integerOrDouble
  _ <- colon

  lon' <- eitherToNum <$> integerOrDouble
  _ <- colon

  altitude' <- eitherToNum <$> integerOrDouble
  _ <- colon

  time' <- many (token digit)
  _ <- colon

  voltage' <- eitherToNum <$> integerOrDouble
  _ <- colon

  crc16T <- number 16 hexDigit
  _ <- colon

  crc16C <- crcHaskell . dropWhileEnd (/= ':') . init . tail . BRC.unpack <$> line

  return $ return $ TelemetryLine
    (T.pack callsign')
    (Coordinates lat' lon')
    altitude'
    (readTime defaultTimeLocale "%H%M%S" time')
    voltage'
    (mkCRCConfirmation (TelemetryCRC crc16T) crc16C)

eitherToNum :: (Num b, Integral a) => Either a b -> b
eitherToNum = either fromIntegral id
