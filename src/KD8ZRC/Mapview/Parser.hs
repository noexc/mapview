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
import Linear.V3
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

  magX <- integer
  _ <- colon

  magY <- integer
  _ <- colon

  magZ <- integer
  _ <- colon

  celsius <- eitherToNum <$> integerOrDouble
  _ <- colon

  crc16T <- char '0' *> oneOf "xX" *> number 16 hexDigit
  _ <- colon

  crc16C <- crcHaskell . dropWhileEnd (/= ':') . init . BRC.unpack <$> line

  return $ return $ TelemetryLine
    (T.pack callsign')
    (Coordinates lat' lon')
    altitude'
    (readTime defaultTimeLocale "%H%M%S" time')
    (MagField (V3 magX magY magZ))
    (Celsius celsius)
    (mkCRCConfirmation (TelemetryCRC crc16T) crc16C)

eitherToNum :: (Num b, Integral a) => Either a b -> b
eitherToNum = either fromIntegral id
