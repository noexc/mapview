{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens
import qualified Data.ByteString as BR
import qualified Data.ByteString.Char8 as BRC
import Data.Char (digitToInt)
import Data.Digest.CRC16
import Data.List (dropWhileEnd, foldl')
import qualified Data.Text as T
import Data.Thyme.Clock
import Data.Thyme.Format
import Data.Word
import KD8ZRC.Mapview.Types
import Text.Trifecta
import System.Locale

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

data LookangleCoordinates =
  LookangleCoordinates Coordinates Meters deriving (Eq, Show)

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

isCRCMatch :: CRCConfirmation -> Bool
isCRCMatch (CRCMatch _) = True
isCRCMatch _ = False

data TelemetryLine = TelemetryLine {
    _callsign    :: T.Text
  , _coordinates :: Coordinates
  , _altitude    :: Meters
  , _time        :: UTCTime
  , _crc         :: CRCConfirmation
  } deriving (Eq)
makeLenses ''TelemetryLine

crcHaskellF :: Word16 -> Bool -> Word16 -> [Word8] -> Word16
crcHaskellF poly inverse initial = BR.foldl (crc16Update poly inverse) initial . BR.pack

crcHaskell :: String -> CalculatedCRC
crcHaskell s =
  CalculatedCRC . fromIntegral $ crcHaskellF
    0x1021
    False
    0xffff
    [fromIntegral (fromEnum x) :: Word8 | x <- s]


parser :: (Monad m, DeltaParsing m) => m TelemetryLine
parser = do
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
  crc16T <- number 16 hexDigit
  _ <- colon
  crc16C <- crcHaskell . dropWhileEnd (/= ':') . init . tail . BRC.unpack <$> line
  return $ TelemetryLine
    (T.pack callsign')
    (Coordinates lat' lon')
    altitude'
    (readTime defaultTimeLocale "%H%M%S" time')
    (mkCRCConfirmation (TelemetryCRC crc16T) crc16C)
  where
    number base baseDigit =
      foldl' (\x d -> base * x + toInteger (digitToInt d)) 0 <$> some baseDigit

eitherToNum :: (Num b, Integral a) => Either a b -> b
eitherToNum = either fromIntegral id

mvConfig :: MapviewConfig TelemetryLine
mvConfig = MapviewConfig {
    mvParser = parser
}

main :: IO ()
main = error ""
