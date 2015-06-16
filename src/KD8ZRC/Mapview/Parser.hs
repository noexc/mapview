{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module KD8ZRC.Mapview.Parser where

import Control.Lens
import Control.Monad (mzero)
import qualified Data.Aeson as A
import qualified Data.ByteString as BR
import qualified Data.ByteString.Char8 as BRC
import Data.Char (digitToInt)
import Data.Digest.CRC16
import Data.List (dropWhileEnd)
import Data.List (foldl')
import qualified Data.Text as T
import Data.Thyme.Clock
import Data.Thyme.Format
import Data.Thyme.Format.Aeson ()
import Data.Word
import Options.Applicative
import System.Locale
import Text.Trifecta

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

instance Show TelemetryLine where
  show (TelemetryLine call (Coordinates lat' lon') alt tm crc') =
    "call:\t\t" ++ T.unpack call ++ "\n" ++
    "lat/lon:\t" ++ show lat' ++ ", " ++ show lon' ++ "\n" ++
    "alt:\t\t" ++ show alt ++ "m\n" ++
    "time:\t\t" ++ show tm ++ "\n" ++
    "crc:\t\t" ++ show crc'

-- TODO: lens-aeson?
instance A.ToJSON Coordinates where
  toJSON (Coordinates lat lon) =
    A.object
    [ "lat" A..= lat
    , "lon" A..= lon
    ]

instance A.ToJSON LookangleCoordinates where
  toJSON (LookangleCoordinates (Coordinates lat lon) alt) =
    A.object
    [ "lat" A..= lat
    , "lon" A..= lon
    , "alt" A..= alt
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
  toJSON (TelemetryLine _ coord alt t crc') =
    A.object
    [ "coordinates"    A..= coord
    , "altitude"       A..= alt
    , "time"           A..= t
    , "crc"            A..= crc'
    ]



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

  crc16T <- number 16 hexDigit
  _ <- colon

  crc16C <- crcHaskell . dropWhileEnd (/= ':') . init . tail . BRC.unpack <$> line

  return $ return $ TelemetryLine
    (T.pack callsign')
    (Coordinates lat' lon')
    altitude'
    (readTime defaultTimeLocale "%H%M%S" time')
    (mkCRCConfirmation (TelemetryCRC crc16T) crc16C)

eitherToNum :: (Num b, Integral a) => Either a b -> b
eitherToNum = either fromIntegral id
