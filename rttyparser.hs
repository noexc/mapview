{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.Aeson as A
import Data.Attoparsec.Text
import Data.Thyme.Clock
import Data.Thyme.Format
import Data.Thyme.Format.Aeson ()
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Text as T
import GHC.IO.Handle
import Shelly hiding (time)
import System.Directory (createDirectoryIfMissing)
import System.Locale

type Latitude  = Double
type Longitude = Double
type Meters    = Double

data Coordinates = Coordinates {
    _latitude  :: Latitude
  , _longitude :: Longitude
  } deriving Show

makeLenses ''Coordinates

data CoordinatesList = CoordinatesList {
    coordinatesList :: [Coordinates]
    }

data RTTYLine = RTTYLine {
    _callsign   :: T.Text
  , coordinates :: Coordinates
  , _altitude   :: Meters
  , _time       :: UTCTime
  } deriving Show

makeLenses ''RTTYLine

-- TODO: lens-aeson?
instance A.ToJSON Coordinates where
  toJSON (Coordinates lat lon) =
    A.object
    [ "latitude"  A..= lat
    , "longitude" A..= lon
    ]

instance A.ToJSON CoordinatesList where
  toJSON (CoordinatesList c) =
    A.object
    [ "coordinates"  A..= c
    ]

instance A.FromJSON CoordinatesList where
  parseJSON (A.Object v) = CoordinatesList <$>
                               v A..: "coordinates"
  parseJSON _            = mzero

instance A.FromJSON Coordinates where
  parseJSON (A.Object v) = Coordinates <$>
                               v A..: "latitude"
                           <*> v A..: "longitude"
  parseJSON _            = mzero

instance A.ToJSON RTTYLine where
  toJSON (RTTYLine c coord alt t) =
    A.object
    [ "callsign"    A..= c
    , "coordinates" A..= coord
    , "altitude"    A..= alt
    , "time"        A..= t
    ]

main :: IO ()
main = do
  createDirectoryIfMissing True "/tmp/w8upd"
  createDirectoryIfMissing True "/var/tmp/w8upd"
  writeFile "/tmp/w8upd/rtty-coordinates.json" ""
  writeFile "/var/tmp/w8upd/rttylog" ""
  writeFile "/var/tmp/w8upd/coordinates-log.json" ""
  readRTTY

readRTTY :: IO ()
readRTTY = shellyNoDir $ runHandle "minimodem" ["-r", "-q", "rtty", "-S", "700", "-M", "870"] writeJson

recordCoordinates :: Coordinates -> IO ()
recordCoordinates latest = do
  cList <- fmap A.decode (C8L.readFile "/var/tmp/w8upd/coordinates-log.json") :: IO (Maybe CoordinatesList)
  writeFile "/var/tmp/w8upd/coordinates-log.json" (C8L.unpack $ A.encode (latest : maybe [] coordinatesList cList))

writeJson :: Handle -> Sh ()
writeJson h = do
  liftIO $ hSetBuffering h NoBuffering
  line <- liftIO $ hGetLine h
  when (not (null line) && (line /= "RRRRR")) $ do
      liftIO $ putStrLn $ "RECEIVED LINE: " ++ line
      liftIO $ appendFile "/var/tmp/w8upd/rttylog" (line ++ "\n")
      case parseOnly parseLine (T.pack line) of
        Left err -> liftIO $ putStrLn $ "ERROR: " ++ err
        Right rttyLine'' -> do
          currentDay <- liftIO getCurrentTime
          rttyLine' <- liftIO rttyLine''
          let rttyLine = time . _utctDay .~ currentDay ^. _utctDay $ rttyLine'
          liftIO $ putStrLn $ "...which parsed into: " ++ show rttyLine
          liftIO $ writeFile "/tmp/w8upd/rtty-coordinates.json" (C8L.unpack $ A.encode rttyLine)
  writeJson h

parseLine :: Parser (IO RTTYLine)
parseLine = do
  _ <- char ':'
  callsign' <- takeWhile1 (/= ':')
  _ <- char ':'
  latitude' <- takeWhile1 (/= ':')
  _ <- char ':'
  longitude' <- takeWhile1 (/= ':')
  _ <- char ':'
  altitude' <- takeWhile1 (/= ':')
  _ <- char ':'
  time' <- takeWhile1 (/= ':')
  return (
    (return $ RTTYLine
     callsign'
     (Coordinates
      (read $ T.unpack latitude'  :: Latitude)
      (read $ T.unpack longitude' :: Longitude))
     (read $ T.unpack altitude'  :: Meters)
     (readTime defaultTimeLocale "%H%M%S" (T.unpack time'))) :: IO RTTYLine)
