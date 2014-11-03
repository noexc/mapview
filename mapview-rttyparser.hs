{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.Aeson as A
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Thyme.Clock
import Data.Thyme.Format
import Data.Thyme.Format.Aeson ()
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Text as T
import GHC.IO.Handle
import Options.Applicative hiding (Failure, Parser, Success)
import Shelly hiding (time)
import qualified System.IO.Strict as S
import System.Locale
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import Text.Trifecta

import KD8ZRC.Mapview.Types

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
    } deriving Show

data RTTYLine = RTTYLine {
    _callsign   :: T.Text
  , _coordinates :: Coordinates
  , _altitude   :: Meters
  , _time       :: UTCTime
  } deriving Show

makeLenses ''RTTYLine

-- TODO: lens-aeson?
instance A.ToJSON Coordinates where
  toJSON (Coordinates lat lon) =
    A.object
    [ "lat"  A..= lat
    , "lon" A..= lon
    ]

instance A.FromJSON Coordinates where
  parseJSON (A.Object v) = Coordinates <$>
                             v A..: "lat"
                         <*> v A..: "lon"
  parseJSON _          = mzero

instance A.ToJSON RTTYLine where
  toJSON (RTTYLine _ coord alt t) =
    A.object
    [ "coordinates" A..= coord
    , "altitude"    A..= alt
    , "time"        A..= t
    ]

main :: IO ()
main = execParser opts >>= runMain
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> progDesc "Read RTTY telemetry from minimodem"
     <> header "mapview-rttyparser - Read RTTY telemetry from minimodem" )

runMain :: CLIOptions -> IO ()
runMain c = snd <$> (runConfig c) >>= readRTTY

readRTTY :: TelemetryOptions -> IO ()
readRTTY p = shelly $ runHandle "minimodem" (map T.pack $ minimodemFlags p) (writeJson p)

recordCoordinates :: TelemetryOptions -> Coordinates -> IO ()
recordCoordinates p latest = do
  old <- S.readFile (historyPath p)
  let cList = A.decode (C8L.pack old) :: Maybe [Coordinates]
  writeFile (historyPath p) (C8L.unpack $ A.encode (latest : fromMaybe [] cList))

writeJson :: TelemetryOptions -> Handle -> Sh ()
writeJson p h = do
  liftIO $ hSetBuffering h NoBuffering
  line' <- liftIO $ hGetLine h
  when (not (null line')) $ do
      liftIO $ putStrLn $ "RECEIVED LINE: " ++ line'
      liftIO $ appendFile (rawLogPath p) (line' ++ "\n")
      case parseString parseLine mempty line' of
        Failure e -> liftIO $ putDoc e
        Success rttyLine'' -> do
          currentDay <- liftIO getCurrentTime
          rttyLine' <- liftIO rttyLine''
          let rttyLine = time . _utctDay .~ currentDay ^. _utctDay $ rttyLine'
          liftIO $ putStrLn $ "...which parsed into: " ++ show rttyLine
          liftIO $ writeFile (workingPath p) (C8L.unpack $ A.encode rttyLine)
          liftIO $ recordCoordinates p (rttyLine ^. coordinates)
  writeJson p h

parseLine :: Parser (IO RTTYLine)
parseLine = do
  _ <- colon
  callsign' <- manyTill anyChar (try colon)

  -- This is okay to do here. Any pattern match fail will get caught by
  -- Trifecta and handled nicely.
  Right lat' <- integerOrDouble
  _ <- colon

  -- And again.
  Right lon' <- integerOrDouble
  _ <- colon

  altitude' <- double
  _ <- colon
  time' <- many (token digit)
  return $ return $ RTTYLine (T.pack callsign') (Coordinates lat' lon') altitude' (readTime defaultTimeLocale "%H%M%S" time')
