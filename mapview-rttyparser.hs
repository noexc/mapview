{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.Aeson as A
import Data.Attoparsec.Text
import qualified Data.Configurator as Cfg
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Thyme.Clock
import Data.Thyme.Format
import Data.Thyme.Format.Aeson ()
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Text as T
import GHC.IO.Handle
import Options.Applicative hiding (Parser)
import qualified Options.Applicative (Parser)
import Shelly hiding (time)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified System.IO.Strict as S
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
    [ "latitude"  A..= lat
    , "longitude" A..= lon
    ]

instance A.FromJSON Coordinates where
  parseJSON (A.Object v) = Coordinates <$>
                             v A..: "latitude"
                         <*> v A..: "longitude"
  parseJSON _          = mzero

instance A.ToJSON RTTYLine where
  toJSON (RTTYLine _ coord alt t) =
    A.object
    [ "coordinates" A..= coord
    , "altitude"    A..= alt
    , "time"        A..= t
    ]

data CLIOptions =
  CLIOptions { configFile :: String } deriving (Show)

data RTTYPaths =
  RTTYPaths { historyPath :: String
            , rawLogPath :: String
            , workingPath :: String
            }

parseOptions :: Options.Applicative.Parser CLIOptions
parseOptions =
  CLIOptions <$> strOption (long "conf"
                            <> short 'c'
                            <> metavar "CONFIG_FILE"
                            <> value "mapview.conf")

main :: IO ()
main = execParser opts >>= runMain
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> progDesc "Read RTTY telemetry from minimodem"
     <> header "mapview-rttyparser - Read RTTY telemetry from minimodem" )

runMain :: CLIOptions -> IO ()
runMain (CLIOptions configFile') = do
  config <- Cfg.load [Cfg.Required $ configFile']
  historyPath' <- (Cfg.require config "telemetry.coordinates-history") :: IO String
  createDirectoryIfMissing True (baseDir historyPath')

  rawLogPath' <- (Cfg.require config "telemetry.raw-log") :: IO String
  createDirectoryIfMissing True (baseDir rawLogPath')

  workingPath' <- (Cfg.require config "telemetry.working-coordinates") :: IO String
  createDirectoryIfMissing True (baseDir workingPath')

  let paths = RTTYPaths historyPath' rawLogPath' workingPath'

  mapM_ createFileIfMissing [historyPath', rawLogPath', workingPath']

  readRTTY paths
  where
    baseDir = init . dropWhileEnd (/= '/')

    createFileIfMissing p = do
      doesExist <- doesFileExist p
      if doesExist
        then return ()
        else writeFile p ""

readRTTY :: RTTYPaths -> IO ()
readRTTY p = shelly $ runHandle "minimodem" ["-r", "-q", "rtty", "-S", "700", "-M", "870"] (writeJson p)

recordCoordinates :: RTTYPaths -> Coordinates -> IO ()
recordCoordinates p latest = do
  old <- S.readFile (historyPath p)
  let cList = A.decode (C8L.pack old) :: Maybe [Coordinates]
  writeFile (historyPath p) (C8L.unpack $ A.encode (latest : fromMaybe [] cList))

writeJson :: RTTYPaths -> Handle -> Sh ()
writeJson p h = do
  liftIO $ hSetBuffering h NoBuffering
  line <- liftIO $ hGetLine h
  when (not (null line)) $ do
      liftIO $ putStrLn $ "RECEIVED LINE: " ++ line
      liftIO $ appendFile (rawLogPath p) (line ++ "\n")
      case parseOnly parseLine (T.pack line) of
        Left err -> liftIO $ putStrLn $ "ERROR: " ++ err
        Right rttyLine'' -> do
          currentDay <- liftIO getCurrentTime
          rttyLine' <- liftIO rttyLine''
          let rttyLine = time . _utctDay .~ currentDay ^. _utctDay $ rttyLine'
          liftIO $ putStrLn $ "...which parsed into: " ++ show rttyLine
          liftIO $ writeFile (workingPath p) (C8L.unpack $ A.encode rttyLine)
          liftIO $ recordCoordinates p (rttyLine ^. coordinates)
  writeJson p h

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
