{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Attoparsec.Text
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Text as T
import GHC.IO.Handle
import Shelly

type Latitude  = Double
type Longitude = Double
type Meters    = Double

data RTTYLine = RTTYLine {
    callsign :: T.Text
  , longitude :: Longitude
  , latitude :: Latitude
  , altitude :: Meters
    -- Time is also in the line, but we can use the system time instead.
  } deriving Show

instance ToJSON RTTYLine where
  toJSON (RTTYLine c lon lat alt) =
    object ["callsign" .= c, "longitude" .= lon, "latitude" .= lat, "altitude" .= alt]

main :: IO ()
main = readRTTY

readRTTY :: IO ()
readRTTY = shelly $ runHandle "minimodem" ["-r", "-q", "rtty", "-S", "700", "-M", "870"] writeJson

writeJson :: Handle -> Sh ()
writeJson h = do
  liftIO $ hSetBuffering h NoBuffering
  line <- liftIO $ hGetLine h
  liftIO $ putStrLn $ "RECEIVED LINE: " ++ line
  let parsed = parseOnly parseLine (T.pack line)
  case parsed of
    Left err -> liftIO $ putStrLn $ "ERROR: " ++ err
    Right rttyLine -> do
      liftIO $ putStrLn $ "...which parsed into: " ++ show rttyLine
      liftIO $ writeFile "/tmp/rtty-coordinates.json" (C8L.unpack $ encode rttyLine)
  writeJson h


parseLine :: Parser RTTYLine
parseLine = do
  callsign' <- takeWhile1 (/= ':')
  _ <- char ':'
  longitude' <- takeWhile1 (/= ':')
  _ <- char ':'
  latitude' <- takeWhile1 (/= ':')
  _ <- char ':'
  altitude' <- takeWhile1 (/= ':')
  return $ RTTYLine callsign' (read $ T.unpack longitude' :: Longitude) (read $ T.unpack latitude' :: Latitude) (read $ T.unpack altitude' :: Meters)
