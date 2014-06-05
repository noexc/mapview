{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  W8UPD.MapView.Announce.Main
-- Copyright   :  (c) 2014 Ricky Elrod>
--
-- Maintainer  :  Ricky Elrod <ricky@elrod.me>
-- Stability   :  experimental
-- Portability :  portable

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text
import Options.Applicative

data Announcement =
    Emergency {
      text :: String
      -- ^ Used when something terrible has happened. mapview-psc uses JS @alert()@ for this. This should almost never be used.
    }
    | Critical {
      text :: String
      -- ^ Used when something bad has happened.
    }
    | Warning {
      text :: String
      -- ^ Used when some difficulties are being experienced or something might go worse than planned.
    }
    | Successful {
      text :: String
      -- ^ Used when something has gone right. :-)
    }
    | Info {
      text :: String
      -- ^ Used for general purpose information.
    }

instance ToJSON Announcement where
  toJSON m@(Emergency s)  = object ["data" .= ("announcement" :: Text), "severity" .= show m, "text" .= s]
  toJSON m@(Critical s)  = object ["data" .= ("announcement" :: Text), "severity" .= show m, "text" .= s]
  toJSON m@(Warning s)  = object ["data" .= ("announcement" :: Text), "severity" .= show m, "text" .= s]
  toJSON m@(Successful s)  = object ["data" .= ("announcement" :: Text), "severity" .= show m, "text" .= s]
  toJSON m@(Info s)  = object ["data" .= ("announcement" :: Text), "severity" .= show m, "text" .= s]

instance Show Announcement where
  show (Emergency _)  = "emergency"
  show (Critical _)   = "critical"
  show (Warning _)    = "warning"
  show (Successful _) = "success"
  show (Info _)       = "info"

severityOptions :: (String -> Announcement) -> Parser Announcement
severityOptions = flip fmap (argument str (metavar "MESSAGE"))

announceOptions :: Parser Announcement
announceOptions =
  subparser (
    (command "emergency" (info (severityOptions Emergency)
                          (progDesc "Send an emergency broadcast. Use very sparingly.")))
    <> (command "critical" (info (severityOptions Critical)
                            (progDesc "Send a critical broadcast.")))
    <> (command "warning" (info (severityOptions Warning)
                           (progDesc "Send a warning broadcast.")))
    <> (command "successful" (info (severityOptions Successful)
                              (progDesc "Send an announcement about something that went right.")))
    <> (command "info" (info (severityOptions Info)
                        (progDesc "Send a general informative announcement")))
    )

opts :: ParserInfo Announcement
opts = info (announceOptions <**> helper) idm

main :: IO ()
main = execParser opts >>= runAnnouncement

runAnnouncement :: Announcement -> IO ()
runAnnouncement s = putStrLn $ C.unpack (encode s)
