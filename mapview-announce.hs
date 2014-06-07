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
--import Data.Configurator
import Data.Text
import Options.Applicative

data FullCommand = FullCommand GlobalOptions Announcement

data Announcement = Announcement Urgency String

data GlobalOptions =
  GlobalOptions { configFile :: String }

data Urgency
  =  Emergency
     -- ^ Used when something terrible has happened. mapview-psc uses JS @alert()@ for this. This should almost never be used.
    | Critical
      -- ^ Used when something bad has happened.
    | Warning
      -- ^ Used when some difficulties are being experienced or something might go worse than planned.
    | Successful
      -- ^ Used when something has gone right. :-)
    | Info
      -- ^ Used for general purpose information.

instance ToJSON Announcement where
  toJSON (Announcement u s)  = object ["data" .= ("announcement" :: Text), "severity" .= show u, "text" .= s]

instance Show Urgency where
  show Emergency  = "emergency"
  show Critical   = "critical"
  show Warning    = "warning"
  show Successful = "success"
  show Info       = "info"

severityOptions :: (String -> Announcement) -> Parser Announcement
severityOptions = flip fmap (argument str (metavar "MESSAGE"))

announceOptions :: Parser Announcement
announceOptions =
  subparser (
    (command "emergency" (info (severityOptions (Announcement Emergency))
                          (progDesc "Send an emergency broadcast. Use very sparingly.")))
    <> (command "critical" (info (severityOptions (Announcement Critical))
                            (progDesc "Send a critical broadcast.")))
    <> (command "warning" (info (severityOptions (Announcement Warning))
                           (progDesc "Send a warning broadcast.")))
    <> (command "successful" (info (severityOptions (Announcement Successful))
                              (progDesc "Send an announcement about something that went right.")))
    <> (command "info" (info (severityOptions (Announcement Info))
                        (progDesc "Send a general informative announcement")))
    )

globalOptions :: Parser GlobalOptions
globalOptions = GlobalOptions
                <$> strOption (long "conf" <> short 'c' <> metavar "CONFIG_FILE")

fullCommand :: Parser FullCommand
fullCommand = FullCommand
              <$> globalOptions
              <*> announceOptions

opts :: ParserInfo FullCommand
opts = info (fullCommand <**> helper)
       (fullDesc
       <> progDesc "Broadcast announcements to mapview websocket clients"
       <> header "mapview-announce - an announcement generator for mapview")

main :: IO ()
main = execParser opts >>= runAnnouncement

runAnnouncement :: FullCommand -> IO ()
runAnnouncement (FullCommand _ a) = do
  let jsonData = C.unpack (encode a)
  putStrLn jsonData
