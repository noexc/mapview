{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  W8UPD.MapView.Announce.Main
-- Copyright   :  (c) 2014 Ricky Elrod>
--
-- Maintainer  :  Ricky Elrod <ricky@elrod.me>
-- Stability   :  experimental
-- Portability :  portable

module Main where

import Options.Applicative

data Announcement =
    Emergency String
    -- ^ Used when something terrible has happened. mapview-psc uses JS @alert()@ for this. This should almost never be used.
    | Critical String
      -- ^ Used when something bad has happened.
    | Warning String
      -- ^ Used when some difficulties are being experienced or something might go worse than planned.
    | Successful String
      -- ^ Used when something has gone right. :-)
    | Info String
      -- ^ Used for general purpose information.

severityOptions :: (String -> Announcement) -> Parser Announcement
severityOptions s =
  s <$> argument str (metavar "MESSAGE")

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
runAnnouncement (Emergency s)  = putStrLn s
runAnnouncement (Critical s)   = putStrLn s
runAnnouncement (Warning s)    = putStrLn s
runAnnouncement (Successful s) = putStrLn s
runAnnouncement (Info s)       = putStrLn s
