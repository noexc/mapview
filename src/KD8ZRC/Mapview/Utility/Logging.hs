{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module : KD8ZRC.Mapview.Utility.Logging
-- Copyright : (C) 2015 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Provides standard implementations of logging callbacks, including logging:
--
--   * The raw downlink
--   * Websocket connections
--   * Parse successes and errors
----------------------------------------------------------------------------
module KD8ZRC.Mapview.Utility.Logging where

import Control.Monad.IO.Class
import Data.Thyme
import KD8ZRC.Mapview.Types
import System.Console.ANSI
import Text.PrettyPrint.ANSI.Leijen

-- | Log a token and string to standard-output.
logStdout :: MonadIO m => String -> String -> m ()
logStdout token s =
  liftIO . putStrLn $ "[" ++ token ++ "] " ++ s

--------------------------------------------------------------------------------
-- Raw Packet Callbacks
--------------------------------------------------------------------------------

-- | Log a raw packet to standard output.
logRawPacketStdout :: PacketLineCallback t
logRawPacketStdout =
  PacketLineCallback (
    logStdout
      (setSGRCode [Reset, SetColor Foreground Vivid Yellow] ++
       "RX-RAW" ++
       setSGRCode [Reset]))

-- | Log a raw packet to a file.
logRawPacketFile ::
  String -- ^ The filename to log to.
  -> PacketLineCallback t
logRawPacketFile file =
  PacketLineCallback (\s -> liftIO (formatLine s >>= appendFile file))
  where
    formatLine s = do
      current <- getCurrentTime
      return $ "[" ++ show current ++ "] " ++ s ++ "\n"

--------------------------------------------------------------------------------
-- Parsed Packet Callbacks
--------------------------------------------------------------------------------

logParsedPacketStdoutSuccess :: Show t => ParsedPacketCallback t
logParsedPacketStdoutSuccess =
  ParseSuccessCallback (
    logStdout
             (setSGRCode
              [Reset, SetColor Foreground Vivid Green] ++
              "PRS-OK" ++ setSGRCode [Reset]) . show)

logParsedPacketStdoutFailure :: ParsedPacketCallback t
logParsedPacketStdoutFailure =
  ParseFailureCallback $ \d -> do
    logStdout (setSGRCode
               [Reset, SetColor Foreground Vivid Red] ++
               "PRS-FL" ++ setSGRCode [Reset]) ""
    liftIO $ putDoc d >> putStrLn ""

logParsedPacketStdout :: Show t => [ParsedPacketCallback t]
logParsedPacketStdout = [ logParsedPacketStdoutSuccess
                        , logParsedPacketStdoutFailure
                        ]
