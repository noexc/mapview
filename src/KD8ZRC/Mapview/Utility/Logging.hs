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
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Data.Thyme
import KD8ZRC.Mapview.Types
import System.Console.ANSI
import Text.PrettyPrint.ANSI.Leijen hiding ((<>))

-- | Log a token and string to standard-output.
logStdout :: MonadIO m => String -> BS.ByteString -> m ()
logStdout token s =
  liftIO . BS.putStrLn $ "[" <> BS.pack token <> "] " <> s

--------------------------------------------------------------------------------
-- Raw Packet Callbacks
--------------------------------------------------------------------------------

-- | Log a raw packet to standard output.
logRawPacketStdout :: TelemetryReceivedCallback BS.ByteString t
logRawPacketStdout =
  TelemetryReceivedCallback (
    logStdout
      (setSGRCode [Reset, SetColor Foreground Vivid Yellow] ++
       "RX-RAW" ++
       setSGRCode [Reset]))

-- | Log a raw packet to a file.
logRawPacketFile ::
  String -- ^ The filename to log to.
  -> TelemetryReceivedCallback BS.ByteString t
logRawPacketFile file =
  TelemetryReceivedCallback (\s -> liftIO (formatLine s >>= BS.appendFile file))
  where
    formatLine s = do
      current <- getCurrentTime
      return $ "[" <> BS.pack (show current) <> "] " <> s <> "\n"

--------------------------------------------------------------------------------
-- Parsed Packet Callbacks
--------------------------------------------------------------------------------

logParsedPacketStdoutSuccess :: Show t => ParsedPacketCallback t
logParsedPacketStdoutSuccess =
  ParseSuccessCallback (
    logStdout (setSGRCode
               [Reset, SetColor Foreground Vivid Green] ++
               "PRS-OK" ++ setSGRCode [Reset]) . BS.pack . show)

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
