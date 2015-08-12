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
import KD8ZRC.Mapview.Types

logStdout :: String -> String -> MV t ()
logStdout token s =
  liftIO . putStrLn $ "[" ++ token ++ "] " ++ s

-- | Log a raw packet to standard output.
logRawPacketStdout :: String -> MV t ()
logRawPacketStdout pkt = logStdout "RX-RAW" pkt
