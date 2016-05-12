-----------------------------------------------------------------------------
-- |
-- Module : KD8ZRC.Mapview.Utility.Concurrent
-- Copyright : (C) 2016 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Provides callbacks to aid in concurrent applications (e.g., updating a 'Chan'
-- at various stages).
----------------------------------------------------------------------------
module KD8ZRC.Mapview.Utility.Concurrent where

import qualified Control.Concurrent.Chan as Chan
import Control.Monad.IO.Class
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import KD8ZRC.Mapview.Types

--------------------------------------------------------------------------------
-- Raw Packet Callbacks
--------------------------------------------------------------------------------

writeChanRaw :: Chan.Chan BS.ByteString -> PacketLineCallback t
writeChanRaw ch =
  PacketLineCallback (\raw -> liftIO $ Chan.writeChan ch raw)

-- | If we know that we can JSON-encode a packet, then we can use this fact to
-- send an encoded version of the packet to the web-socket automagically.
writeChanJsonPkt :: ToJSON t => Chan.Chan BS.ByteString -> ParsedPacketCallback t
writeChanJsonPkt ch =
  ParseSuccessCallback (
    \pkt -> liftIO $ Chan.writeChan ch (BSL.toStrict . encode $ pkt))
