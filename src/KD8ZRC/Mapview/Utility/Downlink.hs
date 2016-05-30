{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module : KD8ZRC.Mapview.Utility.Downlink
-- Copyright : (C) 2015 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Provides standard implementations of raw downlink callbacks, including:
--
--   * How to receive the downlink (modem commands, etc.)
--
-- Standard callbacks for parsing, logging, and other data manipulation
-- happens in other modules. This module only deals with how to __obtain__
-- the downlink data, by e.g., calling out to @minimodem@ or @fldigi-shell@,
-- etc.
----------------------------------------------------------------------------
module KD8ZRC.Mapview.Utility.Downlink where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as BS
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mempty)
#endif
import qualified Data.Text as T
import GHC.IO.Handle
import KD8ZRC.Mapview.Types
import Text.Trifecta hiding (Parser)
import Shelly hiding (FilePath, path, time)
import System.FSNotify

type Parser t = forall m. (DeltaParsing m, Errable m) => m t

-- | Fires off the callbacks in our 'MapviewConfig''s 'mvPacketLineCallback'
-- field. For now, you probably want to make sure to call this from your
-- downlink-obtaining function, on each line.
packetCallbackCaller
  :: BS.ByteString
  -> [ParsedPacketCallback t]
  -> Parser t
  -> MV t ()
packetCallbackCaller pkt cbs parser = do
  config <- ask
  sequenceOf_
    (onTelemetry . traverse . to (`getTelemetryReceivedCallback` pkt))
    config
  parsedPacketCallbackCaller parser pkt cbs

-- | Fires off the callbacks in our 'MapviewConfig''s 'mvParsedPacketCallback'
-- field. For now, you probably want to make sure to call this from your
-- downlink-obtaining function, on each line. This should be fixed at some
-- point, because it forces us to give up a separation of concerns, which is
-- annoying. I'm not yet sure of the correct abstraction, however.
parsedPacketCallbackCaller
  :: Parser t
  -> BS.ByteString
  -> [ParsedPacketCallback t]
  -> MV t ()
parsedPacketCallbackCaller parser pkt cbs = do
  let parsed = parseByteString parser mempty pkt
  mapM_ (`caller` parsed) cbs
  where
    caller :: ParsedPacketCallback t -> Result t -> MV t ()
    caller (ParseSuccessCallback c) (Success t) = c t
    caller (ParseFailureCallback c) (Failure a) = c a
    caller _ _ = return ()

-- | This callback provides a way to obtain downlink data by shelling out to an
-- audio modem implementation, such as @minimodem@ or @fldigi@, and using each
-- (newline-separated) line of its standard output as a downlink packet.
modemStdout
  :: forall t. T.Text      -- ^ The modem command to run (e.g. @minimodem@ or @fldigi-shell@)
  -> [T.Text] -- ^ Flags/arguments to pass to the modem command
  -> [ParsedPacketCallback t] -- ^ Callbacks to run after a parsing attempt
  -> Parser t
  -> MV t ()
modemStdout exe args cbs parser = do
  config <- ask
  shelly $ runHandle (fromText exe) args (hndl config)
  where
    hndl :: MonadIO m => MapviewConfig t -> Handle -> m ()
    hndl c h = do
      liftIO $ hSetBuffering h NoBuffering
      line' <- liftIO $ BS.hGetLine h
      unless (BS.null line') $
        (liftIO $ runReaderT (packetCallbackCaller line' cbs parser) c)
      hndl c h

-- | This callback listens for changes in a directory and acts on them. It has
-- no notion of a \"failed\" parse, and so it will __only call success
-- callbacks__.
--
-- It may be used for things such as waiting for slow-scan TV images to be fed
-- into a directory by an external program.
directoryListener
  :: FilePath
  -> [TelemetryReceivedCallback Event Event]
  -> MV Event ()
directoryListener path cbs = do
  config <- ask
  liftIO $ withManager $ \mgr -> do
    _ <- watchTree
         mgr
         path
         (const True)
         (\evt -> mapM_ (\(TelemetryReceivedCallback cb) -> liftIO $ runReaderT (cb evt) config) cbs)

    forever $ threadDelay 1000000
