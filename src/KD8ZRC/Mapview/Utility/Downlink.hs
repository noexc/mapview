{-# LANGUAGE OverloadedStrings #-}
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

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Text as T
import GHC.IO.Handle
import KD8ZRC.Mapview.Types
import Text.Trifecta
import Shelly hiding (time)

-- | Fires off the callbacks in our 'MapviewConfig''s 'mvPacketLineCallback'
-- field. For now, you probably want to make sure to call this from your
-- downlink-obtaining function, on each line.
packetCallbackCaller :: String -> MV t ()
packetCallbackCaller pkt = do
  sequenceOf_ (mvPacketLineCallback . traverse . to (`getPacketLineCallback` pkt)) =<< ask
  parsedPacketCallbackCaller pkt

-- | Fires off the callbacks in our 'MapviewConfig''s 'mvParsedPacketCallback'
-- field. For now, you probably want to make sure to call this from your
-- downlink-obtaining function, on each line. This should be fixed at some
-- point, because it forces us to give up a separation of concerns, which is
-- annoying. I'm not yet sure of the correct abstraction, however.
parsedPacketCallbackCaller :: String -> MV t ()
parsedPacketCallbackCaller pkt = do
  config <- ask
  let parsed = parseString (config ^. mvParser) mempty pkt
  mapM_ (`caller` parsed) (_mvParsedPacketCallback config)
  where
    caller :: ParsedPacketCallback t -> Result t -> MV t ()
    caller (ParseSuccessCallback c) (Success t) = c t
    caller (ParseFailureCallback c) (Failure a) = c a
    caller _ _ = return ()


-- | This callback provides a way to obtain downlink data by shelling out to an
-- audio modem implementation, such as @minimodem@ or @fldigi@, and using each
-- (newline-separated) line of its standard output as a downlink packet.
modemStdout ::
  T.Text      -- ^ The modem command to run (e.g. @minimodem@ or @fldigi-shell@)
  -> [T.Text] -- ^ Flags/arguments to pass to the modem command
  -> MV t ()
modemStdout exe args = do
  config <- ask
  shelly $ runHandle (fromText exe) args (hndl config)
  where
    hndl :: MonadIO m => MapviewConfig t -> Handle -> m ()
    hndl c h = do
      liftIO $ hSetBuffering h NoBuffering
      line' <- liftIO $ hGetLine h
      unless (null line') (liftIO $ runReaderT (packetCallbackCaller line') c)
      hndl c h
