-----------------------------------------------------------------------------
-- |
-- Module : KD8ZRC.Mapview.Execute
-- Copyright : (C) 2015 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Provides a function that kicks off the backend (TODO: better description once
-- we have a better idea of how this will actually work).
----------------------------------------------------------------------------
module KD8ZRC.Mapview.Execute where

import Control.Lens
import Control.Monad.Trans.Reader
import KD8ZRC.Mapview.Types

-- | Takes a 'MapviewConfig' and uses its '_mvDownlinkSpawn' callback to start
-- our backend process.
--
-- TODO: Threading stuff -- where, when, how?
mapview :: MapviewConfig t -> IO ()
mapview config = runReaderT (config ^. telemetry) config
