{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : KD8ZRC.Mapview.Utility.Coordinates
-- Copyright : (C) 2015 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Uses the <https://hackage.haskell.org/package/coordinate coordinate> package
-- to provide types for working with... coordinates.
----------------------------------------------------------------------------
module KD8ZRC.Mapview.Utility.Coordinates where

import Data.Geo.Coordinate

fromFractional :: Double -> Double -> Maybe Coordinate
fromFractional = (<°>)
