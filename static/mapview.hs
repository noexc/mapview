{-# LANGUAGE OverloadedStrings #-}
module Main where

import DOM
import Fay.Text
import FFI
import Prelude

data WebSocket

newWebSocket :: String -> Fay WebSocket
newWebSocket = ffi "new WebSocket(%1)"

logConsole :: String -> Fay ()
logConsole = ffi "console.log(%1)"

data MapUpdate = MapUpdate {
    callsign  :: Text
  , latitude  :: Double
  , longitude :: Double
  , altitude  :: Double
  , time      :: String
  } deriving Show

getData :: Event -> Fay Text
getData = ffi "%1.data"

parseUpdate :: Text -> Fay MapUpdate
parseUpdate = ffi "JSON.parse(%1)"

containsText :: String -> String -> Fay Bool
containsText = ffi "%1.indexOf(%2) !== -1"

class Eventable a
instance Eventable WebSocket

addEventListener :: Eventable a => a -> String -> (Event -> Fay ()) -> Fay ()
addEventListener = ffi "%1[%2] = %3"

--- Maps stuff (should possibly be moved to separate file) ---

data LatLng

newLatLng :: Double -> Double -> Fay LatLng
newLatLng = ffi "new google.maps.LatLng(%1, %2)"

data MapOptions = MapOptions {
    zoom :: Int
  , center :: LatLng
  , mapTypeId :: String
  }

data Map

gMap :: Element -> MapOptions -> Fay (Ptr Map)
gMap = ffi "new google.maps.Map(%1, %2)"

data MarkerOptions = MarkerOptions {
    position :: LatLng
  , map :: Ptr Map
  , title :: String
  }

data Marker

gMarker :: MarkerOptions -> Fay (Ptr Marker)
gMarker = ffi "new google.maps.Marker(%1)"

gMarkerSetPosition :: Ptr Marker -> LatLng -> Fay ()
gMarkerSetPosition = ffi "%1.setPosition(%2)"

panTo :: Ptr Map -> LatLng -> Fay ()
panTo = ffi "%1.panTo(%2)"

main :: Fay ()
main = do
  socket <- newWebSocket "ws://localhost:9160"

  mapE <- getElementById "map-canvas"
  startingPoint <- newLatLng 0.0 0.0
  v <- gMap mapE (MapOptions 6 startingPoint "roadmap")
  marker <- gMarker $ MarkerOptions startingPoint v "Testing!"
  addEventListener socket "onmessage" (\e -> updateMap e v marker)
  return ()

  where
    updateMap :: Event -> Ptr Map -> Ptr Marker -> Fay ()
    updateMap e v marker = do
      msgData <- getData e
      broadcast <- (unpack msgData) `containsText` "someone "
      when (not broadcast && (unpack msgData) /= "Welcome!") $ do
        update <- parseUpdate msgData
        logConsole $ show update
        l <- newLatLng (latitude update) (longitude update)
        panTo v l
        gMarkerSetPosition marker l
