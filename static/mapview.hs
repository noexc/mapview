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
    callsign    :: Text
  , coordinates :: Coordinates
  , altitude    :: Double
  , time        :: Text
  } deriving Show

data Coordinates = Coordinates {
    latitude :: Double
  , longitude :: Double
  } deriving Show

getData :: Event -> Fay Text
getData = ffi "%1.data"

data JSON
instance Show JSON

parseUpdate :: Text -> Fay MapUpdate
parseUpdate = ffi "JSON.parse(%1)"

parseCoordinatesList :: Text -> Fay [Coordinates]
parseCoordinatesList = ffi "JSON.parse(%1)"

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
instance Eventable Marker

gMarker :: MarkerOptions -> Fay (Ptr Marker)
gMarker = ffi "new google.maps.Marker(%1)"

gMarkerSetPosition :: Ptr Marker -> LatLng -> Fay ()
gMarkerSetPosition = ffi "%1.setPosition(%2)"

data InfoWindow

gInfoWindow :: InfoWindowOptions -> Fay (Ptr InfoWindow)
gInfoWindow = ffi "new google.maps.InfoWindow(%1)"

gInfoWindowOpen :: Ptr InfoWindow -> Ptr Map -> Ptr Marker -> Fay ()
gInfoWindowOpen = ffi "%1.open(%2, %3)"

gAddEvent :: Marker -> String -> Fay () -> Fay ()
gAddEvent = ffi "google.maps.event.addListener(%1, %2, %3)"

data InfoWindowOptions = InfoWindowOptions {
    content :: String
    }

panTo :: Ptr Map -> LatLng -> Fay ()
panTo = ffi "%1.panTo(%2)"

data Polyline

data PolylineOptions = PolylineOptions {
    path :: MVCArray
  , geodesic :: Bool
  , strokeColor :: String
  , strokeOpacity :: Double
  , stokeWeight :: Int
  }

newPolyline :: PolylineOptions -> Fay (Ptr Polyline)
newPolyline = ffi "new google.maps.Polyline(%1)"

plSetPath :: Ptr Polyline -> Ptr MVCArray -> Fay ()
plSetPath = ffi "%1.setPath(%2)"

polylineSetMap :: Ptr Polyline -> Ptr Map -> Fay ()
polylineSetMap = ffi "%1.setMap(%2)"

data MVCArray

newMVCArray :: Fay (Ptr MVCArray)
newMVCArray = ffi "new google.maps.MVCArray()"

mvcPush :: Ptr MVCArray -> Ptr LatLng -> Fay Int
mvcPush = ffi "%1.push(%2)"

mvcPop :: Ptr MVCArray -> Fay ()
mvcPop = ffi "%1.pop()"

main :: Fay ()
main = do
  socket <- newWebSocket "ws://localhost:9160"

  mapE <- getElementById "map-canvas"
  startingPoint <- newLatLng 0.0 0.0
  v <- gMap mapE (MapOptions 6 startingPoint "roadmap")
  marker <- gMarker $ MarkerOptions startingPoint v "Testing!"

  iw <- gInfoWindow $ InfoWindowOptions "<b>hello world!</b>"
  gAddEvent marker "click" (gInfoWindowOpen iw v marker)

  mvcArray <- newMVCArray
  let polyOpt = PolylineOptions mvcArray True "#ff0000" 1.0 2
  polyline <- newPolyline polyOpt


  polylineSetMap polyline v
  y <- mvcPush mvcArray startingPoint
  plSetPath polyline mvcArray

  addEventListener socket "onmessage" (\e -> updateMap e v marker mvcArray)
  return ()

  where
    updateMap :: Event -> Ptr Map -> Ptr Marker -> Ptr MVCArray -> Fay ()
    updateMap e v marker plPath = do
      msgData <- getData e
      broadcast <- unpack msgData `containsText` "someone "
      when (not broadcast && unpack msgData /= "Welcome!" && Prelude.head (unpack msgData) /= '[') $ do
        updateJson <- parseUpdate msgData
        l <- newLatLng (latitude $ coordinates updateJson) (longitude $ coordinates updateJson)
        panTo v l
        gMarkerSetPosition marker l
        mvcPush plPath l

      -- We should come up with a better way to pick this out at some point.
      when (Prelude.head (unpack msgData) == '[') $ do
        mvcPop plPath
        coordinates <- parseCoordinatesList msgData
        latlngs <- Prelude.mapM (\x ->  newLatLng (latitude x) (longitude x)) coordinates
        Prelude.mapM_ (mvcPush plPath) latlngs
