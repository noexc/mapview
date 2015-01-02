module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB
import KD8ZRC.Mapview.Types (Coordinates (..))
import Network.GPSD
import Network.GPSD.Types
import Pipes

tpvToCoordinates :: Tpv -> Maybe Coordinates
tpvToCoordinates tpv = do
  lat' <- tpvLat tpv
  lon' <- tpvLon tpv
  return $ Coordinates lat' lon'

writeCoordinates :: Maybe Coordinates -> IO ()
writeCoordinates c = case c of
                      Nothing -> return ()
                      Just c' -> LB.writeFile "/tmp/gpsd" . encode $ c'

main :: IO ()
main = do
  s <- connectGPSD
  runEffect $
    for (skipErrors (socketToPipe s) :: Producer Tpv IO ())
    (\x -> lift . writeCoordinates . tpvToCoordinates $ x)

