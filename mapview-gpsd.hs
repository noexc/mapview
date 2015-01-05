module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB
import KD8ZRC.Mapview.Types
import Network.GPSD
import Network.GPSD.Types
import Options.Applicative hiding (Failure, Parser, Success)
import Pipes

tpvToCoordinates :: Tpv -> Maybe Coordinates
tpvToCoordinates tpv = do
  lat' <- tpvLat tpv
  lon' <- tpvLon tpv
  return $ Coordinates lat' lon'

writeCoordinates :: ConfigFileOptions -> Maybe Coordinates -> IO ()
writeCoordinates cfg c =
  case c of
   Nothing -> return ()
   Just c' -> LB.writeFile (gpsdHistory cfg) . encode $ c'

main :: IO ()
main = execParser opts >>= runMain
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> progDesc "Process GPSD data and store it for sending to clients"
     <> header "mapview-gpsd - Process GPSD data and store it")

runMain :: CLIOptions -> IO ()
runMain c = snd <$> runConfig c >>= loopGpsd

loopGpsd :: ConfigFileOptions -> IO ()
loopGpsd c = do
  s <- connectGPSD
  runEffect $
    for (skipErrors (socketToPipe s) :: Producer Tpv IO ())
    (\x -> lift . writeCoordinates c . tpvToCoordinates $ x)

