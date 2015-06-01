module Main where

import Data.Foldable (forM_)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB
import KD8ZRC.Mapview.Types
import Network.GPSD
import Network.GPSD.Types
import Options.Applicative hiding (Failure, Parser, Success)
import Pipes

tpvToCoordinates :: Tpv -> Maybe LookangleCoordinates
tpvToCoordinates tpv = do
  lat' <- tpvLat tpv
  lon' <- tpvLon tpv
  alt' <- tpvAlt tpv
  return $ LookangleCoordinates (Coordinates lat' lon') alt'

writeCoordinates :: ConfigFileOptions -> Maybe LookangleCoordinates -> IO ()
writeCoordinates cfg c =
  forM_ c (LB.writeFile (gpsdCoordinates cfg) . encode)

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
