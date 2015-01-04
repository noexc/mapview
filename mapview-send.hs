{-# LANGUAGE OverloadedStrings #-}
import Control.Exception (finally)
import Control.Concurrent
import Control.Monad (forM_, forever, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Configurator as Cfg
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import qualified Data.Text.IO as T
import qualified Filesystem.Path.CurrentOS as COS
import qualified Network.WebSockets as WS
import Options.Applicative hiding (Failure, Parser, Success)
import System.Directory (doesFileExist)
import System.FSNotify

import KD8ZRC.Mapview.Types

type Client = (String, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient = (:)

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ (map snd clients) (`WS.sendTextData` message)

main :: IO ()
main = execParser opts >>= runMain
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> progDesc "Listen for updates to files containing Telemetry data \
                 \and forward them along via websockets."
     <> header "mapview-send - Telemetry to WebSockets relay" )

runMain :: CLIOptions -> IO ()
runMain c = do
  (config, opts) <- runConfig c

  state <- newMVar newServerState

  host <- Cfg.require config "websockets.host"
  port <- Cfg.require config "websockets.port"
  WS.runServer host port $ application opts state

application :: ConfigFileOptions -> MVar ServerState -> WS.ServerApp
application opts state pending = do
  conn <- WS.acceptRequest pending
  uuid <- UUIDv4.nextRandom
  liftIO $ putStrLn $ "New client: " ++ UUID.toString uuid
  let client = (UUID.toString uuid, conn)
  flip finally (disconnect client) $ do
    liftIO $ modifyMVar_ state $ \s -> do
      let s' = addClient client s
      WS.sendTextData (snd client) ("Welcome!" :: Text)
      broadcast "someone joined" s'

      l <- doesFileExist (historyPath opts)
      when l $ do
        json <- T.readFile (historyPath opts)
        WS.sendTextData (snd client) json

      e <- doesFileExist (workingPath opts)
      when e $ do
        json <- T.readFile (workingPath opts)
        WS.sendTextData (snd client) json

      return s'
    talk opts (snd client) state
  where
    disconnect client = do
      -- Remove client and return new state
      s <- modifyMVar state $ \s ->
        let s' = removeClient client s in return (s', s')
      broadcast "someone disconnected" s

talk :: ConfigFileOptions -> WS.Connection -> MVar ServerState -> IO ()
talk opts conn state = forever $
  withManager $ \man -> do
    _ <- watchTree man (COS.fromText $ T.pack (baseDir (workingPath opts))) (const True) handle
    msg <- WS.receiveData conn
    liftIO $ readMVar state >>= broadcast msg
  where
    handle :: Event -> IO ()
    handle (Modified fp _) = handle' fp
    handle (Added fp _)    = handle' fp
    handle (Removed _ _)   = WS.sendTextData conn (T.pack "File got deleted somehow. Uh oh!")

    handle' :: COS.FilePath -> IO ()
    handle' fp = do
      let filename = COS.encodeString fp
      when (filename == workingPath opts) $ do
        json <- T.readFile (workingPath opts)
        WS.sendTextData conn json
