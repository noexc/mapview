{-# LANGUAGE OverloadedStrings #-}
import Control.Exception (finally)
import Control.Concurrent
import Control.Monad (forM_, forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import qualified Data.Text.IO as T
import qualified Filesystem.Path.CurrentOS as COS
import qualified Network.WebSockets as WS
import System.Directory (doesFileExist)
import System.FSNotify

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
main = do
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  uuid <- UUIDv4.nextRandom
  liftIO $ putStrLn $ "New client: " ++ UUID.toString uuid
  let client = (UUID.toString uuid, conn)
  flip finally (disconnect client) $ do
    liftIO $ modifyMVar_ state $ \s -> do
      let s' = addClient client s
      WS.sendTextData (snd client) ("Welcome!" :: Text)
      broadcast "someone joined" s'

      l <- doesFileExist "/tmp/w8upd/coordinates-log.json"
      when l $ do
        json <- T.readFile "/tmp/w8upd/coordinates-log.json"
        WS.sendTextData (snd client) json

      e <- doesFileExist "/tmp/w8upd/rtty-coordinates.json"
      when e $ do
        json <- T.readFile "/tmp/w8upd/rtty-coordinates.json"
        WS.sendTextData (snd client) json

      return s'
    talk (snd client) state
  where
    disconnect client = do
      -- Remove client and return new state
      s <- modifyMVar state $ \s ->
        let s' = removeClient client s in return (s', s')
      broadcast "someone disconnected" s

talk :: WS.Connection -> MVar ServerState -> IO ()
talk conn state = forever $
  withManager $ \man -> do
    _ <- forkIO $
      watchTree man (COS.fromText $ T.pack "/tmp/w8upd") (const True) handle
    msg <- WS.receiveData conn
    liftIO $ readMVar state >>= broadcast msg
  where
    handle :: Event -> IO ()
    handle (Modified fp _) = handle' fp
    handle (Added fp _)    = handle' fp
    handle (Removed _ _)   = WS.sendTextData conn (T.pack "File got deleted somehow. Ut oh!")

    handle' :: COS.FilePath -> IO ()
    handle' fp = do
      let filename = COS.encodeString fp
      when (filename == "/tmp/w8upd/rtty-coordinates.json") $ do
        json <- T.readFile "/tmp/w8upd/rtty-coordinates.json"
        WS.sendTextData conn json
