module TestUtils where

import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Database.Persist.Postgresql (withPostgresqlConn, runMigrationSilent)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientEnv(..), parseBaseUrl)

import CacheServer (runServer)
import Cache (RedisInfo, localRedisInfo)
import Database (PGInfo, localConnString)
import BasicSchema (migrateAll)

setupTests :: IO (PGInfo, RedisInfo, ClientEnv, ThreadId)
setupTests = do
  mgr <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8000"
  let clientEnv = ClientEnv mgr baseUrl Nothing
  runStdoutLoggingT $ withPostgresqlConn localConnString $ \dbConn ->
    runReaderT (runMigrationSilent migrateAll) dbConn
  tid <- forkIO runServer
  threadDelay 1000000
  return (localConnString, localRedisInfo, clientEnv, tid)
