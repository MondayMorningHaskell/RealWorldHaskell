module Cache where

import           Control.Monad (void)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Int (Int64)
import           Database.Redis

import           BasicSchema

type RedisInfo = ConnectInfo

localRedisInfo :: RedisInfo
localRedisInfo = defaultConnectInfo

runRedisAction :: RedisInfo -> Redis a -> IO a
runRedisAction redisInfo action = do
  connection <- connect redisInfo
  runRedis connection action

cacheUser :: RedisInfo -> Int64 -> User -> IO ()
cacheUser redisInfo uid user = runRedisAction redisInfo $ void $ setex (pack . show $ uid) 3600 (pack . show $ user)

fetchUserRedis :: RedisInfo -> Int64 -> IO (Maybe User)
fetchUserRedis redisInfo uid = runRedisAction redisInfo $ do
  result <- get (pack . show $ uid)
  case result of
    Right (Just userString) -> return $ Just (read . unpack $ userString)
    _ -> return Nothing

deleteUserCache :: RedisInfo -> Int64 -> IO ()
deleteUserCache redisInfo uid = do
  connection <- connect redisInfo
  runRedis connection $ do
    _ <- del [pack . show $ uid]
    return ()
