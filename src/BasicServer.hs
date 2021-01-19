{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module BasicServer where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Database (fetchUserPG, createUserPG, localConnString)
import           BasicSchema

type UsersAPI = 
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

fetchUsersHandler :: ConnectionString -> Int64 -> Handler User
fetchUsersHandler connString uid = do
  maybeUser <- liftIO $ fetchUserPG connString uid
  case maybeUser of
    Just user -> return user
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

createUserHandler :: ConnectionString -> User -> Handler Int64
createUserHandler connString user = liftIO $ createUserPG connString user

usersServer :: ConnectionString -> Server UsersAPI
usersServer connString = 
  (fetchUsersHandler connString) :<|> 
  (createUserHandler connString)

runServer :: IO ()
runServer = run 8000 (serve usersAPI (usersServer localConnString))
