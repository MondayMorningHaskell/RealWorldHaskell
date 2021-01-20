{-# LANGUAGE OverloadedStrings #-}

module DatabaseEsq where

import           Control.Monad (void)
import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT,
                                       LogLevel(..), filterLogger)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Int (Int64)
import           Data.Maybe (listToMaybe)
import           Database.Esqueleto (select, from, where_, (^.), val, (==.), on,
                                     InnerJoin(..), limit, orderBy, desc)
import           Database.Persist (get, insert, delete, entityVal, Entity)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Database.Persist.Postgresql (ConnectionString, withPostgresqlConn,
                                              runMigration, SqlPersistT)

import           SchemaEsq

type PGInfo = ConnectionString

localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres"

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

fetchUserPG :: PGInfo -> Int64 -> IO (Maybe User)
fetchUserPG connString uid = runAction connString (get (toSqlKey uid))

createUserPG :: PGInfo -> User -> IO Int64
createUserPG connString user = fromSqlKey <$> runAction connString (insert user)

deleteUserPG :: PGInfo -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid

fetchArticlePG :: PGInfo -> Int64 -> IO (Maybe Article)
fetchArticlePG connString aid = runAction connString selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) (Maybe Article)
    selectAction = ((fmap entityVal) . listToMaybe) <$> (select . from $ \articles -> do
      where_ (articles ^. ArticleId ==. val (toSqlKey aid))
      return articles)

fetchArticlesByAuthorPG :: PGInfo -> Int64 -> IO [Entity Article]
fetchArticlesByAuthorPG connString uid = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Article]
    fetchAction = select . from $ \articles -> do
      where_ (articles ^. ArticleAuthorId ==. val (toSqlKey uid))
      return articles

fetchRecentArticlesPG :: PGInfo -> IO [(Entity User, Entity Article)]
fetchRecentArticlesPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [(Entity User, Entity Article)]
    fetchAction = select . from $ \(users `InnerJoin` articles) -> do
      on (users ^. UserId ==. articles ^. ArticleAuthorId)
      orderBy [desc (articles ^. ArticlePublishedTime)]
      limit 3
      return (users, articles)

createArticlePG :: PGInfo -> Article -> IO Int64
createArticlePG connString article = fromSqlKey <$> runAction connString (insert article)

deleteArticlePG :: PGInfo -> Int64 -> IO ()
deleteArticlePG connString aid = runAction connString (delete articleKey)
  where
    articleKey :: Key Article
    articleKey = toSqlKey aid
