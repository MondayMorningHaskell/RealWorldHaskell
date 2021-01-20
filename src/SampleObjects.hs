{-# LANGUAGE OverloadedStrings #-}
module SampleObjects where

import Data.Int (Int64)
import Database.Persist.Sql (toSqlKey)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import SchemaEsq

testUser1 :: User
testUser1 = User
  { userName = "james"
  , userEmail = "james@test.com"
  , userAge = 25
  , userOccupation = "Software Engineer"
  }

testUser2 :: User
testUser2 = User
  { userName = "kate"
  , userEmail = "kate@test.com"
  , userAge = 24
  , userOccupation = "Software Engineer"
  }

testUser3 :: User
testUser3 = User
  { userName = "jeremy"
  , userEmail = "jeremy@test.com"
  , userAge = 23
  , userOccupation = "Teacher"
  }

testUser4 :: User
testUser4 = User
  { userName = "alex"
  , userEmail = "alex@test.com"
  , userAge = 30
  , userOccupation = "Petroleum Engineer"
  }

testUser5 :: User
testUser5 = User
  { userName = "adam"
  , userEmail = "adam@test.com"
  , userAge = 30
  , userOccupation = "Accountant"
  }

testUser6 :: User
testUser6 = User
  { userName = "alexa"
  , userEmail = "alexa@test.com"
  , userAge = 30
  , userOccupation = "Mechanical Engineer"
  }

testArticle1 :: Int64 -> Article
testArticle1 uid = Article
  { articleTitle = "First post"
  , articleBody = "A great description of our first blog post body."
  , articlePublishedTime = posixSecondsToUTCTime 1498914000
  , articleAuthorId = toSqlKey uid
  }

testArticle2 :: Int64 -> Article
testArticle2 uid = Article
  { articleTitle = "Second post"
  , articleBody = "Dummy body description"
  , articlePublishedTime = posixSecondsToUTCTime 1498917600
  , articleAuthorId = toSqlKey uid
  }

testArticle3 :: Int64 -> Article
testArticle3 uid = Article
  { articleTitle = "Third post"
  , articleBody = "Fascinating!"
  , articlePublishedTime = posixSecondsToUTCTime 1498921200
  , articleAuthorId = toSqlKey uid
  }

testArticle4 :: Int64 -> Article
testArticle4 uid = Article
  { articleTitle = "Fourth post"
  , articleBody = "Overall summary of the blog"
  , articlePublishedTime = posixSecondsToUTCTime 1498924800
  , articleAuthorId = toSqlKey uid
  }

testArticle5 :: Int64 -> Article
testArticle5 uid = Article
  { articleTitle = "Fifth post"
  , articleBody = "Description of everything in the universe"
  , articlePublishedTime = posixSecondsToUTCTime 1498928400
  , articleAuthorId = toSqlKey uid
  }

testArticle6 :: Int64 -> Article
testArticle6 uid = Article
  { articleTitle = "Sixth post"
  , articleBody = "A reflection on the events of the past week"
  , articlePublishedTime = posixSecondsToUTCTime 1498932000
  , articleAuthorId = toSqlKey uid
  }

testArticle7 :: Int64 -> Article
testArticle7 uid = Article
  { articleTitle = "Seventh post"
  , articleBody = "A great description of our first blog post body."
  , articlePublishedTime = posixSecondsToUTCTime 1498914000
  , articleAuthorId = toSqlKey uid
  }

testArticle8 :: Int64 -> Article
testArticle8 uid = Article
  { articleTitle = "Eighth post"
  , articleBody = "Dummy body description"
  , articlePublishedTime = posixSecondsToUTCTime 1498917600
  , articleAuthorId = toSqlKey uid
  }

testArticle9 :: Int64 -> Article
testArticle9 uid = Article
  { articleTitle = "Ninth post"
  , articleBody = "Fascinating!"
  , articlePublishedTime = posixSecondsToUTCTime 1498921200
  , articleAuthorId = toSqlKey uid
  }

testArticle10 :: Int64 -> Article
testArticle10 uid = Article
  { articleTitle = "Tenth post"
  , articleBody = "Overall summary of the blog"
  , articlePublishedTime = posixSecondsToUTCTime 1498924800
  , articleAuthorId = toSqlKey uid
  }

testArticle11 :: Int64 -> Article
testArticle11 uid = Article
  { articleTitle = "Eleventh post"
  , articleBody = "Description of everything in the universe"
  , articlePublishedTime = posixSecondsToUTCTime 1498928400
  , articleAuthorId = toSqlKey uid
  }

testArticle12 :: Int64 -> Article
testArticle12 uid = Article
  { articleTitle = "Twelfth post"
  , articleBody = "A reflection on the events of the past week"
  , articlePublishedTime = posixSecondsToUTCTime 1498932000
  , articleAuthorId = toSqlKey uid
  }

testArticle13 :: Int64 -> Article
testArticle13 uid = Article
  { articleTitle = "Thirteenth post"
  , articleBody = "Overall summary of the blog"
  , articlePublishedTime = posixSecondsToUTCTime 1498914001
  , articleAuthorId = toSqlKey uid
  }

testArticle14 :: Int64 -> Article
testArticle14 uid = Article
  { articleTitle = "Fourteenth post"
  , articleBody = "Description of everything in the universe"
  , articlePublishedTime = posixSecondsToUTCTime 1498917601
  , articleAuthorId = toSqlKey uid
  }

testArticle15 :: Int64 -> Article
testArticle15 uid = Article
  { articleTitle = "Fifteenth post"
  , articleBody = "A reflection on the events of the past week"
  , articlePublishedTime = posixSecondsToUTCTime 1498921201
  , articleAuthorId = toSqlKey uid
  }

testArticle16 :: Int64 -> Article
testArticle16 uid = Article
  { articleTitle = "Sixteenth post"
  , articleBody = "Overall summary of the blog"
  , articlePublishedTime = posixSecondsToUTCTime 1498924801
  , articleAuthorId = toSqlKey uid
  }

testArticle17 :: Int64 -> Article
testArticle17 uid = Article
  { articleTitle = "Seventeenth post"
  , articleBody = "Description of everything in the universe"
  , articlePublishedTime = posixSecondsToUTCTime 1498928401
  , articleAuthorId = toSqlKey uid
  }

testArticle18 :: Int64 -> Article
testArticle18 uid = Article
  { articleTitle = "Eighteenth post"
  , articleBody = "A reflection on the events of the past week"
  , articlePublishedTime = posixSecondsToUTCTime 1498932001
  , articleAuthorId = toSqlKey uid
  }
