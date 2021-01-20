module Main where

import System.Environment (getArgs)

import Database (localConnString, migrateDB)
import qualified DatabaseEsq as E

main :: IO ()
main = do
  args <- getArgs
  if null args || head args /= "esq"
    then migrateDB localConnString
    else E.migrateDB localConnString
