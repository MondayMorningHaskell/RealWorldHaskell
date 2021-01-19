module Main where

import DatabaseEsq (localConnString, migrateDB)

main :: IO ()
main = migrateDB localConnString
