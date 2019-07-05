module Main where

import qualified Database.SQLite.Simple as SQ

import qualified Paris
import qualified Persist

main :: IO ()
main = do
  actions <- Paris.drawPCMap
  SQ.withConnection
    "/Users/nathan/src/haskell/deep-games/game-archive.sqlite"
    (Persist.recordGame actions)
