module Main where

import qualified Database.SQLite.Simple as SQ

import qualified Paris
import qualified Persist

-- (state1,actions1) <- Random.evalRandIO Paris.randomGame
-- game <- SQ.withConnection "/Users/nathan/src/haskell/deep-games/game-archive.sqlite" (Persist.recordGame actions1)
-- actions1b <- SQ.withConnection "/Users/nathan/src/haskell/deep-games/game-archive.sqlite" (Persist.loadGame game)

main :: IO ()
main = do
  (gs, actions) <- Paris.randomGame
  let gs' = (Paris.fixedPlayout gs actions)
  Paris.drawPCMap gs'
  SQ.withConnection
    "/Users/nathan/src/haskell/deep-games/game-archive.sqlite"
    (Persist.recordGame actions)
  return ()
