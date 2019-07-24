-- | Stores game records in SQLite

{-# LANGUAGE OverloadedStrings #-}

module Persist where

import Control.Lens ((%~), (&), (.~), (^.))
import qualified Database.SQLite.Simple as SQ
import qualified Data.String.Combinators as SC
import qualified Data.List as List
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Traversable (traverse)
import qualified Control.Exception as CE

import Paris

corpToInt :: Corp -> Int
corpToInt Yellow = 0
corpToInt Blue = 1
corpToInt Brown = 2
corpToInt Purple = 3
corpToInt Black = 4
corpToInt Red = 5

intToCorp :: Int -> Corp
intToCorp 0 = Yellow
intToCorp 1 = Blue
intToCorp 2 = Brown
intToCorp 3 = Purple
intToCorp 4 = Black
intToCorp 5 = Red

tNumToInt :: TNum -> Int
tNumToInt TOne = 1
tNumToInt TTwo = 2

intToTNum :: Int -> TNum
intToTNum 1 = TOne
intToTNum 2 = TTwo

holdingToTuple h =
  ((_yellow h), (_blue h), (_brown h), (_purple h), (_black h), (_red h))

tupleToHolding (yellow, blue, brown, purple, black, red) =
  Holding
  { _yellow = yellow
  , _blue = blue
  , _brown = brown
  , _purple = purple
  , _black = black
  , _red = red
  }

insertBuild :: SQ.Connection -> Int -> Int -> Build -> IO ()
insertBuild conn game move build =
  SQ.execute
    conn
    "INSERT INTO build_actions (game,move,corp,hex_row,hex_col) VALUES (?,?,?,?,?)"
    ((game, move, corpToInt (_buildCorp build)) SQ.:. (_buildHex build))

createBuildTable :: SQ.Query
createBuildTable =
  "CREATE TABLE IF NOT EXISTS build_actions " <>
  (SC.parens
     (mconcat
        (SC.punctuate
           ", "
           [ "game INTEGER"
           , "move INTEGER"
           , "corp INTEGER"
           , "hex_row INTEGER"
           , "hex_col INTEGER"
           ])))

insertStopBuild :: SQ.Connection -> Int -> Int -> IO ()
insertStopBuild conn game move =
  SQ.execute
    conn
    "INSERT INTO stop_build_actions (game,move) VALUES (?,?)"
    (game, move)

createStopBuildTable :: SQ.Query
createStopBuildTable =
  "CREATE TABLE IF NOT EXISTS stop_build_actions " <>
  (SC.parens (mconcat (SC.punctuate ", " ["game INTEGER", "move INTEGER"])))

insertTrade :: SQ.Connection -> Int -> Int -> Trade -> IO ()
insertTrade conn game move trade =
  SQ.execute
    conn
    "INSERT INTO trade_actions (game,move,num,sell,buy) VALUES (?,?,?,?,?)"
    ( game
    , move
    , tNumToInt (_tNum trade)
    , corpToInt (_sell trade)
    , corpToInt (_buy trade))

createTradeTable :: SQ.Query
createTradeTable =
  "CREATE TABLE IF NOT EXISTS trade_actions " <>
  (SC.parens
     (mconcat
        (SC.punctuate
           ", "
           [ "game INTEGER"
           , "move INTEGER"
           , "num INTEGER"
           , "sell INTEGER"
           , "buy INTEGER"
           ])))

insertTakeInitialShare :: SQ.Connection -> Int -> Int -> Holding -> IO ()
insertTakeInitialShare conn game move holding =
  SQ.execute
    conn
    "INSERT INTO take_initial_share_actions (game,move,yellow,blue,brown,purple,black,red) VALUES (?,?,?,?,?,?,?,?)"
    ((game, move) SQ.:. (holdingToTuple holding))

createTakeInitialShareTable :: SQ.Query
createTakeInitialShareTable =
  "CREATE TABLE IF NOT EXISTS take_initial_share_actions " <>
  (SC.parens
     (mconcat
        (SC.punctuate
           ", "
           [ "game INTEGER"
           , "move INTEGER"
           , "yellow INTEGER"
           , "blue INTEGER"
           , "brown INTEGER"
           , "purple INTEGER"
           , "black INTEGER"
           , "red INTEGER"
           ])))

createGameTable :: SQ.Query
createGameTable =
  "CREATE TABLE IF NOT EXISTS games " <>
  (SC.parens (mconcat (SC.punctuate ", " ["game INTEGER PRIMARY KEY"])))

-- Persist.createDB "/Users/nathan/src/haskell/deep-games/game-archive.sqlite"

createDB filename =
  SQ.withConnection
    filename
    (\conn -> do
       SQ.execute_ conn createBuildTable
       SQ.execute_ conn createStopBuildTable
       SQ.execute_ conn createTakeInitialShareTable
       SQ.execute_ conn createTradeTable
       SQ.execute_ conn createGameTable)


insertAction :: SQ.Connection -> Int -> Int -> Action -> IO ()
insertAction conn game move (ABuild a) = insertBuild conn game move a
insertAction conn game move (StopBuild) = insertStopBuild conn game move
insertAction conn game move (ATrade t) = insertTrade conn game move t
insertAction conn game move (ATakeInitialShare h) = insertTakeInitialShare conn game move h

data ImpossibleDBResultException = ImpossibleDBResultException
  { idreErrMsg :: !Text
  } deriving (Show, Typeable)

instance CE.Exception ImpossibleDBResultException

-- | returns id of the added game
recordGame :: [Action] -> SQ.Connection -> IO Int
recordGame actions conn = do
  r <- SQ.query conn "SELECT MAX(game) FROM games" ()
  let game =
        case r of
          [SQ.Only (Nothing)] -> 0 :: Int
          [SQ.Only (Just n)] -> n + 1
          _ ->
            CE.throw
              (ImpossibleDBResultException
                 "Multiple MAX results in recordGame?!")
  SQ.execute conn "INSERT INTO games (game) VALUES (?)" (SQ.Only game)
  traverse
    (\(action, move) -> insertAction conn game move action)
    (zip actions [0 ..])
  return game

loadGame :: Int -> SQ.Connection -> IO [Action]
loadGame game conn = do
  buildRet <-
    (SQ.query
       conn
       "SELECT move,corp,hex_row,hex_col FROM build_actions WHERE game = ?"
       (SQ.Only game))
  let builds =
        map
          (\(move, corp, hex_row, hex_col) ->
             ( move :: Int
             , ABuild
                 (Build
                  {_buildCorp = intToCorp corp, _buildHex = (hex_row, hex_col)})))
          buildRet
  stopBuildRet <-
    (SQ.query
       conn
       "SELECT move FROM stop_build_actions WHERE game = ?"
       (SQ.Only game))
  let stopBuilds =
        map (\(SQ.Only move) -> (move :: Int, StopBuild)) stopBuildRet
  tradeRet <-
    (SQ.query
       conn
       "SELECT move,num,sell,buy FROM trade_actions WHERE game = ?"
       (SQ.Only game))
  let trades =
        map
          (\(move, num, sell, buy) ->
             ( move :: Int
             , ATrade
                 (Trade
                  { _tNum = intToTNum num
                  , _sell = intToCorp sell
                  , _buy = intToCorp buy
                  })))
          tradeRet
  takeInitialShareRet <-
    (SQ.query
       conn
       "SELECT move,yellow,blue,brown,purple,black,red FROM take_initial_share_actions WHERE game = ?"
       (SQ.Only game))
  let takeInitialShares =
        map
          (\((SQ.Only move) SQ.:. holding) ->
             (move :: Int, ATakeInitialShare (tupleToHolding holding)))
          takeInitialShareRet
  return
    ((takeInitialShares ++ builds ++ stopBuilds ++ trades) & List.sortOn fst &
     map snd)
