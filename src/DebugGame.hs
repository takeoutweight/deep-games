{-# LANGUAGE TemplateHaskell #-}
module DebugGame where

import qualified Control.Lens as Lens
import Control.Lens ((%~), (&), (.~), (^.))
import qualified Control.Monad.Random as Random
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Map.Strict (Map(..))

import qualified UCT as UCT

data GameState = GameState
  { _activePlayer :: !Int
  , _moves :: !Int
  , _scores :: !(Map Int Int)
  } deriving Show

Lens.makeLenses ''GameState

type Action = Int

numPlayers = 3
numMoves = 3 * 10
moveChoices = 10

legalMoves :: GameState -> [Action]
legalMoves gs =
  case (_moves gs) >= numMoves of
    True -> []
    False -> [0 .. moveChoices - 1]

execMove :: Action -> GameState -> GameState
execMove act gs =
  gs & (activePlayer %~ (`mod` numPlayers) . (+ 1)) &
  (scores %~
   Map.alter
     (\v ->
        case v of
          Just v -> Just (v + act)
          Nothing -> Just act)
     (_activePlayer gs)) &
  (moves %~ (+ 1))

getScore gs player =
  case (gs & _scores & Map.lookup player) of
    Just v -> v
    Nothing -> 0

-- I.e. just trying to get the highest score you can
favourabilityProportional :: GameState -> GameState -> Double
favourabilityProportional gs fs =
  case (fs & _scores & Map.lookup (_activePlayer gs)) of
    Just v -> fromIntegral v
    Nothing -> 0

-- Just beat your opponents
favourabilityBinary :: GameState -> GameState -> Double
favourabilityBinary gs fs =
  case (getScore fs (_activePlayer gs)) ==
       ([0 .. numPlayers] & map (getScore fs) & List.foldl' max 0) of
    True -> 1
    False -> 0

evalNode :: Random.MonadRandom m => GameState -> m GameState
evalNode = UCT.uniformRandomPlayout 5000 legalMoves execMove

initGS = GameState {_activePlayer = 0, _moves = 0, _scores = Map.empty}

debugGameLogic :: (Random.MonadRandom m) => UCT.SearchLogic m Action GameState GameState
debugGameLogic =
  UCT.SearchLogic
  { UCT._evalNode = evalNode
  , UCT._favourability = favourabilityBinary
  , UCT._legalMoves = legalMoves
  , UCT._execMove = execMove
  }

-- ns <- (UCT.iterateUTC 20000 debugGameLogic initGS UCT.initNodeState)
-- UCT.bestLine ns
