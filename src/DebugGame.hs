{-# LANGUAGE TemplateHaskell #-}
module DebugGame where

import qualified Control.Lens as Lens
import Control.Lens ((%~), (&), (.~), (^.))
import qualified Control.Monad.Random as Random
import qualified Data.Map.Strict as Map
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
  gs & (activePlayer %~ \p -> (p + 1) `mod` numPlayers) &
  (scores %~
   Map.alter
     (\v ->
        case v of
          Just v -> Just (v + 1)
          Nothing -> Just 1)
     (_activePlayer gs)) &
  (moves %~ (+ 1))

favourability :: GameState -> GameState -> Double
favourability gs fs = case (fs & _scores & Map.lookup (_activePlayer gs)) of
  Just v -> fromIntegral 0
  Nothing -> 0

evalNode :: Random.MonadRandom m => GameState -> m GameState
evalNode = UCT.uniformRandomPlayout 5000 legalMoves execMove

debugGameLogic :: (Random.MonadRandom m) => UCT.SearchLogic m Action GameState GameState
debugGameLogic =
  UCT.SearchLogic
  { UCT._evalNode = evalNode
  , UCT._favourability = favourability
  , UCT._legalMoves = legalMoves
  , UCT._execMove = execMove
  }
