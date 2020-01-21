{-# LANGUAGE TemplateHaskell #-}

module UCT where

import Control.Lens ((%~), (&), (.~), (^.))
import qualified Control.Lens as Lens
import qualified Control.Monad.Random as Random
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map(..))

-- Note, we don't store the game state. Experiment how storing it affects speed
-- the children represent the game state reflecting the execution of the action
-- of the key at which they are stored.
data NodeState act = NodeState
  { _visited :: !Int -- number of visits at this node.
  , _wins :: !Double -- sum of victories for "me" so far. (i.e. play out children, and sum up how good each playout is for this position).
  , _children :: !(GameTree act)
  } deriving (Show)

data GameTree act = GameTree
  { _moves :: !(Map act (NodeState act))
  } deriving (Show)

Lens.makeLenses ''GameTree
Lens.makeLenses ''NodeState

playout ::
     (Random.MonadRandom m, Ord act)
  => Double
  -> (gs -> m winVec) -- random sample of win rates for all players in the
                      -- state. (likely just eg "1.0 for a player won on a
                      -- random playout from here, 0.0 for everyone else")
  -> (gs -> winVec -> Double) -- what is the active player's win rate?
  -> (gs -> [act])
  -> (act -> gs -> gs)
  -> gs
  -> NodeState act
  -> m (winVec, NodeState act)
playout explore evalNode favourability legalMoves execMove gs nstate =
  let moveProbs =
        legalMoves gs &
        map
          (\act ->
             case Map.lookup act (_moves (_children nstate)) of
               Nothing ->
                 ( ( act
                   , (NodeState
                      { _visited = 0
                      , _wins = 0
                      , _children = GameTree {_moves = Map.empty}
                      }))
                 , (fromIntegral
                      (round
                         (100000 *
                          (explore *
                           (sqrt (log (fromIntegral ((_visited nstate) + 2)))))))) :: Rational)
               Just (nstate'@(NodeState visited wins children)) ->
                 ( (act, nstate')
                 , (fromIntegral
                      (round
                         (100000 *
                          ((wins / fromIntegral visited) +
                           explore *
                           (sqrt (log (fromIntegral ((_visited nstate) + 2))) /
                            (fromIntegral visited)))))) :: Rational))
  in case moveProbs of
       [] -> do
         winVec <- evalNode gs
         return
           ( winVec
           , nstate & visited %~ (+ 1) & wins %~ (+ (favourability gs winVec)))
       _ -> do
         (action, childState) <- Random.fromList moveProbs
         (winVec, childState') <-
           (playout
              explore
              evalNode
              favourability
              legalMoves
              execMove
              (execMove action gs)
              childState)
         return
           ( winVec
           , (nstate & (children . moves) %~
              (Map.insert
                 action
                 (childState' & (visited %~ (+ 1)) &
                  (wins %~ (+ (favourability gs winVec)))))))
