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
  , _wins :: !Double -- sum of victories for "me" so far. (i.e. play out possible moves, and sum up how good each playout is for this parent game state).
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
                   , do winVec <- evalNode (execMove act gs)
                        return
                          ( winVec
                          , (NodeState
                             { _visited = 1
                             , _wins = (favourability gs winVec)
                             , _children = GameTree {_moves = Map.empty}
                             })))
                 , (fromIntegral
                      (round
                         (100000 *
                          (explore *
                           (sqrt (log (fromIntegral ((_visited nstate) + 2)))))))) :: Rational)
               Just (childState@(NodeState v w c)) ->
                 ( ( act
                   , do (winVec, childState') <-
                          (playout
                             explore
                             evalNode
                             favourability
                             legalMoves
                             execMove
                             (execMove act gs)
                             childState)
                        return
                          ( winVec
                          , childState' & (visited %~ (+ 1)) &
                            (wins %~ (+ (favourability gs winVec)))))
                 , (fromIntegral
                      (round
                         (100000 *
                          ((w / fromIntegral v) +
                           explore *
                           (sqrt (log (fromIntegral ((_visited nstate) + 2))) /
                            (fromIntegral v)))))) :: Rational))
  in case moveProbs of
       [] -> do
         winVec <- evalNode gs
         return (winVec, nstate)
       _ -> do
         (action, childAct) <- Random.fromList moveProbs
         (winVec, childState) <- childAct
         return
           ( winVec
           , (nstate & (children . moves) %~ (Map.insert action childState)))
