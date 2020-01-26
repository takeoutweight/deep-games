{-# LANGUAGE TemplateHaskell #-}

module UCT where

import Control.Lens ((%~), (&), (.~), (^.))
import qualified Control.Lens as Lens
import qualified Control.Monad.Random as Random
import qualified Data.List as List
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

data SearchLogic m act gs winVec = SearchLogic
  { _evalNode :: !(gs -> m winVec) -- random sample of win rates for all players
                      -- in the state. (likely just eg "1.0 for a player won on
                      -- a random playout from here, 0.0 for everyone else")
  , _favourability :: !(gs -> winVec -> Double) -- what is the active player's win rate?
  , _legalMoves :: !(gs -> [act])
  , _execMove :: !(act -> gs -> gs)
  }

Lens.makeLenses ''GameTree
Lens.makeLenses ''NodeState

-- pick uniformly-distributed random moves until no moves are possible
uniformRandomPlayout ::
     Random.MonadRandom m
  => Int
  -> (gs -> [act])
  -> (act -> gs -> gs)
  -> gs
  -> m gs
uniformRandomPlayout timeout legalMoves execMove gs =
  let moves = legalMoves gs
  in case (null moves) || (timeout <= 0) of
       True -> return gs
       False -> do
         move <- Random.uniform moves
         uniformRandomPlayout
           (timeout - 1)
           legalMoves
           execMove
           (execMove move gs)

playout ::
     (Random.MonadRandom m, Ord act)
  => Double
  -> SearchLogic m act gs winVec
  -> gs
  -> NodeState act
  -> m (winVec, NodeState act)
playout explore logic gs nstate =
  let evalNode = (_evalNode logic)
      favourability = (_favourability logic)
      legalMoves = (_legalMoves logic)
      execMove = (_execMove logic)
      moveProbs =
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
                          (playout explore logic (execMove act gs) childState)
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

-- A hack to avoid the case where we have a bunch of 1.0 propability moves and
-- pick one with a lower explore count.  The right math solution would probably
-- be some approximation of credibility interval and, if we're pessimisitic,
-- chooseing the best 25% confidence interval estimated win rate.
bestMoveEpsilon = 1 / 50

-- The best sequence of moves through the given tree
bestLine :: NodeState act -> [act]
bestLine nstate =
  case Map.null (_moves (_children nstate)) of
    True -> []
    False ->
      let moves0 =
            Map.toList (_moves (_children nstate)) &
            (List.sortOn
               (\(act, node) -> (_wins node) / (fromIntegral (_visited node)))) &
            reverse
          bestNode = moves0 & last & snd
          moves1 =
            moves0 &
            (filter
               (\(act, node) ->
                  ((_wins node) / (fromIntegral (_visited node))) >=
                  (((_wins bestNode) / (fromIntegral (_visited bestNode))) -
                   bestMoveEpsilon)))
          (veryBestAct, veryBestNode) =
            moves1 & (List.sortOn (\(act, node) -> (_visited node))) & last
      in veryBestAct : (bestLine veryBestNode)

defaultExploreRate = 1.0

iterateUTC ::
     (Random.MonadRandom m, Ord act)
  => Int
  -> SearchLogic m act gs winVec
  -> gs
  -> NodeState act
  -> m (NodeState act)
iterateUTC 0 logic initState nState = return nState
iterateUTC iterations logic initState nState = do
  (win, state') <- (playout defaultExploreRate logic initState nState)
  (iterateUTC (iterations - 1) logic initState state')

initNodeState =
  (NodeState
   {_visited = 0, _wins = 0, _children = GameTree {_moves = Map.empty}})

randomGameViaUTC ::
     (Random.MonadRandom m, Ord act)
  => Int
  -> SearchLogic m act gs winVec
  -> gs
  -> m [act]
randomGameViaUTC iterations logic initGameState = do
  let execMove = (_execMove logic)
      pick gs ns = do
        nstate' <- (iterateUTC iterations logic gs ns)
        case bestLine nstate' of
          [] -> return []
          (action:_) -> do
            rst <-
              (pick
                 (execMove action gs)
                 ((_moves (_children nstate')) Map.! action))
            return (action : rst)
  line <- (pick initGameState initNodeState)
  return line
