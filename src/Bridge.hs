{-# LANGUAGE TemplateHaskell #-}
module Bridge where

import qualified Control.Lens as Lens
import Control.Lens ((%~), (&), (.~), (^.))
import qualified Control.Monad.Random as Random
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set(..))
import qualified System.Random.Shuffle as Shuffle

import qualified UCT as UCT

data Hand
  = North
  | East
  | South
  | West
  deriving (Enum, Bounded, Show)
data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Enum, Bounded, Eq, Ord, Show)
type Rank = Int
data Card =
  Card Suit
       Rank
  deriving (Eq, Ord, Show)

next :: (Enum a, Bounded a) => a -> a
next = turn 1

prev :: (Enum a, Bounded a) => a -> a
prev = turn (-1)

turn :: (Enum a, Bounded a) => Int -> a -> a
turn n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
  where
    add mod x y = (x + y + mod) `rem` mod


data GameState = GameState
  { _toPlay :: !Hand
  , _played :: ![Card] -- first in the list is lead card
  , _northHand :: !(Set Card)
  , _eastHand :: !(Set Card)
  , _southHand :: !(Set Card)
  , _westHand :: !(Set Card)
  , _trump :: !Suit
  , _northSouthWonTricks :: !Int
  , _eastWestWonTricks :: !Int
  } deriving Show

Lens.makeLenses ''GameState

handLens North = northHand
handLens South = southHand
handLens East = eastHand
handLens West = westHand

data Action = PlayCard !Card deriving Show

legalMoves :: GameState -> [Action]
legalMoves gs =
  let leadHand = (Set.toList (gs ^. (handLens (_toPlay gs))))
  in case (_played gs) of
       [] -> map PlayCard leadHand
       (Card leadSuit _):rst ->
         case filter (\card@(Card suit _) -> suit == leadSuit) leadHand of
           [] -> map PlayCard leadHand
           suited -> map PlayCard suited

execMove :: Action -> GameState -> GameState
execMove (PlayCard card) gs =
  let gs' = gs & handLens (_toPlay gs) %~ Set.delete card & toPlay %~ next
  in case (_played gs) of
       [a@(Card leadSuit _), b, c] ->
         let trick =
               [ (a, turn (-3) (_toPlay gs))
               , (b, turn (-2) (_toPlay gs))
               , (c, turn (-1) (_toPlay gs))
               , (card, (_toPlay gs))
               ]
             trump =
               trick & filter (\(a@((Card suit _), _)) -> suit == (_trump gs))
             suited =
               trick & filter (\(a@((Card suit _), _)) -> suit == leadSuit)
             (_, winner):_ =
               (trump & List.sortOn (\(a@((Card suit rank), _)) -> rank)) ++
               (suited & List.sortOn (\(a@((Card suit rank), _)) -> rank))
             winningTeam =
               case winner of
                 North -> northSouthWonTricks
                 South -> northSouthWonTricks
                 East -> eastWestWonTricks
                 West -> eastWestWonTricks
         in gs' & toPlay .~ winner & played .~ [] & winningTeam %~ (+ 1)
       _ -> gs' & played %~ (++ [card])

randomDeal :: Random.MonadRandom m => m GameState
randomDeal = do
  deck <-
    [Card suit rank | suit <- enumFrom Clubs, rank <- [1 .. 13]] &
    Shuffle.shuffleM
  let (northH, rst1) = List.splitAt 13 deck
  let (eastH, rst2) = List.splitAt 13 rst1
  let (southH, rst3) = List.splitAt 13 rst2
  let (westH, _) = List.splitAt 13 rst3
  return
    GameState
    { _toPlay = North
    , _played = []
    , _northHand = Set.fromList northH
    , _eastHand = Set.fromList eastH
    , _southHand = Set.fromList southH
    , _westHand = Set.fromList westH
    , _trump = Hearts
    , _northSouthWonTricks = 0
    , _eastWestWonTricks = 0
    }
