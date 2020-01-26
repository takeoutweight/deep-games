{-# LANGUAGE TemplateHaskell #-}
module Bridge where

import qualified Control.Lens as Lens
import Control.Lens ((%~), (&), (.~), (^.))
import qualified Control.Monad.Random as Random
import qualified Data.List as List
import qualified Data.Maybe as Maybe
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
type Rank = Int -- Maybe confusing but 1 = 2 .. 12 = K, 13 = A
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

teamTricks North = northSouthWonTricks
teamTricks South = northSouthWonTricks
teamTricks East = eastWestWonTricks
teamTricks West = eastWestWonTricks

suitIcon Clubs = "\9827"
suitIcon Diamonds = "\9830"
suitIcon Hearts = "\9829"
suitIcon Spades = "\9824"

parseSuit '\9827' = Clubs
parseSuit '\9830' = Diamonds
parseSuit '\9829' = Hearts
parseSuit '\9824' = Spades

showRank 13 = "A"
showRank 12 = "K"
showRank 11 = "Q"
showRank 10 = "J"
showRank 9 = "T"
showRank x = show (x + 1)

parseRank 'A' = 13
parseRank 'K' = 12
parseRank 'Q' = 11
parseRank 'J' = 10
parseRank 'T' = 9
-- parseRank x = undefined

showHand hand =
  [ hand & Set.toList &
  Maybe.mapMaybe
    (\(Card csuit rank) ->
       case (suit == csuit) of
         True -> Just rank
         False -> Nothing) &
  List.sort &
  reverse &
  map showRank &
  concat &
  ((suitIcon suit) ++)
  | suit <- [Clubs ..] & reverse
  ] &
  List.intercalate " "

printHands gs =
  [show hand ++ ": " ++ showHand (gs ^. handLens hand) | hand <- [North ..]] &
  List.intercalate "\n" &
  putStrLn

printPlay acts =
  acts &
  map (\(PlayCard (Card suit rank)) -> (showRank rank) ++ (suitIcon suit)) &
  List.intercalate " " &
  putStrLn

data Action =
  PlayCard !Card
  deriving (Show, Eq, Ord)

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
               (trump & List.sortOn (\(a@((Card suit rank), _)) -> rank) &
                reverse) ++
               (suited & List.sortOn (\(a@((Card suit rank), _)) -> rank) &
                reverse)
         in gs' & toPlay .~ winner & played .~ [] & (teamTricks winner) %~ (+ 1)
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

evalNode :: Random.MonadRandom m => GameState -> m GameState
evalNode = UCT.uniformRandomPlayout 5000 legalMoves execMove

favourability :: GameState -> GameState -> Double
favourability gs fs = fs ^. (teamTricks (_toPlay gs)) & fromIntegral

bridgeLogic ::
     (Random.MonadRandom m) => UCT.SearchLogic m Action GameState GameState
bridgeLogic =
  UCT.SearchLogic
  { UCT._evalNode = evalNode
  , UCT._favourability = favourability
  , UCT._legalMoves = legalMoves
  , UCT._execMove = execMove
  }

-- Getting an abysmal ~1000k iterations/s
-- d1 <- randomDeal
-- ns <- (UCT.iterateUTC 1000 bridgeLogic d1 UCT.initNodeState)
-- ns <- (UCT.iterateUTC 1000 bridgeLogic (d1 & execMove (PlayCard (Card Clubs 1))) UCT.initNodeState)
-- printPlay (UCT.bestLine ns)
-- game <- UCT.randomGameViaUTC 5 bridgeLogic d1
