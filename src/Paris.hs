{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Paris where

import Control.Lens ((%~), (&), (.~), (^.))
import qualified Control.Lens as Lens
import Control.Monad.IO.Class as CMIO
import Control.Monad (guard)
import qualified Control.Monad.Random as Random
import qualified Control.Monad.Reader as CMR
import qualified Data.Array as Array
import Data.Array (Array)
import qualified Data.Colour.SRGB as Col
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Map.Strict (Map(..))
import qualified Data.Set as Set
import Data.Set (Set(..))
import Data.Text (Text)
import Data.Typeable
import qualified Diagrams.Backend.SVG as SVG
import qualified Diagrams.Backend.Postscript as PS
-- import qualified Diagrams.Backend.AbstractSVG as ASVG
-- import qualified Diagrams.Backend.DOM as DBDOM
import qualified Diagrams.Prelude as D
import Diagrams.Prelude (Diagram(..), (<>), (@@))
import qualified Diagrams.TwoD.Vector as DV
import qualified Language.Javascript.JSaddle.WKWebView as WKWV
import qualified Language.Javascript.JSaddle.Warp as JSWarp
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOMD
import qualified GHCJS.DOM.Element as DOME
import qualified GHCJS.DOM.Node as DOMN
import qualified Control.Concurrent.MVar as MVar
import qualified System.Random.Shuffle as Shuffle

-- hex grid is flats-on-top, diamond-grid with origin on top.  row,column, with
-- rows pointed SE (i.e. idx increasing SW), columns pointing SW (i.e. idx
-- increasing SE)
type PCMap = Array (Int, Int) Tile

-- how far until the first nonempty hex, (i.e. column idx) for each row:
nwOffset =
  [ 3
  , 1
  , 1
  , 2
  , 2
  , 2
  , 2
  , 0 -- Cherborg
  , 1
  , 2
  , 3
  , 2
  , 1
  , 1 -- Brast
  , 3
  , 7
  , 8
  , 11
  , 12
  , 13
  , 13
  , 14
  , 15
  , 16
  ]

-- column idx of the last nonempty hex
seOffset =
  [ 12
  , 13
  , 13
  , 14
  , 16
  , 15
  , 16
  , 16
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 25
  , 25
  , 26
  , 26
  , 23
  , 22
  , 22
  , 23
  , 22
  , 20
  ]

mapGet :: (Ord k, Ord a) => k -> Map k (Set a) -> (Set a)
mapGet k m =
  case Map.lookup k m of
    Just v -> v
    Nothing -> Set.empty

mapInsert :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
mapInsert k v m =
  Map.alter
    (\ov ->
       case ov of
         Just ov -> Just (Set.insert v ov)
         Nothing -> Just (Set.singleton v))
    k
    m

data Tile = Empty | Rural | OneTown | TwoTown | ThreeTown | FourTown

data Town = Town
  { name :: !String
  , value :: !Int
  } deriving (Show)

data Corp
  = Yellow
  | Blue
  | Brown
  | Purple
  | Black
  | Red
  deriving (Show, Eq, Ord)

allCorps = [Yellow, Blue, Brown, Purple, Black, Red]

corpColor Yellow = (Col.sRGB24read "#009500")
corpColor Blue = (Col.sRGB24read "#00bbff")
corpColor Brown = (Col.sRGB24read "#7a584c")
corpColor Purple = (Col.sRGB24read "#d963ff")
corpColor Black = (Col.sRGB24read "#131414")
corpColor Red = (Col.sRGB24read "#c30000")

type Hex = (Integer, Integer)

type BoardState = Map Hex (Set Corp)

data Holding = Holding
  { _yellow :: !Int
  , _blue :: !Int
  , _brown :: !Int
  , _purple :: !Int
  , _black :: !Int
  , _red :: !Int
  } deriving (Show)

Lens.makeLenses ''Holding

corpLens Yellow = yellow
corpLens Blue = blue
corpLens Brown = brown
corpLens Purple = purple
corpLens Black = black
corpLens Red = red

data PlayerHolding = PlayerHolding
  { _public :: !Holding
  , _private :: !Holding
  } deriving (Show)

Lens.makeLenses ''PlayerHolding

data GameState = GameState
  { _activePlayer :: !Int
  , _activeBuildCount :: !Int
  , _activeBuildCorp :: !(Maybe Corp)
  , _emptyCorps :: !Int
  , _numPlayers :: !Int
  , _numMoves :: !Int
  , _boardState :: !BoardState
  , _reserve :: !Holding
  , _playerHoldings :: Map Int PlayerHolding
  } deriving (Show)

Lens.makeLenses ''GameState

data Build = Build
  { _buildCorp :: !Corp
  , _buildHex :: !Hex
  } deriving (Show)

Lens.makeLenses ''Build

data TNum
  = TOne
  | TTwo
  deriving (Show)

data Trade = Trade
  { _tNum :: !TNum
  , _sell :: !Corp
  , _buy :: !Corp
  } deriving (Show)

Lens.makeLenses ''Trade

data Action
  = ABuild !Build
  | StopBuild
  | ATrade !Trade
  | ATakeInitialShare !Holding
  deriving (Show)

subtractReserve :: Corp -> Int -> GameState -> GameState
subtractReserve corp num gs =
  let gs' = gs & ((reserve . (corpLens corp)) %~ (subtract num))
  in case gs' & Lens.view (reserve . (corpLens corp)) & (<= 0) of
       True -> gs' & emptyCorps %~ (+ 1)
       False -> gs'

addReserve :: Corp -> GameState -> GameState
addReserve corp gs =
  let gs' = gs & ((reserve . (corpLens corp)) %~ (+ 1))
  -- intentionally viewing pre-mutation game state:
  in case gs & Lens.view (reserve . (corpLens corp)) & (== 0) of
       True -> gs' & emptyCorps %~ (subtract 1)
       False -> gs'

execBuild :: Build -> GameState -> GameState
execBuild (Build {_buildCorp = corp, _buildHex = hex}) gameState =
  gameState & (boardState %~ mapInsert hex corp) & subtractReserve corp 1 &
  (activeBuildCount %~ (+ 1)) &
  (activeBuildCorp .~ Just corp)

rotatePlayer :: GameState -> GameState
rotatePlayer gameState =
  gameState & (activePlayer %~ ((`mod` (_numPlayers gameState)) . (+ 1))) &
  (numMoves %~ (+ 1))

execStopBuild :: GameState -> GameState
execStopBuild gameState =
  gameState & activeBuildCount .~ 0 & activeBuildCorp .~ Nothing & rotatePlayer

takePrivateShare corp gameState =
  gameState & subtractReserve corp 1 &
  ((playerHoldings .
    (Lens.ix (_activePlayer gameState)) . private . (corpLens corp)) %~
   (+ 1))

takeShare corp num gameState =
  gameState & subtractReserve corp num &
  ((playerHoldings .
    (Lens.ix (_activePlayer gameState)) . public . (corpLens corp)) %~
   (+ num))

giveShare corp gameState =
  gameState & addReserve corp &
  ((playerHoldings .
    (Lens.ix (_activePlayer gameState)) .
    (if (gameState & _playerHoldings & Map.lookup (_activePlayer gameState) &
         Maybe.fromJust &
         _public &
         (Lens.view (corpLens corp))) >
        0
       then public
       else private) .
    (corpLens corp)) %~
   (subtract 1))

execTrade Trade {_tNum = tnum, _sell = sell, _buy = buy} gameState =
  gameState &
  takeShare
    buy
    (case tnum of
       TOne -> 1
       TTwo -> 2) &
  giveShare sell &
  rotatePlayer

execTakeInitialShare :: Holding -> GameState -> GameState
execTakeInitialShare h gs =
  gs & reserve %~ (\rs -> (opHolding (-) rs h)) &
  (playerHoldings . (Lens.ix (_activePlayer gs)) . private) %~
  (opHolding (+) h) & rotatePlayer

divvyStartingShares :: Random.MonadRandom m => GameState -> m [Action]
divvyStartingShares gs = do
  deck <- (allCorps & map (replicate 31) & concat & Shuffle.shuffleM)
  let takeNum =
        case (_numPlayers gs) of
          3 -> 10
          4 -> 8
          5 -> 6
          6 -> 5
  let (_, actions') =
        (take (_numPlayers gs) [0 ..]) &
        List.foldl'
          (\(deck', actions) player ->
             let (mine, restDeck) = List.splitAt takeNum deck'
                 action =
                   ATakeInitialShare
                     (List.foldl'
                        (\holding corp -> holding & corpLens corp %~ (+ 1))
                        (defaultHolding 0)
                        mine)
             in (restDeck, action : actions))
          (deck, [])
  return actions'

execMove :: Action -> GameState -> GameState
execMove (ABuild a) = execBuild a
execMove (StopBuild) = execStopBuild
execMove (ATrade t) = execTrade t
execMove (ATakeInitialShare h) = execTakeInitialShare h

  

-- not including the center hex
neighbours (r, c) =
  [ (r - 1, c - 1)
  , (r - 1, c)
  , (r, c - 1)
  , (r, c + 1)
  , (r + 1, c)
  , (r + 1, c + 1)
  ]

marseilleHex = (17, 25)

timeout = 500

gameOver :: GameState -> Bool
gameOver gs =
  ((_numMoves gs) > timeout) ||
  (gs & _boardState & Map.lookup marseilleHex & Maybe.isJust) || -- assumes we don't ever have Set.empty
  (_emptyCorps gs >= 5)

legalMoves :: GameState -> [Action]
legalMoves gs =
  case gameOver gs of
    True -> []
    False ->
      ((++)
         (do guard ((_activeBuildCount gs) < 5)
             (cHex, cCorps) <- (_boardState gs) & Map.toList
             corp <- Set.toList cCorps
             guard ((gs & _reserve & (Lens.view (corpLens corp))) >= 1)
             guard
               (case (_activeBuildCorp gs) of
                  Just abc -> abc == corp
                  Nothing -> True)
             nhex <- neighbours cHex
             guard
               (case (_boardState gs) & mapGet nhex & Set.toList of
                  [] -> True
                  [c] -> c /= corp
                  _ -> False)
             guard ((not . startHex) nhex)
             return (ABuild (Build corp nhex)))
         (case (_activeBuildCount gs) of
            0 ->
              (do let maxCount =
                        case (_numPlayers gs) of
                          3 -> 20
                          4 -> 15
                          5 -> 12
                          6 -> 10
                  sellCorp <- allCorps
                  let ph = gs & _playerHoldings & (Map.! (_activePlayer gs))
                  guard
                    (((ph & _public & (Lens.view (corpLens sellCorp))) +
                      (ph & _private & (Lens.view (corpLens sellCorp)))) >
                     0)
                  buyCorp <- allCorps
                  guard (buyCorp /= sellCorp)
                  let ownedCount =
                        ((ph & _public & (Lens.view (corpLens buyCorp))) +
                         (ph & _private & (Lens.view (corpLens buyCorp))))
                  let resCount =
                        (gs & _reserve & (Lens.view (corpLens buyCorp)))
                  ((case ((resCount >= 1) && ((ownedCount + 1) <= maxCount)) of
                      True -> [ATrade (Trade TOne sellCorp buyCorp)]
                      _ -> []) ++
                   (case ((resCount >= 2) && ((ownedCount + 2) <= maxCount)) of
                      True -> [ATrade (Trade TTwo sellCorp buyCorp)]
                      _ -> [])))
            _ -> [StopBuild]))

defaultHolding x =
  Holding
  {_yellow = x, _blue = x, _brown = x, _purple = x, _black = x, _red = x}

defaultPlayerHolding = PlayerHolding (defaultHolding 0) (defaultHolding 0)

randomGame gs =
  let moves = legalMoves gs
  in case null moves of
       True -> return gs
       False -> do
         move <- Random.uniform moves
         randomGame (execMove move gs)

startHex hex =
  case hex of
    (6, 7) -> True
    (6, 8) -> True
    (7, 7) -> True
    (7, 8) -> True -- Paris
    (7, 9) -> True
    (8, 8) -> True
    (8, 9) -> True
    _ -> False

defaultState =
  (GameState
   { _activePlayer = 0
   , _activeBuildCount = 0
   , _activeBuildCorp = Nothing
   , _emptyCorps = 0
   , _numPlayers = 3
   , _numMoves = 0
   , _boardState =
       Map.fromList
         [ ((6, 7), Set.singleton Brown)
         , ((6, 8), Set.singleton Purple)
         , ((7, 7), Set.singleton Blue)
         , ((7, 9), Set.singleton Black)
         , ((8, 8), Set.singleton Yellow)
         , ((8, 9), Set.singleton Red)
         ]
   , _reserve = defaultHolding 31
   , _playerHoldings =
       Map.fromList
         [ (0, defaultPlayerHolding)
         , (1, defaultPlayerHolding)
         , (2, defaultPlayerHolding)
         ]
   })

someState = defaultState & execBuild (Build Yellow (6, 7))

cities =
  Map.fromList
    [ ((0, 3), Town "Lille" 3)
    , ((0, 5), Town "Mons" 1)
    , ((0, 12), Town "Wissembourg" 1)
    , ((1, 1), Town "Dunkerque" 2)
    , ((1, 8), Town "Sedan" 1)
    , ((1, 10), Town "Metz" 1)
    , ((1, 13), Town "Strasbourg" 3)
    , ((2, 1), Town "Calais" 1)
    , ((3, 8), Town "Reims" 1)
    , ((3, 11), Town "Nancy" 1)
    , ((4, 2), Town "Dieppe" 1)
    , ((4, 5), Town "Amiens" 1)
    , ((4, 15), Town "Mulhouse" 1)
    , ((4, 16), Town "Basel" 2)
    , ((6, 2), Town "Le Havre" 2)
    , ((6, 4), Town "Rouen" 1)
    , ((6, 10), Town "Troyes" 1)
    , ((6, 12), Town "Chaumont" 1)
    , ((6, 15), Town "Besançon" 1)
    , ((7, 0), Town "Cherbourg" 2)
    , ((7, 3), Town "Caen" 1)
    , ((7, 6), Town "Mantes-la-Jolie" 1)
    , ((7, 8), Town "Paris" 1)
    , ((8, 11), Town "Laroche" 1) -- not a town?
    , ((8, 14), Town "Dijon" 1)
    , ((8, 18), Town "Genève" 2)
    , ((9, 3), Town "St. Lô" 1)
    , ((9, 8), Town "Châtres" 1)
    , ((10, 7), Town "Le Mans" 1)
    , ((10, 10), Town "Orléans" 1) -- looked for accents up to here
    , ((10, 16), Town "Macons" 1)
    , ((10, 18), Town "Culot" 1)
    , ((11, 4), Town "Rennes" 1)
    , ((11, 9), Town "Tours" 1)
    , ((11, 14), Town "Moulins" 1)
    , ((11, 21), Town "St. Jean-de-Marrienne" 1)
    , ((12, 7), Town "Angers" 1)
    , ((12, 12), Town "Chateauroux" 1)
    , ((12, 15), Town "Vichy" 1)
    , ((12, 18), Town "Lyon" 3)
    , ((12, 20), Town "Grenoble" 1)
    , ((13, 1), Town "Brest" 3)
    , ((13, 7), Town "Nantes" 1)
    , ((13, 18), Town "St. Etienne" 1)
    , ((14, 4), Town "Lorient" 2)
    , ((14, 6), Town "St. Nazaire" 1)
    , ((14, 11), Town "Poitiers" 1)
    , ((14, 16), Town "Clerment-Ferrand" 2)
    , ((14, 20), Town "Valence" 1)
    , ((14, 25), Town "Nice" 1)
    , ((15, 14), Town "Limoges" 1)
    , ((16, 10), Town "La Rochelle" 2)
    , ((16, 22), Town "Avignon" 1)
    , ((17, 11), Town "Rochefort" 1)
    , ((17, 13), Town "Angouleme" 1)
    , ((17, 15), Town "Periguex" 1)
    , ((17, 22), Town "Nimes" 1)
    , (marseilleHex, Town "Marseille" 4) -- (17, 25)
    , ((17, 26), Town "Toulon" 1)
    , ((18, 18), Town "Capdenac" 1)
    , ((19, 14), Town "Bordeaux" 3)
    , ((19, 22), Town "Beziers" 1)
    , ((20, 13), Town "Arcachon" 1)
    , ((20, 17), Town "Montauban" 1)
    , ((20, 19), Town "Toulouse" 2)
    , ((20, 22), Town "Narbonne" 1)
    , ((21, 15), Town "Morcenx" 1)
    , ((22, 18), Town "Tarbes" 1)
    , ((23, 16), Town "Bayonne" 2)
    ]

edgeToEdge :: Floating a => a
edgeToEdge = 1

centerToPointRadius :: Floating a => a
centerToPointRadius = (1 / 2) * (edgeToEdge / (cos ((1 / 12) * D.tau)))

-- I.e. same row, increasing column idx
dirSE = DV.e ((-1 / 12) @@ D.turn)

-- I.e. same column, increasing row idx
dirSW = DV.e ((-5 / 12) @@ D.turn)

theMap :: PCDiag
theMap =
  (zip [0,1 ..] (zip nwOffset seOffset)) &
  map
    (\(row, (startCol, endCol)) ->
       [startCol .. endCol] &
       map
         (\col ->
            ((case Map.lookup (row, col) cities of
                Just (Town name value) ->
                  ((D.text name & D.font "Fira Sans Compressed" &
                    D.fontSize
                      (if (length name) <= 10
                         then 4.1
                         else 3.4)) <>
                   ((D.hexagon centerToPointRadius) & D.rotateBy (0 / 12) &
                    D.fc
                      (case value of
                         1 -> (Col.sRGB24 255 242 0)
                         2 -> (Col.sRGB24 199 191 230)
                         3 -> (Col.sRGB24 237 27 37)
                         4 -> (Col.sRGB24 63 71 205))))
                Nothing ->
                  (((D.hexagon centerToPointRadius) & D.rotateBy (0 / 12) &
                    D.fc (Col.sRGB24 239 244 211))))) &
            D.href ("idx/" ++ show row ++ "," ++ show col) &
            D.translate
              (((fromIntegral row) * dirSW) + ((fromIntegral col) * dirSE)))) &
  mconcat &
  mconcat

drawState :: GameState -> PCDiag
drawState gs =
  (_boardState gs) & Map.toList &
  map
    (\((row, col), cCorps) ->
       (case (Set.toList cCorps) of
          [] -> mempty
          [corp] -> D.square 0.3 & D.fc (corpColor corp) & D.lineWidth 0.4
          [corpA, corpB] ->
            (D.hsep
               0.1
               [ D.square 0.3 & D.fc (corpColor corpA) & D.lineWidth 0.4
               , D.square 0.3 & D.fc (corpColor corpB) & D.lineWidth 0.4
               ]) &
            D.centerX
          _ -> mempty) &
       D.translate (((fromIntegral row) * dirSW) + ((fromIntegral col) * dirSE))) &
  mconcat

corpValues :: GameState -> Holding
corpValues gs =
  Map.foldlWithKey'
    (\hld hex corps ->
       case Map.lookup hex cities of
         Nothing -> hld
         Just (Town _ val) ->
           Set.foldl' (\hld corp -> hld & (corpLens corp) %~ (+ val)) hld corps)
    (defaultHolding 0)
    (_boardState gs)

--opHolding :: PlayerHolding -> Holding
opHolding op ah bh =
  Holding
  { _yellow = op (_yellow ah) (_yellow bh)
  , _blue = op (_blue ah) (_blue bh)
  , _brown = op (_brown ah) (_brown bh)
  , _purple = op (_purple ah) (_purple bh)
  , _black = op (_black ah) (_black bh)
  , _red = op (_red ah) (_red bh)
  }

foldlHolding op init h =
  (op init (_yellow h)) & (flip op) (_blue h) & (flip op) (_brown h) &
  (flip op) (_purple h) &
  (flip op) (_black h) &
  (flip op) (_red h)


drawHolding :: Holding -> PCDiag
drawHolding holding =
  allCorps &
  map
    (\corp ->
       holding & Lens.view (corpLens corp) & show & D.text &
       D.font "Fira Sans Compressed" &
       D.fontSize 7 &
       D.fc (corpColor corp)) &
  (D.vsep 0.4)

drawHoldings :: GameState -> PCDiag
drawHoldings gs =
  let corpVals = corpValues gs
  in [ (drawHolding (_reserve gs))
     , (drawHolding corpVals)
     , ((_playerHoldings gs) & Map.toList &
        map
          (\(player, holdings) ->
             let pHolding =
                   (opHolding (+) (_public holdings) (_private holdings))
             in [ drawHolding pHolding
                , (foldlHolding (+) 0 (opHolding (*) pHolding corpVals)) & show &
                  D.text &
                  D.font "Fira Sans Compressed" &
                  D.fontSize 7 &
                  D.fc (Col.sRGB24read "#131414")
                ] &
                D.vsep 0.5) &
        (D.hsep 0.4))
     ] &
     (D.hsep 0.6) &
     (D.translateX (-10)) &
     (D.translateY (-11))
  

-- "Port Lligat Slab"
-- "Slabo"
-- "Scope One"
-- "Spectral Light"
-- "Charter"
-- "Playfair Display SC"
-- "Smythe" -- quite flowery
-- "Stint Ultra Condensed" - blockey, maybe too squashed
-- Sans:
-- "Fira Sans" "Fira Sans Compressed"
-- drawPCMap = do
--   PS.renderDias (PS.PostscriptOptions "pcmap.eps" (D.mkWidth (8.5 * 72)) PS.EPS) [theMap]

type PCDiag = Diagram SVG.B
drawPCMap = do
  state <-
    Random.evalRandIO
      (do initDivvys <- (divvyStartingShares defaultState)
          let initState =
                List.foldl' (\gs act -> execMove act gs) defaultState initDivvys
          (randomGame initState))
  let diag = (drawHoldings state) <> (drawState state) <> theMap
  SVG.renderSVG "pcmap.svg" (D.mkSizeSpec2D (Just 400) (Just 400)) diag
{-
-}

{-
type PCDiag = Diagram ASVG.B
drawPCMap = do
  state <- Random.evalRandIO (randomGame defaultState)
  let diag = (drawState state) <> theMap
  JSWarp.run
    8383
    (do Just doc <- DOM.currentDocument
        Just body <- DOMD.getBody doc
        DOME.setInnerHTML body ("<h1>Kia ora (Hi)</h1>" :: Text)
        exitMVar <- CMIO.liftIO MVar.newEmptyMVar
        elt <-
          (CMR.runReaderT
             (DBDOM.renderDom
                (ASVG.SVGOptions
                   (D.mkSizeSpec2D (Just 400) (Just 400))
                   mempty
                   "theMap"
                   mempty)
                diag)
             doc)
        elt2 <- DOMD.createTextNode doc ("Heythere" :: Text)
        DOMN.appendChild body elt
        DOMN.appendChild body elt2
        DOM.syncPoint
        CMIO.liftIO $ MVar.takeMVar exitMVar
        return ())
-}
