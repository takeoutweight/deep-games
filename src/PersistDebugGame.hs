{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances  #-} -- for my hijacked instance for Record
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PersistDebugGame where

import Control.Lens ((%~), (&), (.~), (^.), at, non)
import Control.Lens.Wrapped (Wrapped(..), op)
import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.IO.Class as CM
import qualified Control.Lens as L
import qualified Data.List as List
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import qualified Data.Functor.Identity as DFI
import Data.String (fromString)
import qualified Data.Vinyl as V
import Data.Vinyl (Rec((:&)))
import qualified Data.Vinyl.Functor as VF
import qualified Data.Vinyl.TypeLevel as VT
import qualified Data.Vinyl.XRec as VX
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL.BeamExtensions as BExt
import Database.Beam (PrimaryKey, primaryKey)
import qualified Database.Beam.Migrate as BM
import qualified Database.Beam.Migrate.Simple as BMS
import qualified Database.Beam.Migrate.Backend as BMB
import qualified Database.Beam.Sqlite.Migrate as BSM
import qualified Database.Beam.Sqlite as DBS
import qualified Database.SQLite.Simple as SQ
import GHC.Generics
       ((:*:), C1, D1, DecidedStrictness(..), FixityI(..), Generic,
        Generic1, K1, M1, Meta(..), Rec0, Rep, Rep1, S1,
        SourceStrictness(..), SourceUnpackedness(..), from1, to1)
import qualified GHC.Generics as G
-- import qualified Generics.SOP as GSOP
-- import qualified Generics.SOP.NP as GNP
import Data.Coerce (Coercible(..))
import qualified System.Random as Random

import qualified DebugGame as DG
import qualified UCT as UCT

----------

type Record = Rec DFI.Identity
getIdentity = DFI.runIdentity
setIdentity = DFI.Identity

instance VX.IsoHKD DFI.Identity a where
  type HKD DFI.Identity a = a
  unHKD = DFI.Identity
  toHKD (DFI.Identity x) = x

-- I didn't need these extra constraints before, maybe something changed in Vinyl in the last X years.
instance {-# OVERLAPS #-} ( VT.RecAll DFI.Identity rs Show
                          , V.RMap rs
                          , V.RecordToList rs
                          , V.ReifyConstraint Show DFI.Identity rs) =>
                          Show (Record rs) where
  show xs = showsPrec 0 xs ""
  showsPrec p xs =
    showParen
      (p > fconsPrecedence)
      (\suffix ->
         (\str -> str <> " &: Nil" <> suffix) .
         List.intercalate " &: " .
         V.recordToList .
         V.rmap
           (\(VF.Compose (V.Dict x)) ->
              VF.Const $
              (let str = (showsPrec (fconsPrecedence + 1) x "")
               in case List.stripPrefix "Identity " str of
                    Just a -> a
                    Nothing -> str)) $
         V.reifyConstraint @Show xs)

instance {-# OVERLAPS #-} (V.RMap rs, V.ReifyConstraint Show f rs, V.RecordToList rs)
  => Show (V.Rec f rs) where
  show xs =
    (\str -> "{" <> str <> "}")
      . List.intercalate ", "
      . V.recordToList
      . V.rmap (\(VF.Compose (V.Dict x)) -> VF.Const $ show x)
      $ V.reifyConstraint @Show xs

fcons :: r -> Record rs -> Record (r : rs)
fcons e rs = (setIdentity e) :& rs

fconsPrecedence ::  Int
fconsPrecedence = 5

(&:) :: r -> Record rs -> Record (r : rs)
e &: rs = fcons e rs
infixr 5 &:

pattern Nil :: V.Rec f '[]
pattern Nil = V.RNil
  
----------

newtype ActivePlayer = ActivePlayer Int deriving (Show, Generic)
instance Wrapped ActivePlayer

newtype Moves = Moves Int deriving (Show, Generic)
instance Wrapped Moves

newtype Score = Score (Map Int Int) deriving (Show, Generic)
instance Wrapped Score



type DebugGameState f = V.Rec f '[ ActivePlayer, Moves, Score]


type RandomTupleT f = ( B.C f ActivePlayer, B.C f Moves, B.C f Score)

newtype RandomTuple f =
  RandomTuple (RandomTupleT f)
  deriving Generic via (RandomTupleT f)
  deriving anyclass B.Beamable -- only works because I expanded the tuple.

data ANil = ANil deriving Generic

-- no surprise, works the same :kind! (Rep (Tup Int))
newtype Tup a = Tup (a, ANil) deriving Generic via (a,ANil)

type TupleWithNilT f = (B.C f Int, ANil)

--  this one leaves ANil opaque as a Rec0
newtype TupleWithNil f = TupleWithNil (TupleWithNilT f)
--  deriving Generic via (TupleWithNilT f)



data RandomRecord f = RandomRecord
  { _id :: B.C f Int
  , _active :: B.C f ActivePlayer
  , _moves :: B.C f Moves
  , _score :: B.C f Score
  } deriving (Generic, B.Beamable)

instance B.Table RandomRecord where
  data PrimaryKey RandomRecord f = RandomRecordId (B.C f Int)
                               deriving (Generic, B.Beamable)
  primaryKey = RandomRecordId . _id

data RandomDB f = RandomDB
  { _randomRecord :: f (B.TableEntity RandomRecord)
  } deriving (Generic, B.Database be)

-- randomDB :: BM.CheckedDatabaseSettings DBS.Sqlite RandomDB
-- randomDB = BM.defaultMigratableDbSettings

initDebugGameState = ActivePlayer 0 &: Moves 0 &: Score Map.empty &: Nil

data DebugGameStateDB f = DebugGameStateDB (V.Rec f '[ B.C f ActivePlayer, B.C f Moves, B.C f Score])
--  deriving stock Generic -- this works.
--  deriving anyclass B.Beamable -- doesn't work, not sure why?


newtype TaggedRec f = TaggedRec { unTaggedRec :: (V.Rec f '[Int, String])}

-- newtype TaggedRecC f = TaggedRecC (TaggedRec (B.C f)) -- can't paritally apply B.C

-- instance B.Beamable (TaggedRec (B.C f))

-- deriving Show via (Rec DFI.Identity '[Int, String])

-- (V.rget @Int (unTaggedRec (TaggedRec ((3 :: Int) &: "hi" &: Nil))))

instance Generic (TaggedRec f) where
  type Rep (TaggedRec f) =
    D1 ('MetaData "(,)" "GHC.Tuple" "ghc-prim" 'False)
       (C1
          ('MetaCons "(,)" 'PrefixI 'False)
          ((S1
             ('MetaSel
                'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
             (Rec0 (f Int)))
           :*:
           (S1
             ('MetaSel
               'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
             (Rec0 (f String)))))

--  from :: a -> GHC.Generics.Rep a x
  from (TaggedRec r) = G.from ((V.rget @Int r),(V.rget @String r))
--  to :: GHC.Generics.Rep a x -> a
--  to _ = undefined
  to (G.M1 (G.M1 ((G.M1 (G.K1 a)) G.:*: (G.M1 (G.K1 b))))) = (TaggedRec (a :& b :& V.RNil))


data SomeSum
  = SumA Int
  | SumB String
         String
  deriving (Generic, Show)

L.makePrisms ''SomeSum

-- Basically pretty hard to work w/ newtypes in Beam
data SimpleRecord f = SimpleRecord
  { _sid :: B.C f Int
  , _sactive :: B.C f Int
  , _smoves :: B.C f Int
  , _sscore :: B.C f Int
  } deriving (Generic, B.Beamable)

instance B.Table SimpleRecord where
  data PrimaryKey SimpleRecord f = SimpleRecordId (B.C f Int)
                               deriving (Generic, B.Beamable)
  primaryKey = SimpleRecordId . _sid

data SimpleDB f = SimpleDB
  { _simpleRecord :: f (B.TableEntity SimpleRecord)
  } deriving (Generic, B.Database be)

simpleDB :: BM.CheckedDatabaseSettings DBS.Sqlite SimpleDB
simpleDB = BM.defaultMigratableDbSettings

{-
-- This doesn't work
data EmptyDB (f :: * -> *) = EmptyDB
  {
  } deriving (Generic, B.Database be)

emptyDB :: BM.CheckedDatabaseSettings DBS.Sqlite EmptyDB
emptyDB = BM.defaultMigratableDbSettings
-}
createSimpleDB filename = SQ.withConnection filename (\conn -> (SQ.execute_ conn "VACUUM;"))

-- actual <- SQ.withConnection "test2.db" (\conn -> DBS.runBeamSqlite conn BSM.getDbConstraints)
-- let soln = ((BM.heuristicSolver BM.defaultActionProvider actual (BM.collectChecks debugDB)) :: BM.Solver DBS.Sqlite) & BM.finalSolution
-- (case soln of BM.Solved mcs -> map BM.migrationCommand mcs) & map (BMB.backendRenderSyntax BSM.migrationBackend) & traverse putStrLn
-- SQ.withConnection "test2.db" (\conn -> DBS.runBeamSqlite conn (BMS.autoMigrate BSM.migrationBackend debugDB))
-- SQ.withConnection "test2.db" (\conn -> DBS.runBeamSqlite conn (BMS.createSchema BSM.migrationBackend debugDB))

---------- The Actual DebugGame Schema ----------

data GameT f = Game
  { _gameId :: B.C f Int
  } deriving (Generic, B.Beamable)

instance B.Table GameT where
  data PrimaryKey GameT f = GameId (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = GameId . _gameId

type Game = GameT B.Identity
deriving instance Show Game
deriving instance Eq Game
deriving instance Ord Game


data ActionT f = Action
  { _game :: PrimaryKey GameT f
  , _moveNum  :: B.C f Int
  , _action :: B.C f Int
  , _r0 :: B.C f Double
  , _r1 :: B.C f Double
  , _r2 :: B.C f Double
  , _r3 :: B.C f Double
  , _r4 :: B.C f Double
  , _r5 :: B.C f Double
  , _r6 :: B.C f Double
  , _r7 :: B.C f Double
  , _r8 :: B.C f Double
  , _r9 :: B.C f Double
  } deriving (Generic, B.Beamable)

-- Not a SQLite thing
-- "ALTER TABLE actions ADD CHECK (UNIQUE (game_id, num) ON CONFLICT REPLACE)"
-- These apparently violate uniqueness in a way I don't understand?
-- "CREATE UNIQUE INDEX action_index on actions (actions.game_id, actions.num);"
-- CREATE UNIQUE INDEX "action_index" on "actions" ("game_id", "num");
-- CREATE UNIQUE INDEX "action_index" on "actions" ("actions.game_id", "actions.num");

type Action = ActionT B.Identity

instance B.Table ActionT where
  data PrimaryKey ActionT f = ActionNoId
                       deriving (Generic, B.Beamable)
  primaryKey _ = ActionNoId

data DebugDB f = DebugDB
  { _debugGames :: f (B.TableEntity GameT)
  , _debugActions :: f (B.TableEntity ActionT)
  } deriving (Generic, B.Database be)

debugDB :: BM.CheckedDatabaseSettings DBS.Sqlite DebugDB
debugDB = BM.defaultMigratableDbSettings

withSavepoint :: (CM.MonadIO io, Catch.MonadMask io) => SQ.Connection -> io a -> io a
withSavepoint conn action =
  Catch.mask $ \restore -> do
    idInt <- CM.liftIO Random.randomIO
    let savepoint =
          ("Backup_withSavepoint2" <>
           fromString (show (mod idInt (1000000000 :: Int))))
    CM.liftIO (SQ.execute_ conn ("SAVEPOINT " <> savepoint))
    r <-
      (restore action) `Catch.onException`
      (CM.liftIO
         (do (SQ.execute_ conn ("ROLLBACK TO " <> savepoint))
             (SQ.execute_ conn ("RELEASE " <> savepoint))))
    CM.liftIO (SQ.execute_ conn ("RELEASE " <> savepoint))
    return r

-- | returns id of the added game
-- recordGame :: [Action] -> SQ.Connection -> IO Int
nextId =
  B.all_ (_debugGames (BM.unCheckDatabase debugDB)) &
  B.aggregate_ (\g -> B.fromMaybe_ (-1) (B.max_ (_gameId g)))
  & fmap (+1)

--  B.select &
--  B.runSelectReturningOne &
--  (fmap (L.view (non (-1)))) &
--  
--  DBS.runBeamSqlite conn &
--  withSavepoint conn

-- insertGame :: SQ.Connection -> IO ()
insertGame :: SQ.Connection -> IO Game
insertGame conn =
  (do next <- nextId
      pure (Game {_gameId = next})) &
  B.insertFrom &
  B.insert (_debugGames (BM.unCheckDatabase debugDB)) &
  BExt.runInsertReturningList &
  fmap head &
  DBS.runBeamSqlite conn &
  withSavepoint conn

recordGame :: [(Int, UCT.GameTree Int)] -> SQ.Connection -> IO Game
recordGame actions conn = do
  game <- insertGame conn
  (zip actions [0 ..]) &
    map
      (\((action, gt), idx) ->
         let winRate move =
               case Map.lookup move (UCT._moves gt) of
                 Nothing -> 0.0
                 Just ns -> UCT.winRate ns
         in Action
            { _game = primaryKey game
            , _moveNum = idx
            , _action = action
            , _r0 = winRate 0
            , _r1 = winRate 1
            , _r2 = winRate 2
            , _r3 = winRate 3
            , _r4 = winRate 4
            , _r5 = winRate 5
            , _r6 = winRate 6
            , _r7 = winRate 7
            , _r8 = winRate 8
            , _r9 = winRate 9
            }) &
    B.insertValues &
    B.insert (_debugActions (BM.unCheckDatabase debugDB)) &
    B.runInsert &
    DBS.runBeamSqlite conn & -- DBS.runBeamSqlite putStrLn conn &
    withSavepoint conn
  return game

-- rg3 <- UCT.randomGameViaUTC 1000 DG.debugGameLogic DG.initGS
-- SQ.withConnection "test2.db" (\conn -> recordGame rg3 conn)
-- UCT.bestLine (UCT.NodeState 0 0 (snd (rg3 !! 11)))
