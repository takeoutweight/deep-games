{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances  #-} -- for my hijacked instance for Record
{-# LANGUAGE TypeApplications      #-}

module PersistDebugGame where

import Control.Lens.Wrapped (Wrapped(..), op)
import qualified Data.List as List
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import qualified Data.Functor.Identity as DFI
import qualified Data.Vinyl as V
import Data.Vinyl (Rec((:&)))
import qualified Data.Vinyl.Functor as VF
import qualified Data.Vinyl.TypeLevel as VT
import GHC.Generics (Generic)

----------

type Record = Rec DFI.Identity
getIdentity = DFI.runIdentity
setIdentity = DFI.Identity


-- I didn't need these extra constraints before, maybe somethign changed in Vinyl in the last X years.
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

initDebugGameState = ActivePlayer 0 &: Moves 0 &: Score Map.empty &: Nil
