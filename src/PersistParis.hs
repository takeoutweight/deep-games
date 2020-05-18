-- | Stores game records in SQLite, except w/ Beam

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
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PersistParis where

import Control.Lens ((%~), (&), (.~), (^.))
import Control.Lens.Wrapped (Wrapped(..), op)
import qualified Control.Lens as L
import qualified Data.List as List
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import qualified Data.Functor.Identity as DFI
import qualified Data.Vinyl as V
import Data.Vinyl (Rec((:&)))
import qualified Data.Vinyl.Functor as VF
import qualified Data.Vinyl.TypeLevel as VT
import qualified Data.Vinyl.XRec as VX
import qualified Database.Beam as B
import qualified Database.Beam.Migrate as BM
import qualified Database.Beam.Migrate.Simple as BMS
import qualified Database.Beam.Migrate.Backend as BMB
import qualified Database.Beam.Sqlite.Migrate as BSM
import qualified Database.Beam.Sqlite as DBS
import qualified Database.SQLite.Simple as SQ
import Data.Coerce (Coercible(..))

-- TODO write it all
