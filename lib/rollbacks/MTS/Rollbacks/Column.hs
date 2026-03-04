-- | Column GADT for rollback point storage.
--
-- Provides a single-constructor GADT that maps
-- @key@ to 'RollbackPoint' values. Downstream
-- consumers choose the key type (e.g.
-- @WithOrigin slot@, @Maybe slot@) and embed
-- this into their own column GADT.
module MTS.Rollbacks.Column
    ( -- * Column GADT
      RollbackColumn (..)

      -- * KV type alias
    , RollbackKV
    )
where

import Data.Type.Equality ((:~:) (..))
import Database.KV.Transaction
    ( GCompare (..)
    , GEq (..)
    , GOrdering (..)
    , KV
    )
import MTS.Rollbacks.Types
    ( RollbackPoint
    )

-- | KV pair for the rollback column.
type RollbackKV key inv meta =
    KV key (RollbackPoint inv meta)

-- | Column GADT with a single constructor for
-- rollback point storage.
--
-- Downstream consumers embed this into their own
-- column GADT:
--
-- @
-- data MyColumns c where
--     MyKV :: MyColumns (KV Key Value)
--     MyRollbacks
--         :: MyColumns
--              (RollbackKV (WithOrigin Slot) Inv Meta)
-- @
data RollbackColumn key inv meta c where
    RollbackPoints
        :: RollbackColumn
            key
            inv
            meta
            (RollbackKV key inv meta)

instance GEq (RollbackColumn key inv meta) where
    geq RollbackPoints RollbackPoints =
        Just Refl

instance GCompare (RollbackColumn key inv meta) where
    gcompare RollbackPoints RollbackPoints =
        GEQ
