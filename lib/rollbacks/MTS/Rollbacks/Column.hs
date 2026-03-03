-- | Column GADT for rollback point storage.
--
-- Provides a single-constructor GADT that maps
-- @WithOrigin slot@ keys to 'RollbackPoint' values.
-- Downstream consumers embed this into their own
-- column GADT with a wrapper constructor.
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
    , WithOrigin
    )

-- | KV pair for the rollback column.
type RollbackKV slot inv meta =
    KV (WithOrigin slot) (RollbackPoint inv meta)

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
--              (RollbackKV Slot Inv Meta)
-- @
data RollbackColumn slot inv meta c where
    RollbackPoints
        :: RollbackColumn
            slot
            inv
            meta
            (RollbackKV slot inv meta)

instance GEq (RollbackColumn slot inv meta) where
    geq RollbackPoints RollbackPoints =
        Just Refl

instance GCompare (RollbackColumn slot inv meta) where
    gcompare RollbackPoints RollbackPoints =
        GEQ
