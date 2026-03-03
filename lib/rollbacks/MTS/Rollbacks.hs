-- | Generic rollback library for key-value stores.
--
-- Every mutation (insert, delete, update) is modeled as
-- a swap: bring a new binding, exchange it with whatever
-- the state currently holds at that key. The displaced
-- binding is the inverse operation.
--
-- Rollback replays the inverse log in reverse order,
-- restoring the original state. This is proved correct
-- in @lean\/Rollbacks\/Rollback.lean@.
module MTS.Rollbacks
    ( -- * Re-exports
      module MTS.Rollbacks.Types
    )
where

import MTS.Rollbacks.Types
