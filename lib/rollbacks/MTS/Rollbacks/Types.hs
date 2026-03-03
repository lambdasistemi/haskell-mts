-- | Core types for the swap-partition rollback model.
--
-- The key insight: every state mutation is a /swap/.
-- Insert, delete, and update are all the same operation
-- — put a new value at a key, get back what was there.
-- The displaced value is the inverse operation.
--
-- See @lean\/Rollbacks\/SwapPartition.lean@ for the
-- formal model and @lean\/Rollbacks\/Rollback.lean@
-- for correctness proofs.
module MTS.Rollbacks.Types
    ( -- * Operations
      Operation (..)

      -- * Inverse computation
    , inverseOf
    )
where

-- | A mutation on a key-value store.
--
-- Both 'Insert' and 'Delete' are swaps in disguise:
--
-- * @Insert k v@ swaps @(k, v)@ into the state,
--   displacing @(k, ∅)@ or @(k, oldV)@
-- * @Delete k@ swaps @(k, ∅)@ into the state,
--   displacing @(k, v)@
--
-- Corresponds to @Binding@ in the Lean formalization.
data Operation key value
    = -- | Insert or update a key-value pair.
      Insert key value
    | -- | Delete a key.
      Delete key
    deriving stock (Eq, Show)

-- | Compute the inverse of an operation given the
-- current state at that key.
--
-- This is the \"swap\": we need the current value
-- to produce the displaced binding.
--
-- Corresponds to @swap@ in @SwapPartition.lean@:
--
-- * @inverseOf (Insert k v) Nothing  = Just (Delete k)@
--   (was empty, inverse removes the new value)
-- * @inverseOf (Insert k v) (Just old) = Just (Insert k old)@
--   (was occupied, inverse restores old value)
-- * @inverseOf (Delete k) Nothing  = Nothing@
--   (deleting empty is a no-op)
-- * @inverseOf (Delete k) (Just v) = Just (Insert k v)@
--   (was occupied, inverse restores the value)
inverseOf
    :: Operation key value
    -- ^ The operation being applied
    -> Maybe value
    -- ^ Current value at the key (read before applying)
    -> Maybe (Operation key value)
    -- ^ The inverse operation ('Nothing' if no-op)
inverseOf (Insert k _v) Nothing = Just (Delete k)
inverseOf (Insert k _v) (Just old) =
    Just (Insert k old)
inverseOf (Delete _k) Nothing = Nothing
inverseOf (Delete k) (Just v) = Just (Insert k v)
