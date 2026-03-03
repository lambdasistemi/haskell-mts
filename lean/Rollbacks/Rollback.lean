import Rollbacks.SwapPartition

/-!
# Rollback Proofs

Theorems proving that the swap-partition model supports
correct rollback via inverse log replay.

## Main results

1. `swap_inverse_restores` — swapping twice at the same
   key restores the original state
2. `swap_inverse_binding` — the displaced binding,
   when swapped back, produces the original binding
3. `rollback_restores` — replaying the reversed inverse
   log restores the original state
-/

-- ============================================================
-- Swap involution: swap the inverse back → original state
-- ============================================================

/-- Swapping a binding and then swapping the displaced
    binding back restores the original state at every key. -/
theorem swap_inverse_restores
    {κ α : Type}
    [DecidableEq κ]
    (s : State κ α)
    (b : Binding κ α)
    : let (s', inv) := swap s b
      let (s'', _) := swap s' inv
      ∀ (k : κ), s'' k = s k := by
  intro k
  dsimp [swap]
  split <;> simp_all

/-- The displaced binding from a swap, when swapped back,
    produces the original binding as its own inverse. -/
theorem swap_inverse_binding
    {κ α : Type}
    [DecidableEq κ]
    (s : State κ α)
    (b : Binding κ α)
    : let (s', inv) := swap s b
      let (_, inv') := swap s' inv
      inv' = b := by
  simp [swap]

-- ============================================================
-- Single-step rollback
-- ============================================================

/-- After applying one swap, applying the inverse restores
    the state (pointwise equality). -/
theorem single_step_rollback
    {κ α : Type}
    [DecidableEq κ]
    (s : State κ α)
    (b : Binding κ α)
    : let (s', inv) := swap s b
      (swap s' inv).1 = s := by
  funext k
  exact swap_inverse_restores s b k

-- ============================================================
-- Multi-step rollback
-- ============================================================

/-- Rollback: apply the inverse log in reverse order. -/
def rollback
    {κ α : Type}
    [DecidableEq κ]
    (s : State κ α)
    (invLog : List (Binding κ α))
    : State κ α :=
  (applySwaps s invLog.reverse).1

/-- Applying a sequence of swaps and then rolling back
    with the inverse log restores the original state.

    This is the main correctness theorem: for any
    sequence of operations, the displaced bindings
    (inverse log) are sufficient to restore the
    original state when replayed in reverse. -/
theorem rollback_restores
    {κ α : Type}
    [DecidableEq κ]
    (s : State κ α)
    (ops : List (Binding κ α))
    : let (s', invLog) := applySwaps s ops
      rollback s' invLog = s := by
  induction ops generalizing s with
  | nil =>
    simp [applySwaps, rollback]
  | cons b bs ih =>
    simp only [applySwaps]
    show rollback
          (applySwaps (swap s b).1 bs).1
          ((swap s b).2
            :: (applySwaps (swap s b).1 bs).2)
        = s
    unfold rollback
    rw [List.reverse_cons, applySwaps_append_fst]
    have h_ih := ih (swap s b).1
    unfold rollback at h_ih
    rw [h_ih]
    simp [applySwaps]
    exact single_step_rollback s b

-- ============================================================
-- Conservation: state always has exactly one binding per key
-- ============================================================

/-- The state maps every key to exactly one value.
    This holds by construction (State is a total function),
    but we state it explicitly for documentation. -/
theorem state_total
    {κ α : Type}
    (s : State κ α)
    (k : κ)
    : ∃ (v : Val α), s k = v :=
  ⟨s k, rfl⟩

/-- Swap preserves totality: after a swap, the resulting
    state still maps every key to exactly one value. -/
theorem swap_preserves_totality
    {κ α : Type}
    [DecidableEq κ]
    (s : State κ α)
    (b : Binding κ α)
    (k : κ)
    : ∃ (v : Val α), (swap s b).1 k = v :=
  ⟨(swap s b).1 k, rfl⟩

-- ============================================================
-- Non-interference: swaps at different keys commute
-- ============================================================

/-- Swaps at distinct keys commute: the order doesn't matter
    when operating on different keys. -/
theorem swap_commute
    {κ α : Type}
    [DecidableEq κ]
    (s : State κ α)
    (b₁ b₂ : Binding κ α)
    (h : b₁.key ≠ b₂.key)
    : let (s₁, _) := swap s b₁
      let (s₂, _) := swap s₁ b₂
      let (t₁, _) := swap s b₂
      let (t₂, _) := swap t₁ b₁
      ∀ (k : κ), s₂ k = t₂ k := by
  intro k
  dsimp [swap]
  split <;> split <;> simp_all
