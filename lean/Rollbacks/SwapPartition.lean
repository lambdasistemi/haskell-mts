/-!
# Swap Partition Model

The state of a key-value store is a total function
`Key → Value⊥` (where `⊥` is the "empty" element).

Every mutation (insert, delete, update) is a **swap**:
bring a new binding `(k, v)`, exchange it with whatever
the state currently holds at `k`. The displaced binding
is the inverse operation.

## Universe

`Keys × (Values ∪ {⊥})` is partitioned into two disjoint
sets:

- **S** (state): for each key, exactly one pair `(k, _)`
- **W** (world): everything else

Every step picks `(k, v)` from W, swaps it with `(k, v')`
in S. The pair `(k, v')` moves to the log (subset of W).

## Key properties

1. **Conservation**: `|S| = |Keys|` always (one binding
   per key)
2. **Involution**: swap ∘ swap = id (applying the inverse
   restores)
3. **Rollback**: replaying the inverse log in reverse
   restores any previous state
-/

/-- Values include a distinguished "empty" element. -/
inductive Val (α : Type) : Type where
  | empty : Val α
  | some : α → Val α
  deriving DecidableEq, Repr

/-- A state is a total function from keys to values. -/
def State (κ α : Type) : Type := κ → Val α

/-- The initial state: every key maps to empty. -/
def State.init (κ α : Type) : State κ α :=
  fun _ => Val.empty

/-- A binding: a key paired with a value (possibly empty). -/
structure Binding (κ α : Type) where
  key : κ
  val : Val α
  deriving DecidableEq, Repr

/-- Swap a binding into the state at its key.
    Returns the displaced binding (the inverse). -/
def swap
    {κ α : Type}
    [DecidableEq κ]
    (s : State κ α)
    (b : Binding κ α)
    : State κ α × Binding κ α :=
  let displaced : Binding κ α := ⟨b.key, s b.key⟩
  let s' : State κ α := fun k =>
    if k = b.key then b.val else s k
  (s', displaced)

/-- Apply a sequence of swaps, collecting the inverse log.
    Log is in forward order (oldest inverse first). -/
def applySwaps
    {κ α : Type}
    [DecidableEq κ]
    (s : State κ α)
    : List (Binding κ α)
    → State κ α × List (Binding κ α)
  | [] => (s, [])
  | b :: bs =>
    let (s', inv) := swap s b
    let (sFinal, invRest) := applySwaps s' bs
    (sFinal, inv :: invRest)

/-- The state component of applySwaps over a concatenation
    equals applying the first list then the second. -/
theorem applySwaps_append_fst
    {κ α : Type}
    [DecidableEq κ]
    (s : State κ α)
    (xs ys : List (Binding κ α))
    : (applySwaps s (xs ++ ys)).1
    = (applySwaps (applySwaps s xs).1 ys).1 := by
  induction xs generalizing s with
  | nil => simp [applySwaps]
  | cons x xs ih =>
    simp only [List.cons_append, applySwaps]
    exact ih (swap s x).1
