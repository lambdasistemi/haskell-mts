import Rollbacks.SwapPartition

/-!
# Journal / KV / CSMT Invariants

Models the three-store system used in MTS split-mode:

- **KV**: the source of truth for key-value pairs
- **Journal**: tracks pending changes for the CSMT tree
- **CSMT**: the Merkle tree (frozen during KVOnly mode)

## Operations (KVOnly mode)

- **insert k v**: write to KV and journal
- **delete k**: remove from KV; if k is in journal, remove
  from journal (elide); if not, write JDelete to journal

## Replay (transition to Full)

Apply journal to CSMT: JInsert entries are inserted,
JDelete entries remove from CSMT. Then journal is cleared.

## Invariants

1. From empty CSMT: `journal.keys = KV.keys`
2. After replay: `journal = ∅` and `CSMT = KV`
3. General: `KV = apply(journal, CSMT)`
-/

-- ============================================================
-- Store model using partial functions
-- ============================================================

/-- A store is a partial function from keys to values. -/
def FStore (κ α : Type) : Type := κ → Option α

/-- Empty store. -/
def FStore.empty {κ α : Type} : FStore κ α :=
  fun _ => none

/-- Insert a key-value pair. -/
def FStore.insert {κ α : Type} [DecidableEq κ]
    (s : FStore κ α) (k : κ) (v : α)
    : FStore κ α :=
  fun k' => if k' = k then some v else s k'

/-- Remove a key. -/
def FStore.erase {κ α : Type} [DecidableEq κ]
    (s : FStore κ α) (k : κ)
    : FStore κ α :=
  fun k' => if k' = k then none else s k'

/-- Lookup. -/
def FStore.get? {κ α : Type}
    (s : FStore κ α) (k : κ)
    : Option α :=
  s k

-- ============================================================
-- Journal entries
-- ============================================================

/-- Journal entries track the relationship to CSMT:
    - ins: genuinely new key (not in CSMT)
    - upd: overwrite of a CSMT key (CSMT has old value)
    - del: removal of a CSMT key -/
inductive JEntry (α : Type) : Type where
  | ins : α → JEntry α
  | upd : α → JEntry α
  | del : α → JEntry α
  deriving Repr, BEq

-- ============================================================
-- Three-store state
-- ============================================================

/-- The three-store state. -/
structure Stores (κ α : Type) where
  kv : FStore κ α
  journal : FStore κ (JEntry α)
  csmt : FStore κ α

/-- Empty stores. -/
def Stores.empty {κ α : Type} : Stores κ α :=
  ⟨FStore.empty, FStore.empty, FStore.empty⟩

-- ============================================================
-- KVOnly operations
-- ============================================================

/-- Insert in KVOnly mode.
    Journal tag depends on current journal state:
    - Nothing × Nothing → JInsert (new key)
    - Nothing × Just _  → JUpdate (key from CSMT)
    - JInsert × _       → JInsert (still new)
    - JUpdate × _       → JUpdate (still CSMT key)
    - JDelete × _       → JUpdate (re-insert CSMT key) -/
def kvInsert {κ α : Type} [DecidableEq κ]
    (st : Stores κ α) (k : κ) (v : α)
    : Stores κ α :=
  let tag := match st.journal.get? k with
    | some (.ins _) => JEntry.ins v
    | some (.upd _) => JEntry.upd v
    | some (.del _) => JEntry.upd v
    | none =>
      match st.kv.get? k with
      | some _ => JEntry.upd v  -- key in KV → from CSMT
      | none   => JEntry.ins v  -- genuinely new
  { st with
    kv := st.kv.insert k v
    journal := st.journal.insert k tag
  }

/-- Delete in KVOnly mode.
    Journal tag composition:
    - Nothing → JDelete (CSMT key)
    - JInsert → ∅ elide (new key, not in CSMT)
    - JUpdate → JDelete (CSMT key removed)
    - JDelete → ⊥ unreachable (KV empty) -/
def kvDelete {κ α : Type} [DecidableEq κ]
    (st : Stores κ α) (k : κ)
    : Stores κ α :=
  match st.kv.get? k with
  | none => st
  | some v =>
    let kv' := st.kv.erase k
    match st.journal.get? k with
    | some (.ins _) =>
      -- New key: elide (cancel insert+delete)
      { st with kv := kv', journal := st.journal.erase k }
    | some (.upd _) =>
      -- CSMT key: must tell replay to delete from tree
      { st with kv := kv', journal := st.journal.insert k (.del v) }
    | some (.del _) =>
      -- Unreachable: KV has key but journal says deleted
      st
    | none =>
      -- Key from CSMT (no journal entry)
      { st with kv := kv', journal := st.journal.insert k (.del v) }

-- ============================================================
-- Replay
-- ============================================================

/-- Apply a single journal entry to the CSMT. -/
def applyEntry {κ α : Type} [DecidableEq κ]
    (csmt : FStore κ α) (k : κ) (e : JEntry α)
    : FStore κ α :=
  match e with
  | .ins v => csmt.insert k v
  | .upd v => csmt.insert k v
  | .del _ => csmt.erase k

/-- Apply a list of journal entries to the CSMT. -/
def applyEntries {κ α : Type} [DecidableEq κ]
    (csmt : FStore κ α)
    : List (κ × JEntry α) → FStore κ α
  | [] => csmt
  | (k, e) :: rest => applyEntries (applyEntry csmt k e) rest

-- ============================================================
-- Operations and sequences
-- ============================================================

/-- An operation in KVOnly mode. -/
inductive Op (κ α : Type) : Type where
  | ins : κ → α → Op κ α
  | del : κ → Op κ α

/-- Apply a single operation. -/
def applyOp {κ α : Type} [DecidableEq κ]
    (st : Stores κ α) : Op κ α → Stores κ α
  | .ins k v => kvInsert st k v
  | .del k => kvDelete st k

/-- Apply a sequence of operations. -/
def applyOps {κ α : Type} [DecidableEq κ]
    (st : Stores κ α) : List (Op κ α) → Stores κ α
  | [] => st
  | op :: ops => applyOps (applyOp st op) ops

-- ============================================================
-- QC1: From empty CSMT, journal keys = KV keys
-- ============================================================

/-- The genesis invariant: starting from empty stores,
    journal has a key iff KV has that key, and all journal
    entries are JInsert (no JUpd or JDel since CSMT is empty). -/
def genesisInv {κ α : Type} (st : Stores κ α) : Prop :=
  st.csmt = FStore.empty
  ∧ (∀ (k : κ), (st.journal.get? k).isSome = (st.kv.get? k).isSome)
  ∧ (∀ (k : κ) (e : JEntry α), st.journal.get? k = some e →
      ∃ (v : α), e = JEntry.ins v)

/-- Insert preserves genesis invariant. -/
theorem kvInsert_preserves_genesisInv
    {κ α : Type} [DecidableEq κ]
    (st : Stores κ α) (k : κ) (v : α)
    (h : genesisInv st)
    : genesisInv (kvInsert st k v) := by
  obtain ⟨hcsmt, hinv, hins_only⟩ := h
  refine ⟨hcsmt, fun k' => ?_, fun k' e he => ?_⟩
  · -- Part 2: isSome agreement
    simp only [kvInsert, FStore.get?, FStore.insert]
    by_cases hk : k' = k
    · subst hk; simp only [ite_true]
      cases hj : st.journal k' with
      | none => cases hkv : st.kv k' <;> simp
      | some je => cases je <;> simp
    · simp only [show k' ≠ k from hk, ite_false]; exact hinv k'
  · -- Part 3: all entries are JInsert
    simp only [kvInsert, FStore.insert, FStore.get?] at he
    by_cases hk : k' = k
    · subst hk; simp only [ite_true] at he
      cases hj : st.journal k' with
      | none =>
        simp [hj] at he
        cases hkv : st.kv k'
        · simp [hkv] at he; exact ⟨v, he.symm⟩
        · -- journal none, KV some → contradicts genesis inv
          have := hinv k'; simp [FStore.get?, hj, hkv] at this
      | some je =>
        obtain ⟨v'', rfl⟩ := hins_only k' je (by simp [FStore.get?, hj])
        simp [hj] at he; exact ⟨v, he.symm⟩
    · simp only [show k' ≠ k from hk, ite_false] at he
      exact hins_only k' e (by simp [FStore.get?]; exact he)

/-- Delete preserves genesis invariant. -/
theorem kvDelete_preserves_genesisInv
    {κ α : Type} [DecidableEq κ]
    (st : Stores κ α) (k : κ)
    (h : genesisInv st)
    : genesisInv (kvDelete st k) := by
  obtain ⟨hcsmt, hinv, hins_only⟩ := h
  simp only [kvDelete, FStore.get?]
  cases hkv : st.kv k with
  | none => exact ⟨hcsmt, hinv, hins_only⟩
  | some v =>
    have hj_some : (st.journal k).isSome = true := by
      have := hinv k; simp [FStore.get?, hkv] at this; exact this
    obtain ⟨je, hje⟩ := Option.isSome_iff_exists.mp hj_some
    obtain ⟨v', rfl⟩ := hins_only k je (by simp [FStore.get?]; exact hje)
    -- Journal has JInsert → elide
    simp [hje]
    refine ⟨hcsmt, fun k' => ?_, fun k' e he => ?_⟩
    · simp only [FStore.erase, FStore.get?]
      by_cases hk : k' = k
      · simp [hk]
      · simp only [show k' ≠ k from hk, ite_false]; exact hinv k'
    · simp only [FStore.erase, FStore.get?] at he
      by_cases hk : k' = k
      · simp [hk] at he
      · simp only [show k' ≠ k from hk, ite_false] at he
        exact hins_only k' e (by simp [FStore.get?]; exact he)

/-- Any operation preserves genesis invariant. -/
theorem applyOp_preserves_genesisInv
    {κ α : Type} [DecidableEq κ]
    (st : Stores κ α) (op : Op κ α)
    (h : genesisInv st)
    : genesisInv (applyOp st op) := by
  cases op with
  | ins k v => exact kvInsert_preserves_genesisInv st k v h
  | del k => exact kvDelete_preserves_genesisInv st k h

/-- Any op sequence preserves genesis invariant. -/
theorem applyOps_preserves_genesisInv
    {κ α : Type} [DecidableEq κ]
    (st : Stores κ α) (ops : List (Op κ α))
    (h : genesisInv st)
    : genesisInv (applyOps st ops) := by
  induction ops generalizing st with
  | nil => exact h
  | cons op ops ih =>
    exact ih _ (applyOp_preserves_genesisInv st op h)

/-- **QC1**: From empty, journal keys = KV keys. -/
theorem genesis_invariant_holds
    {κ α : Type} [DecidableEq κ]
    (ops : List (Op κ α))
    : genesisInv (applyOps Stores.empty ops) :=
  applyOps_preserves_genesisInv _ ops
    ⟨rfl,
     fun k => by simp [Stores.empty, FStore.empty, FStore.get?],
     fun k e he => by simp [Stores.empty, FStore.empty, FStore.get?] at he⟩

-- ============================================================
-- QC2–4: General invariant and replay
-- ============================================================

/-- Applying a journal entry at key k to CSMT, then looking
    up k, gives the expected result. -/
theorem applyEntry_at_key {κ α : Type} [DecidableEq κ]
    (csmt : FStore κ α) (k : κ) (e : JEntry α)
    : (applyEntry csmt k e).get? k =
      match e with
      | .ins v => some v
      | .upd v => some v
      | .del _ => none := by
  cases e with
  | ins v => simp [applyEntry, FStore.insert, FStore.get?]
  | upd v => simp [applyEntry, FStore.insert, FStore.get?]
  | del v => simp [applyEntry, FStore.erase, FStore.get?]

/-- Applying a journal entry at key k does not affect
    other keys. -/
theorem applyEntry_other_key {κ α : Type} [DecidableEq κ]
    (csmt : FStore κ α) (k k' : κ) (e : JEntry α)
    (h : k' ≠ k)
    : (applyEntry csmt k e).get? k' = csmt.get? k' := by
  cases e with
  | ins v => simp [applyEntry, FStore.insert, FStore.get?, h]
  | upd v => simp [applyEntry, FStore.insert, FStore.get?, h]
  | del v => simp [applyEntry, FStore.erase, FStore.get?, h]

-- ============================================================
-- General invariant: KV = apply(journal, CSMT)
-- ============================================================

/-- The general invariant: for every key, KV agrees with
    what you'd get by applying the journal to the CSMT.

    - journal has JInsert k v → KV(k) = some v ∧ CSMT(k) = none
    - journal has JDelete k _ → KV(k) = none ∧ CSMT(k) = some _
    - journal has nothing for k → KV(k) = CSMT(k)

    This is the "special union" — KV = apply(journal, CSMT).

    Note: JInsert entries always correspond to keys NOT in
    CSMT (new keys or keys whose journal entry was overwritten).
    This is maintained by the fact that transitions clear the
    journal and sync CSMT = KV, so any subsequent JInsert is
    for a genuinely new key. -/
def generalInv {κ α : Type} (st : Stores κ α) : Prop :=
  ∀ (k : κ),
    -- Part 1: KV = apply(journal, CSMT)
    (st.kv.get? k =
      match st.journal.get? k with
      | some (.ins v) => some v
      | some (.upd v) => some v
      | some (.del _) => none
      | none => st.csmt.get? k)
    -- Part 2: JInsert ↔ not in CSMT, JUpd/JDel ↔ in CSMT
    ∧ (match st.journal.get? k with
      | some (.ins _) => st.csmt.get? k = none
      | some (.upd _) => (st.csmt.get? k).isSome = true
      | some (.del _) => (st.csmt.get? k).isSome = true
      | none => True)

/-- Empty stores satisfy the general invariant. -/
theorem generalInv_empty {κ α : Type}
    : generalInv (Stores.empty : Stores κ α) := by
  intro k
  simp [Stores.empty, FStore.empty, FStore.get?]

/-- Insert preserves the general invariant. -/
theorem kvInsert_preserves_generalInv
    {κ α : Type} [DecidableEq κ]
    (st : Stores κ α) (k : κ) (v : α)
    (h : generalInv st)
    : generalInv (kvInsert st k v) := by
  intro k'
  simp only [kvInsert, FStore.insert, FStore.get?]
  by_cases hk : k' = k
  · subst hk
    simp only [ite_true]
    obtain ⟨hkv_inv, hcsmt_inv⟩ := h k'
    simp only [FStore.get?] at hkv_inv hcsmt_inv
    cases hj : st.journal k' with
    | some je =>
      cases je <;> simp_all
    | none =>
      cases hkv : st.kv k' with
      | none => simp_all
      | some w =>
        simp_all
        rw [← hkv_inv]; simp
  · simp only [show k' ≠ k from hk, ite_false]
    exact h k'

/-- Delete preserves the general invariant. -/
theorem kvDelete_preserves_generalInv
    {κ α : Type} [DecidableEq κ]
    (st : Stores κ α) (k : κ)
    (h : generalInv st)
    : generalInv (kvDelete st k) := by
  intro k'
  simp only [kvDelete, FStore.get?]
  cases hkv : st.kv k with
  | none => exact h k'
  | some v =>
    cases hj : st.journal k with
    | some je =>
      cases je with
      | ins v' =>
        -- Elide
        simp only [FStore.erase]
        by_cases hk : k' = k
        · subst hk; simp only [ite_true]
          obtain ⟨_, hcsmt⟩ := h k'
          simp only [FStore.get?, hj] at hcsmt
          exact ⟨by simp [hcsmt], trivial⟩
        · simp only [show k' ≠ k from hk, ite_false]; exact h k'
      | upd v' =>
        -- JUpdate → JDelete
        simp only [FStore.erase, FStore.insert]
        by_cases hk : k' = k
        · subst hk; simp only [ite_true]
          obtain ⟨_, hcsmt⟩ := h k'
          simp only [FStore.get?, hj] at hcsmt
          exact ⟨by simp, hcsmt⟩
        · simp only [show k' ≠ k from hk, ite_false]; exact h k'
      | del v' =>
        -- JDel branch: kvDelete returns st unchanged (no-op)
        simp only
        exact h k'
    | none =>
      -- No journal → JDelete
      simp only [FStore.erase, FStore.insert]
      by_cases hk : k' = k
      · subst hk; simp only [ite_true]
        obtain ⟨hkv_inv, _⟩ := h k'
        simp only [FStore.get?, hkv, hj] at hkv_inv
        exact ⟨by simp, by rw [← hkv_inv]; simp⟩
      · simp only [show k' ≠ k from hk, ite_false]; exact h k'

/-- Any op preserves the general invariant. -/
theorem applyOp_preserves_generalInv
    {κ α : Type} [DecidableEq κ]
    (st : Stores κ α) (op : Op κ α)
    (h : generalInv st)
    : generalInv (applyOp st op) := by
  cases op with
  | ins k v => exact kvInsert_preserves_generalInv st k v h
  | del k => exact kvDelete_preserves_generalInv st k h

/-- Any op sequence preserves the general invariant. -/
theorem applyOps_preserves_generalInv
    {κ α : Type} [DecidableEq κ]
    (st : Stores κ α) (ops : List (Op κ α))
    (h : generalInv st)
    : generalInv (applyOps st ops) := by
  induction ops generalizing st with
  | nil => exact h
  | cons op ops ih =>
    exact ih (applyOp st op) (applyOp_preserves_generalInv st op h)

/-- **QC2**: The general invariant holds from empty after
    any sequence of operations. -/
theorem general_invariant_holds
    {κ α : Type} [DecidableEq κ]
    (ops : List (Op κ α))
    : generalInv (applyOps Stores.empty ops) :=
  applyOps_preserves_generalInv _ ops generalInv_empty

-- ============================================================
-- QC3: After replay, CSMT = KV
-- ============================================================

/-- Pointwise replay: apply journal at a single key. -/
def replayAt {κ α : Type}
    (st : Stores κ α) (k : κ) : Option α :=
  match st.journal.get? k with
  | some (.ins v) => some v
  | some (.upd v) => some v
  | some (.del _) => none
  | none => st.csmt.get? k

/-- **QC3**: If the general invariant holds, replaying
    journal into CSMT at any key produces KV. -/
theorem replay_gives_csmt_eq_kv
    {κ α : Type}
    (st : Stores κ α)
    (h : generalInv st)
    : ∀ (k : κ), replayAt st k = st.kv.get? k := by
  intro k
  simp only [replayAt]
  exact (h k).1.symm

/-- **QC3 applied**: From empty, after any ops, replay = KV. -/
theorem replay_csmt_eq_kv_from_empty
    {κ α : Type} [DecidableEq κ]
    (ops : List (Op κ α))
    : let st := applyOps Stores.empty ops
      ∀ (k : κ), replayAt st k = st.kv.get? k :=
  replay_gives_csmt_eq_kv _ (general_invariant_holds ops)

-- ============================================================
-- QC4: Invariant survives replay cycles
-- ============================================================

/-- Model the Full transition: replay into CSMT, clear journal. -/
def transition {κ α : Type}
    (st : Stores κ α) : Stores κ α :=
  { kv := st.kv
  , journal := FStore.empty
  , csmt := fun k => replayAt st k
  }

/-- After transition, journal is empty. -/
theorem transition_journal_empty {κ α : Type}
    (st : Stores κ α)
    : (transition st).journal = FStore.empty := by
  simp [transition]

/-- After transition, CSMT = KV. -/
theorem transition_csmt_eq_kv {κ α : Type}
    (st : Stores κ α)
    (h : generalInv st)
    : ∀ (k : κ), (transition st).csmt.get? k = st.kv.get? k := by
  intro k
  simp only [transition, FStore.get?, replayAt]
  exact (h k).1.symm

/-- After transition, general invariant holds.
    (CSMT = KV, journal empty → trivially satisfied.) -/
theorem transition_preserves_generalInv {κ α : Type}
    (st : Stores κ α)
    (h : generalInv st)
    : generalInv (transition st) := by
  intro k
  simp only [transition, FStore.empty, FStore.get?, replayAt]
  constructor
  · exact (h k).1
  · trivial

/-- **QC4**: Multi-cycle: ops → transition → ops → transition.
    Invariant holds and CSMT = KV after each transition. -/
theorem multi_cycle_invariant
    {κ α : Type} [DecidableEq κ]
    (ops₁ ops₂ : List (Op κ α))
    : let st₁ := applyOps Stores.empty ops₁
      let st₂ := transition st₁
      let st₃ := applyOps st₂ ops₂
      let st₄ := transition st₃
      generalInv st₄
      ∧ ∀ (k : κ), st₄.csmt.get? k = st₃.kv.get? k := by
  constructor
  · apply transition_preserves_generalInv
    apply applyOps_preserves_generalInv
    apply transition_preserves_generalInv
    exact general_invariant_holds ops₁
  · apply transition_csmt_eq_kv
    apply applyOps_preserves_generalInv
    apply transition_preserves_generalInv
    exact general_invariant_holds ops₁
