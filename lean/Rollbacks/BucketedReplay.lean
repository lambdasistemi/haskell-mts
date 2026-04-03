import Rollbacks.JournalInvariant

/-!
# Bucketed Replay and Crash Recovery

Extends the single-step `transition` model with:

1. **Chunked replay**: journal entries applied in chunks
   (associativity of `applyEntries` over append)
2. **Bucketed parallelism**: entries partitioned by key prefix,
   applied independently per bucket (commutativity for disjoint
   keys)
3. **Crash recovery**: crash after partial replay, recovery
   completes the remaining entries

## Main theorems

- `applyEntries_append`: chunked replay = full replay
- `applyEntry_comm_disjoint`: entries at different keys commute
- `applyEntries_perm_disjoint`: disjoint-key lists commute
- `crash_recovery_correct`: partial + remaining = full replay
-/

-- ============================================================
-- Chunked replay: associativity of applyEntries
-- ============================================================

/-- `applyEntries` distributes over list append.
    This is the foundation for chunked replay:
    replaying chunks 1..K then K+1..M = replaying all. -/
theorem applyEntries_append {κ α : Type} [DecidableEq κ]
    (csmt : FStore κ α)
    (entries₁ entries₂ : List (κ × JEntry α))
    : applyEntries csmt (entries₁ ++ entries₂)
      = applyEntries (applyEntries csmt entries₁) entries₂ := by
  induction entries₁ generalizing csmt with
  | nil => simp [applyEntries]
  | cons e es ih =>
    simp only [List.cons_append, applyEntries]
    exact ih (applyEntry csmt e.1 e.2)

-- ============================================================
-- Disjoint-key commutativity
-- ============================================================

/-- Two entries at different keys commute. -/
theorem applyEntry_comm_disjoint {κ α : Type} [DecidableEq κ]
    (csmt : FStore κ α)
    (k₁ k₂ : κ) (e₁ : JEntry α) (e₂ : JEntry α)
    (h : k₁ ≠ k₂)
    : applyEntry (applyEntry csmt k₁ e₁) k₂ e₂
      = applyEntry (applyEntry csmt k₂ e₂) k₁ e₁ := by
  funext k
  show (applyEntry _ k₂ e₂).get? k = (applyEntry _ k₁ e₁).get? k
  by_cases hk1 : k = k₁
  · subst hk1
    rw [applyEntry_at_key, applyEntry_other_key _ k₂ k e₂ h,
        applyEntry_at_key]
  · by_cases hk2 : k = k₂
    · subst hk2
      rw [applyEntry_at_key,
          applyEntry_other_key _ k₁ k e₁ hk1,
          applyEntry_at_key]
    · rw [applyEntry_other_key _ k₂ k e₂ hk2,
          applyEntry_other_key _ k₁ k e₁ hk1,
          applyEntry_other_key _ k₁ k e₁ hk1,
          applyEntry_other_key _ k₂ k e₂ hk2]

/-- Applying a single entry then a disjoint list = applying
    the list then the entry. -/
theorem applyEntry_comm_disjoint_list
    {κ α : Type} [DecidableEq κ]
    (csmt : FStore κ α)
    (k : κ) (e : JEntry α)
    (entries : List (κ × JEntry α))
    (h : ∀ p ∈ entries, p.1 ≠ k)
    : applyEntries (applyEntry csmt k e) entries
      = applyEntry (applyEntries csmt entries) k e := by
  induction entries generalizing csmt with
  | nil => simp [applyEntries]
  | cons p ps ih =>
    simp only [applyEntries]
    have hpk : p.1 ≠ k := h p (.head ps)
    have hps : ∀ q ∈ ps, q.1 ≠ k :=
      fun q hq => h q (.tail p hq)
    rw [applyEntry_comm_disjoint csmt k p.1 e p.2
          hpk.symm]
    exact ih (applyEntry csmt p.1 p.2) hps

/-- Two disjoint-key entry lists commute:
    applying list₁ then list₂ = applying list₂ then list₁,
    when no key appears in both lists. -/
theorem applyEntries_perm_disjoint
    {κ α : Type} [DecidableEq κ]
    (csmt : FStore κ α)
    (entries₁ entries₂ : List (κ × JEntry α))
    (h : ∀ p₁ ∈ entries₁, ∀ p₂ ∈ entries₂, p₁.1 ≠ p₂.1)
    : applyEntries (applyEntries csmt entries₁) entries₂
      = applyEntries (applyEntries csmt entries₂) entries₁ := by
  induction entries₁ generalizing csmt with
  | nil => simp [applyEntries]
  | cons p ps ih =>
    simp only [applyEntries]
    have hp : ∀ q ∈ entries₂, p.1 ≠ q.1 :=
      fun q hq => h p (.head ps) q hq
    have hps : ∀ r ∈ ps, ∀ q ∈ entries₂, r.1 ≠ q.1 :=
      fun r hr q hq => h r (.tail p hr) q hq
    rw [ih (applyEntry csmt p.1 p.2) hps]
    rw [applyEntry_comm_disjoint_list csmt p.1 p.2
          entries₂ (fun q hq => (hp q hq).symm)]

-- ============================================================
-- Folding applyEntries over a list of buckets
-- ============================================================

/-- Folding `applyEntries` over buckets = applying
    the concatenation of all buckets. -/
theorem foldl_applyEntries_eq_flatten
    {κ α : Type} [DecidableEq κ]
    (csmt : FStore κ α)
    (buckets : List (List (κ × JEntry α)))
    : List.foldl (fun c b => applyEntries c b) csmt buckets
      = applyEntries csmt buckets.flatten := by
  induction buckets generalizing csmt with
  | nil => rfl
  | cons b bs ih =>
    simp only [List.foldl, List.flatten_cons]
    rw [ih (applyEntries csmt b), applyEntries_append]

-- ============================================================
-- Crash recovery
-- ============================================================

/-- Model a crash during replay.

    The journal entries are split into `applied` (already
    replayed before crash) and `remaining` (not yet replayed).

    After crash, the CSMT has `applied` entries integrated.
    Recovery replays `remaining` entries. -/
structure CrashState (κ α : Type) where
  /-- Store state before transition -/
  preTransition : Stores κ α
  /-- Entries already applied before crash -/
  applied : List (κ × JEntry α)
  /-- Entries not yet applied -/
  remaining : List (κ × JEntry α)

/-- The CSMT state after a crash: partial replay applied. -/
def crashedCsmt {κ α : Type} [DecidableEq κ]
    (cs : CrashState κ α) : FStore κ α :=
  applyEntries cs.preTransition.csmt cs.applied

/-- Recovery: apply remaining entries to crashed CSMT. -/
def recoverCsmt {κ α : Type} [DecidableEq κ]
    (cs : CrashState κ α) : FStore κ α :=
  applyEntries (crashedCsmt cs) cs.remaining

/-- **Crash recovery theorem**: recovering from a crash at
    any point produces the same CSMT as a clean full replay.

    This is a direct consequence of `applyEntries_append`:
    replaying `applied ++ remaining` = replaying `applied`
    then `remaining`. -/
theorem crash_recovery_correct {κ α : Type} [DecidableEq κ]
    (cs : CrashState κ α)
    : recoverCsmt cs
      = applyEntries cs.preTransition.csmt
          (cs.applied ++ cs.remaining) := by
  simp only [recoverCsmt, crashedCsmt]
  rw [applyEntries_append]

/-- **Crash recovery preserves KV agreement**: after crash
    and recovery, the CSMT matches KV — same as a clean
    transition. -/
theorem crash_recovery_gives_kv {κ α : Type} [DecidableEq κ]
    (cs : CrashState κ α)
    (hinv : generalInv cs.preTransition)
    (hreplay : ∀ k : κ, replayAt cs.preTransition k =
      (applyEntries cs.preTransition.csmt
        (cs.applied ++ cs.remaining)).get? k)
    : ∀ k : κ,
        (recoverCsmt cs).get? k
        = cs.preTransition.kv.get? k := by
  intro k
  rw [crash_recovery_correct, ← hreplay k]
  exact replay_gives_csmt_eq_kv cs.preTransition hinv k
