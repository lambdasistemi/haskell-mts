<!--
Sync Impact Report
- Version: 0.0.0 → 1.0.0
- Added: All principles (new constitution)
- Templates requiring updates: ✅ plan-template.md (no changes needed), ✅ tasks-template.md (no changes needed), ✅ spec-template.md (no changes needed)
- Follow-up TODOs: none
-->

# MTS (Merkle Tree Store) Constitution

## Core Principles

### I. Shared Interface, Pluggable Implementations

MTS provides a single `MerkleTreeStore` record parameterized by type
families. Every implementation (CSMT, MPF) MUST satisfy the same
interface and pass the same QuickCheck property suite. Feature parity
is enforced by shared properties, not by convention.

### II. Property-Driven Correctness

QuickCheck properties are the primary correctness mechanism. Every
new capability MUST have a corresponding property in the shared
suite. Properties MUST be implementation-agnostic (run against all
backends). Example-based tests are acceptable only when no reasonable
property can be expressed. Generators MUST be standalone functions
(`genFoo`), not `Arbitrary` instances.

### III. Formal Verification of Invariants

Critical invariants (rollback correctness, journal preservation) MUST
be formalized in Lean 4 before implementation. The design loop is:
discuss, document, formalize in Lean, refine docs, repeat until all
theorems compile with no `sorry`. Lean proofs act as the arbiter of
precision for ambiguous prose.

### IV. Hackage-Ready Quality

All code MUST pass `cabal check`, have Haddock on all exports,
explicit export lists, and module headers. Fourmolu (70-char column
limit, leading commas, leading arrows) is enforced in CI.
`-Wall -Werror` is the baseline. Code that does not format or lint
cleanly MUST NOT be merged.

### V. Reproducible Builds via Nix

The Nix flake is the single source of truth for build tooling. CI
and local development use the same `nix develop` shell. No tool
installation outside Nix. All `source-repository-package` entries
MUST have SHA256 pins in nix32 format. Cachix caches build artifacts.

### VI. Observability and Tracing

Replay phases, long-running operations, and backend interactions
SHOULD provide trace callbacks so consumers can monitor progress.
Tracing MUST be opt-in (callback-based), not tied to a specific
logging framework.

## Architecture Constraints

- **Monorepo structure**: Single `mts.cabal` with sublibraries
  (`mts`, `csmt`, `mpf`, `rollbacks`, test libs). Each sublibrary
  MUST be independently testable.
- **Backend abstraction**: Each implementation provides Pure
  (in-memory, for testing) and RocksDB (persistent, for production)
  backends. New backends MUST satisfy the same property suite.
- **Type families over typeclasses**: The `MerkleTreeStore` record
  uses type families (`MtsKey`, `MtsValue`, `MtsHash`, `MtsProof`)
  to abstract over implementations. Do not introduce typeclasses
  for this abstraction.
- **StrictData by default**: All modules use the `StrictData`
  extension. Lazy fields require explicit annotation and
  justification.
- **Linear git history**: Rebase merge only. No merge commits on
  main.

## Development Workflow

- **Worktree-per-branch**: The main repo stays on `main`. All work
  happens in git worktrees (`/code/haskell-mts-<desc>/`).
- **Issue-first**: Every feature or fix starts with a GitHub issue.
  Issues are added to the `paolino/Planning` project board.
- **Pre-push CI**: Run `just ci` locally before pushing. CI
  round-trips are expensive; catch errors locally.
- **Small focused commits**: Each commit addresses a single concern.
  Conventional Commits format (`feat:`, `fix:`, `refactor:`, etc.).
- **Release-please**: Semantic versioning via commit message
  conventions. `feat:` = minor, `fix:` = patch, `feat!:` = major.
- **Testing locally first**: Always run tests on the local machine
  before relying on CI. If a test cannot run locally, fix the test
  infrastructure.

## Governance

This constitution defines non-negotiable principles for MTS
development. All PRs MUST comply. Amendments require updating this
file, incrementing the version, and documenting the change in the
sync impact report.

**Version**: 1.0.0 | **Ratified**: 2026-03-27 | **Last Amended**: 2026-03-27
