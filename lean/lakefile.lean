import Lake
open Lake DSL

package «rollbacks» where
  leanOptions := #[
    ⟨`autoImplicit, false⟩
  ]

@[default_target]
lean_lib Rollbacks where
  srcDir := "."
