## Why

Project analysis now runs once per accepted LSP revision, but every revision still reparses every
module and the compiler has no safe way to relate unchanged concrete syntax across an edit. The
existing source-qualified preorder identities remain excellent canonical identities within a
snapshot; this change adds measured reuse and adjacent-revision correspondence without weakening
snapshot coherence or committing to a persistent incremental parser.

## What Changes

- Add a compiler-owned syntax-correspondence artifact that deterministically relates unchanged,
  structurally equal subtrees between adjacent syntax files for the same logical source.
- Make correspondence conservative: ambiguous duplicate siblings, changed subtrees, and foreign
  source identities remain unmatched rather than receiving a guessed identity.
- Reuse the exact immutable `SyntaxFile` object when a module's source identity, origin, and bytes
  are unchanged between accepted project revisions.
- Let project analysis revise a prior accepted analysis, report fresh/reused/changed syntax per
  module, and retain correspondence for changed modules while recomputing semantic facts into one
  coherent new snapshot.
- Pass only the last atomically committed project analysis into the next LSP analysis job; stale or
  partially computed revisions never become reuse inputs.
- Keep incremental lexing/parsing, persistent green/red storage, and semantic artifact
  invalidation outside this change.

## Capabilities

### New Capabilities

- `bootstrap-syntax-correspondence`: Deterministic, conservative correspondence between concrete
  syntax elements in adjacent revisions of one logical source.

### Modified Capabilities

- `bootstrap-project-analysis`: Project revisions reuse unchanged syntax artifacts and expose
  per-module syntax-revision evidence without reusing stale semantic facts.
- `language-server-synchronization`: The scheduler supplies only its last complete committed
  project result as the reuse basis for a subsequent accepted analysis.

## Impact

The compiler gains a public `SyntaxCorrespondence` actor and a revision-aware `ProjectAnalysis`
entry point. Module-closure loading gains an optional prior closure for syntax reuse. The LSP
`ProjectSession` analysis callback receives the previous committed result, and workspace analysis
retains the shared `ProjectAnalysis` value with each document view. Compiler exports,
release-candidate validation, and focused performance/correctness tests expand accordingly.
