## Why

`ProjectAnalysis.revise` can reuse unchanged syntax, but it still has no sound way to distinguish a
module-local edit from a dependency change that alters another module's meaning. Before semantic
artifacts can be reused, the compiler needs a deterministic definition of an import-visible module
surface and a tested invalidation oracle that remains correct for cycles and malformed source.

## What Changes

- Add an immutable compiler-owned semantic surface for each analyzed module, derived from the
  completed declaration and resolution facts that another module can observe.
- Define semantic-surface equality independently of hashing and exclude body facts, source spans,
  object identity, and map iteration order from that equality.
- Add an SCC-aware invalidation plan for adjacent accepted project revisions. Local semantic-input
  changes invalidate their component; changed dependency surfaces propagate only to dependent
  components; stable surfaces stop propagation.
- Record deterministic per-module semantic observations and reuse/recompute counts through
  `ProjectAnalysis` and the existing phase-report infrastructure.
- Keep every semantic phase globally recomputed in this change. The invalidation plan is a
  correctness oracle for the next reuse change, not permission to expose prior semantic facts.

## Capabilities

### New Capabilities

- `bootstrap-module-semantic-surface`: Defines deterministic import-visible module surfaces,
  equality, dependency components, and conservative invalidation planning.

### Modified Capabilities

- `bootstrap-project-analysis`: Adds adjacent-revision semantic invalidation observations while
  preserving complete immutable frontend recomputation and atomic root views.

## Impact

- Compiler actors: new semantic-surface and invalidation-plan modules; integration in `Pipeline`
  and `ProjectAnalysis`.
- Public compiler data: project revisions expose immutable module surfaces and semantic revision
  observations.
- Tests: focused unrelated, body-only, public-contract, visibility, struct-shape, transitive,
  cyclic, malformed-source, and fresh-process determinism fixtures.
- LSP scheduling and protocol behavior remain unchanged; the LSP consumes the compiler-owned
  project result without owning invalidation logic.
