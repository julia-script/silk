## Why

Silk now knows nominal struct identities, fields, dependencies, and physical layouts before backend
work, but programs still cannot create or use a struct value. Construction and field projection are
the smallest runtime aggregate slice that turns those declaration facts into useful language data.

## What Changes

- Add lossless, recoverable labeled struct literals such as `Token { kind: kind, lexeme: move
lexeme }` and chained field projections such as `token.span.start`.
- Restrict raw struct literals to the module defining the nominal type; external modules construct
  values through ordinary public actor functions such as `Token.make`.
- Require every literal field exactly once, permit source field order to differ from declaration
  order, and retain unknown, duplicate, missing, inaccessible, and mistyped field states with exact
  diagnostics and provenance.
- Elaborate construction and projection into typed HIR over canonical nominal and field identities,
  with whole-value affine moves and no partial move out of a non-Copy struct.
- Extend instance discovery, compiler-owned target ABI planning, MIR, cleanup, evaluation, native
  LLVM emission, and direct WebAssembly emission for nominal aggregate parameters, results, locals,
  construction, projection, and deterministic declaration-order cleanup.
- Expose struct-value syntax, semantic mappings, ownership, MIR, evaluation, layouts, and emitted
  representations through the facade and the unified `/labs` workbench.
- Keep struct patterns and destructuring, `Copy` declarations, mutation and borrows, partial moves,
  arrays, unions, pointers, and general unsafe aggregate construction out of this change.

## Capabilities

### New Capabilities

- `bootstrap-struct-values`: Complete module-owned struct construction, typed field places and
  reads, whole-value affine ownership, aggregate calls and returns, and declaration-order cleanup.

### Modified Capabilities

- `bootstrap-syntax`: Parse and recover labeled struct literals and chained field projections
  losslessly.
- `bootstrap-semantic-facts`: Resolve literal targets, field initializers, and projected field paths
  to canonical nominal and field identities with explicit unavailable states.
- `bootstrap-hir`: Represent typed aggregate construction and field projection as canonical HIR
  operations with exact provenance.
- `bootstrap-ownership`: Classify nominal values as move-only, reject partial moves, and plan
  declaration-order field cleanup for live whole values.
- `bootstrap-instances`: Discover nominal runtime types and aggregate-bearing function instances
  from reachable construction, projection, parameters, and results.
- `bootstrap-target-layout`: Add target-selected aggregate parameter/result realization facts and
  reuse catalog entries in reachable runtime plans without backend-owned ABI decisions.
- `bootstrap-mir`: Carry nominal logical types and explicit aggregate construction/projection
  operations through verified, deterministic backend-neutral MIR.
- `bootstrap-evaluation`: Evaluate aggregate values, moves, calls, returns, and projections with
  deterministic traces.
- `bootstrap-backend`: Realize compiler-owned aggregate layouts and calling conventions in native
  LLVM and direct WebAssembly output with interpreter parity.
- `bootstrap-compiler-driver`: Compile and differentially verify programs whose internal functions
  pass and return nominal aggregate values.
- `bootstrap-analysis-facade`: Publish immutable struct-value, ownership, ABI, MIR, and evaluation
  queries without tooling reconstruction.
- `bootstrap-syntax-inspector`: Inspect construction and projection end to end in the unified
  `/labs` workbench with browser-local presets and accessible text equivalents.

## Impact

The compiler's syntax, semantic facts, HIR, ownership, discovery, target layout, MIR, evaluator,
LLVM backend, WebAssembly backend, driver, and analysis facade will gain runtime nominal values.
Public data-model types and deterministic encodings will expand, as will docs presets and package
export validation. No external dependency is introduced, and physical layout or aggregate ABI
authority does not move into either backend.
