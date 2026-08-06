## Why

The completed algorithmic-language slice has strong feature-by-feature coverage, but it has not yet
proved that modules, aggregates, arrays, mutation, loops, unions, and matching compose into one
compiler-shaped algorithm. Before choosing a memory roadmap, we need one acceptance program whose
first genuine limitation can guide that next slice.

## What Changes

- Add a small multi-module Silk implementation of a canonical remaining-member coverage fold over
  fixed compiler-shaped data.
- Run the unchanged program through logical evaluation, native LLVM emission, and direct
  WebAssembly emission with one agreed result and deterministic artifacts.
- Add the program to the unified `/labs` presets so its syntax, semantic facts, HIR, ownership,
  instances, layout, MIR, evaluation, and backend realizations can be inspected together.
- Keep the acceptance change observational: it may add fixtures, facade-backed projections, and
  tests, but it must not add or relax language semantics. Any missing capability is recorded as the
  input to the next roadmap rather than hidden inside this change.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-compiler-driver`: Require one composed compiler-shaped algorithm to retain
  interpreter/native/WebAssembly parity and deterministic artifacts.
- `bootstrap-syntax-inspector`: Require the composed acceptance program to be available through the
  existing unified workbench without a standalone inspector.

## Impact

- Compiler corpus fixtures and differential/determinism tests under `packages/compiler/test`.
- Existing facade-backed `/labs` presets and preset tests under `apps/docs/app/labs`.
- `roadmaps/project.md`, updated only with the limitation demonstrated by the accepted program.
- No compiler API, syntax, semantic, ownership, MIR, layout, evaluator, or backend contract changes.
