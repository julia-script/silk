## Why

Silk can pass borrowed slices into ordinary calls but cannot retain a returned view even when its
only origin is one live borrowed parameter. `Vector`, `Bytes`, and other ordinary source
abstractions need this conservative returned-borrow foundation without opening the larger stored
borrow and lifetime-parameter design space.

## What Changes

- Allow an ordinary function to return one shared or exclusive lexical view derived from its single
  borrowed parameter.
- Track the returned view's origin so the owner cannot be mutated, moved, or dropped while that view
  remains live, and reject escape beyond the owner's lexical lifetime.
- Permit shared returned views from shared or exclusive input and exclusive returned views only from
  exclusive input.
- Keep multiple possible borrow origins, effect/service returned borrows, captures, and
  lifetime-bearing fields, arrays, unions, errors, and Effect values unsupported.
- Add the smallest unsafe raw-buffer view intrinsics needed for ordinary source-defined
  `Vector.asSlice` and `Vector.asMutSlice` wrappers.
- Preserve evaluator, LLVM, and direct-Wasm agreement for accepted and rejected programs.

## Capabilities

### New Capabilities

- None.

### Modified Capabilities

- `bootstrap-runtime-slices`: Allow lexical slice locals and returned views with one proven borrowed
  origin while retaining the stored-borrow prohibition.
- `bootstrap-ownership`: Carry borrow provenance and suspend conflicting owner access for the
  returned view's complete live range.
- `bootstrap-intrinsic-boundary`: Admit only the raw-buffer-to-slice representation primitives
  needed by ordinary source wrappers.
- `bootstrap-silk-stdlib`: Add canonical `Vector` shared and exclusive slice accessors without
  compiler knowledge of `Vector`.

## Impact

The change affects slice and function result analysis, borrow provenance and liveness, ownership
diagnostics, HIR/MIR representation, evaluator/native/direct-Wasm lowering, the sealed intrinsic
inventory, canonical `RawBuffer` and `Vector` source, tooling facts, and focused acceptance tests.
It deliberately does not add general lifetime syntax, stored references, borrow-polymorphic
results, or effect-returned borrows.
