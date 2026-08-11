## Why

Portable I/O and future text facilities need an owned arbitrary-byte value rather than exposing
`Vector<u8>` as every public domain result. Silk can build that value entirely in ordinary source
once returned lexical slices are available.

## What Changes

- Add nominal owned `Bytes` as a source-defined wrapper over `Vector<u8>`.
- Provide minimal construction, copy, length, append, shared-slice, exclusive-slice, move, and Drop
  behavior.
- Keep bytes encoding-neutral: no UTF-8 claim, formatting policy, filesystem semantics, or hidden
  compiler collection behavior.
- Preserve allocation failure and `Allocator` requirements only on operations that grow or copy
  owned storage.
- Verify cleanup and observable behavior across evaluation, LLVM, and direct Wasm.
- Require `add-returned-lexical-borrows` before implementation.

## Capabilities

### New Capabilities

- `bootstrap-owned-bytes`: The ownership, allocation, borrowing, and sequence behavior of the
  canonical `Bytes` value.

### Modified Capabilities

- `bootstrap-silk-stdlib`: Ship `Bytes` as canonical navigable Silk source built from `Vector<u8>`
  and ordinary allocation contracts.

## Impact

The change affects canonical standard-library source and manifest generation, owned-sequence
composition, allocation/error rows, Drop cleanup, evaluator/native/direct-Wasm acceptance,
documentation, completion, hover, occurrences, and navigation. It adds no new collection-shaped
compiler primitive and makes no String or encoding decision.
