## 1. Prerequisite and Canonical Source

- [x] 1.1 Confirm `add-returned-lexical-borrows` is complete and its `Vector` shared/exclusive accessors are available.
- [x] 1.2 Add nominal move-only `Bytes` canonical Silk source as an ordinary wrapper over `Vector<u8>` with recursive Drop cleanup.
- [x] 1.3 Export `Bytes` through the canonical module graph and generated standard-library manifest without filesystem or String dependencies.
- [x] 1.4 Preserve stable field-projection paths when an ordinary wrapper borrows a field through `&T` or `&mut T`, with evaluator, native LLVM, and direct-Wasm parity.

## 2. Minimal Bytes Operations

- [x] 2.1 Implement empty construction and `length` with no allocator requirement or failure row.
- [x] 2.2 Implement copying from `&[u8]` with explicit `OutOfMemory ? &mut Allocator` and exact byte preservation.
- [x] 2.3 Implement append from `&[u8]` with explicit growth allocation effects and deterministic byte order.
- [x] 2.4 Implement `asSlice` and `asMutSlice` by returning the wrapped Vector's lexical views without allocation or copying.

## 3. Ownership, Parity, and Tooling

- [x] 3.1 Add ownership and cleanup tests for moves, successful and failed allocation, mutations through exclusive views, and exact-once release.
- [x] 3.2 Add evaluation, native LLVM, and direct-Wasm parity fixtures covering arbitrary non-UTF-8 octets, copy, append, length, and borrowed access.
- [x] 3.3 Add a direct-Wasm artifact test proving `Bytes` introduces no operating-system imports.
- [x] 3.4 Add hover, definition, occurrences, completion, and navigation tests proving `Bytes` resolves to canonical Silk source.
- [x] 3.5 Regenerate committed manifests and goldens, run `pnpm check`, and run `pnpm release:candidate` if package contents or exports changed.
