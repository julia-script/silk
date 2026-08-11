## 1. Prerequisite and Canonical Source

- [ ] 1.1 Confirm `add-returned-lexical-borrows` is complete and its `Vector` shared/exclusive accessors are available.
- [ ] 1.2 Add nominal move-only `Bytes` canonical Silk source as an ordinary wrapper over `Vector<u8>` with recursive Drop cleanup.
- [ ] 1.3 Export `Bytes` through the canonical module graph and generated standard-library manifest without filesystem or String dependencies.

## 2. Minimal Bytes Operations

- [ ] 2.1 Implement empty construction and `length` with no allocator requirement or failure row.
- [ ] 2.2 Implement copying from `&[u8]` with explicit `OutOfMemory ? &mut Allocator` and exact byte preservation.
- [ ] 2.3 Implement append from `&[u8]` with explicit growth allocation effects and deterministic byte order.
- [ ] 2.4 Implement `asSlice` and `asMutSlice` by returning the wrapped Vector's lexical views without allocation or copying.

## 3. Ownership, Parity, and Tooling

- [ ] 3.1 Add ownership and cleanup tests for moves, successful and failed allocation, mutations through exclusive views, and exact-once release.
- [ ] 3.2 Add evaluation, native LLVM, and direct-Wasm parity fixtures covering arbitrary non-UTF-8 octets, copy, append, length, and borrowed access.
- [ ] 3.3 Add a direct-Wasm artifact test proving `Bytes` introduces no operating-system imports.
- [ ] 3.4 Add hover, definition, occurrences, completion, and navigation tests proving `Bytes` resolves to canonical Silk source.
- [ ] 3.5 Regenerate committed manifests and goldens, run `pnpm check`, and run `pnpm release:candidate` if package contents or exports changed.

