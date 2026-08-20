## 1. Canonical union model

- [x] 1.1 Generalize member admission to finite detached ordinary value types.
- [x] 1.2 Implement deterministic flattening, `never` removal, deduplication, canonical ordering, and monomorphic generic renormalization.
- [x] 1.3 Derive compatibility, inference, Copy, ownership, cleanup, and target layout from one member plan.
- [x] 1.4 Remove nominal-only union membership and storage branches.

## 2. Compiler pipeline and engines

- [x] 2.1 Carry exact ordinary member mappings and tags through semantic facts and HIR.
- [x] 2.2 Encode and verify deterministic tags, payload layouts, narrowing, and cleanup in MIR.
- [x] 2.3 Implement ordinary-member injection and projection in evaluation, LLVM, and Wasm.
- [x] 2.4 Publish exact member evidence for failure recovery and shared patterns.

## 3. Verification

- [x] 3.1 Add scalar, array, executable, nominal, droppable, generic, reordered, and invalid borrowed-member tests.
- [x] 3.2 Add cross-engine active-payload cleanup and committed-golden determinism coverage.
- [ ] 3.3 Update diagnostics, canonical specs, language docs, and inspectors.
- [ ] 3.4 Run typecheck, Biome, full tests, native acceptance where layout/cleanup is target-specific, and `pnpm check`.
