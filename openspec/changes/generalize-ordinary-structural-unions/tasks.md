## 1. Canonical union model

- [ ] 1.1 Generalize member admission to finite detached ordinary value types.
- [ ] 1.2 Implement deterministic flattening, `never` removal, deduplication, canonical ordering, and generic distinctness proof.
- [ ] 1.3 Derive compatibility, inference, Copy, ownership, cleanup, and target layout from one member plan.
- [ ] 1.4 Remove nominal-only union membership and storage branches.

## 2. Compiler pipeline and engines

- [ ] 2.1 Carry exact ordinary member mappings and tags through semantic facts and HIR.
- [ ] 2.2 Encode and verify deterministic tags, payload layouts, narrowing, and cleanup in MIR.
- [ ] 2.3 Implement ordinary-member injection and projection in evaluation, LLVM, and Wasm.
- [ ] 2.4 Publish exact member evidence for failure recovery and shared patterns.

## 3. Verification

- [ ] 3.1 Add scalar, array, executable, nominal, droppable, generic, reordered, and invalid borrowed-member tests.
- [ ] 3.2 Add cross-engine active-payload cleanup and committed-golden determinism coverage.
- [ ] 3.3 Update diagnostics, canonical specs, language docs, and inspectors.
- [ ] 3.4 Run typecheck, Biome, full tests, native acceptance where layout/cleanup is target-specific, and `pnpm check`.
