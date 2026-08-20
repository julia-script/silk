## 1. Sealed Copy evidence

- [x] 1.1 Define one zero-operation sealed Copy conformance fact and deterministic proof state.
- [x] 1.2 Validate all stored fields, Drop exclusion, allocation ownership, recursive availability, and conflicting evidence.
- [x] 1.3 Route generic bounds, arrays, unions, layouts, and ownership categories through the single proof.
- [x] 1.4 Delete structural Copy inference and blanket nominal move-only fallbacks.

## 2. Executable aggregate ownership

- [x] 2.1 Derive represented callable ownership from realized fields and retain capture-access constraints separately.
- [x] 2.2 Derive represented Effect ownership from realized fields and retain run/capture rules separately.
- [x] 2.3 Apply ordinary aggregate partial moves and exact cleanup to executable-bearing nominals.
- [x] 2.4 Retire `OWN0013` and update `OWN0014`/`OWN0015` to report only access-specific violations.

## 3. Verification

- [x] 3.1 Add valid Copy, invalid Drop/allocation Copy, generic derivation, array/union, callable, Effect, and partial-move tests.
- [x] 3.2 Verify evaluator/Wasm/native cleanup only where representation behavior differs.
- [x] 3.3 Update diagnostics, canonical specs, docs, and every existing ownership fixture.
- [x] 3.4 Run typecheck, Biome, full tests, native acceptance, and `pnpm check`.
