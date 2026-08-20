## 1. Entry contracts

- [x] 1.1 Replace entry discovery with the confirmed public ordinary `()`, ordinary `i32`, and closed effectful unit shapes.
- [x] 1.2 Diagnose private entry, invalid return, typed failure openness, and every unresolved requirement at the semantic boundary.
- [x] 1.3 Remove `Report` declarations, conformance exceptions, entry gates, generated docs assumptions, tests, and wording.
- [x] 1.4 Implement zero, one, and ordinary custom-status mappings exactly.

## 2. Structured termination

- [x] 2.1 Define one target-neutral success/failure/trap outcome with provenance, logical path, and causal history.
- [x] 2.2 Produce and preserve the outcome in evaluation, MIR, LLVM, Wasm, optimization, suspension, and cleanup paths.
- [x] 2.3 Build stable logical frames independent of backend driver/resume frames.
- [x] 2.4 Expose data-only embedding results and standalone CLI/native rendering.
- [x] 2.5 Derive adapter and runtime linkage from reachable entry/report inventory and remove ambient machinery.

## 3. Verification

- [x] 3.1 Add ordinary/effect success, typed failure, trap, private main, open requirement, recovery history, and suspension trace tests.
- [x] 3.2 Add evaluator/native/Wasm parity and trivial-program pay-for-use structural checks.
- [x] 3.3 Update diagnostics, canonical specs, language docs, CLI docs, and generated artifacts.
- [ ] 3.4 Run typecheck, Biome, full compiler tests, native acceptance, release-candidate checks where package output changes, and `pnpm check`.
