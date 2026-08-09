## 1. Characterization

- [ ] 1.1 Add Copy-only, empty multi-affine, and allocated multi-affine return fixtures with distinct field observations
- [ ] 1.2 Assert canonical result-lane paths, MIR verification, evaluator cleanup, LLVM IR shape, and direct Wasm behavior
- [ ] 1.3 Reproduce the native failure in debug and release and identify the first divergent boundary

## 2. Native return repair

- [ ] 2.1 Retain the compiler-selected result lanes and canonical paths in the LLVM declared-function record
- [ ] 2.2 Drive callee aggregate construction and caller extraction symmetrically from that result shape
- [ ] 2.3 Reject internal result-lane mismatches without Vector-, generic-owner-, or cleanup-specific lowering

## 3. Regression evidence

- [ ] 3.1 Prove empty and allocated multi-affine returns across evaluator, native LLVM, and direct WebAssembly
- [ ] 3.2 Assert balanced evaluator acquisition, release, and Drop traces plus unaffected scalar and Copy-aggregate execution
- [ ] 3.3 Add fresh-process determinism for the repaired calling shape, MIR, LLVM, and Wasm artifacts

## 4. Documentation and verification

- [ ] 4.1 Update the stack-VM findings disposition and both roadmaps with the completed repair evidence
- [ ] 4.2 Run focused tests, `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and strict OpenSpec validation
