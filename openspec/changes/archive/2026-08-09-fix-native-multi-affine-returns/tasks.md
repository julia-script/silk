## 1. Characterization

- [x] 1.1 Prove ordinary two-owner aggregate and Effect transport across evaluator, native, and Wasm
- [x] 1.2 Reproduce the original separate-vector VM trap in native debug and release builds
- [x] 1.3 Localize the invalid diagnostic tag to path-insensitive address-root reload after an untaken borrow branch

## 2. Native address-root repair

- [x] 2.1 Initialize every statically selected LLVM address-root slot with complete zero lanes at function entry
- [x] 2.2 Synchronize an address root when its defining operation establishes the semantic value
- [x] 2.3 Remove the compile-time materialization guard and reload only from always-valid private storage

## 3. Regression evidence

- [x] 3.1 Prove taken and untaken exclusive-borrow paths across evaluator, native LLVM, and direct WebAssembly
- [x] 3.2 Prove the original separate-vector VM executes natively with valid exactly-once cleanup
- [x] 3.3 Add fresh-process determinism and retain unaffected scalar, aggregate, slice, and reference-projection gates

## 4. Documentation and verification

- [x] 4.1 Correct the stack-VM findings disposition and both roadmaps with the completed repair evidence
- [x] 4.2 Run focused tests, `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and strict OpenSpec validation
