## 1. Reportability and Entry Discovery

- [x] 1.1 Add the compiler-sealed `Report` nominal type and operation-free marker conformance validation with focused declaration-index tests
- [x] 1.2 Extend entry discovery with explicit ordinary/effectful variants, `Unit`/requirement/reportability checks, and actionable unavailable reasons
- [x] 1.3 Include effect-entry failure types and their cleanup-hook dependencies in deterministic instance reachability tests

## 2. MIR Entry Closure

- [x] 2.1 Add explicit machine-entry metadata and a generated effect-closing operation/function to the MIR model and textual encoder
- [x] 2.2 Verify entry targets, scalar adapter contracts, normalized failure tags, typed payload locals, and cleanup plans as deterministic violations
- [x] 2.3 Lower effectful entries to the generated adapter while preserving ordinary entry semantics and deterministic function identity
- [x] 2.4 Extend bootstrap evaluation with effect-entry success and structured unhandled-failure outcomes, including payload cleanup

## 3. Backend Parity

- [x] 3.1 Select `silk_main` from explicit MIR entry identity and retain ordered report identities in backend artifacts
- [x] 3.2 Lower effect-entry closure through the LLVM backend with exact success, failure-tag, trap, and cleanup behavior
- [x] 3.3 Lower the same import-free closure through the direct WebAssembly backend and add native/Wasm parity tests

## 4. Native Reporting

- [x] 4.1 Generate ordinary pass-through or effect-reporting C shim source from artifact termination metadata using byte-array report literals
- [x] 4.2 Thread termination metadata through driver finalization and test success, deterministic stderr reporting, status normalization, invalid tags, and failed writes

## 5. Verification and Release Surface

- [x] 5.1 Update affected MIR, backend, driver, CLI, golden, and release-candidate fixtures without compatibility shims
- [x] 5.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`, fixing every change-caused failure
- [x] 5.3 Validate the OpenSpec change strictly and confirm every implementation task is complete
