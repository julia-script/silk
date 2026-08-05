## 1. Planning surface

- [x] 1.1 Create `packages/compiler/src/ToolchainPlan.ts` (pure, browser-safe): fixed
  optimization profiles, planned object and link commands as structured argv data, shim source

## 2. Orchestration

- [x] 2.1 Create `packages/compiler/src/NativeToolchain.ts` (deep import only): pinned
  `Toolchain`, named build scopes with path-backed artifacts, recursive removal at exit, explicit
  save-temps promotion
- [x] 2.2 `emitObject`: write bitcode, invoke pinned Clang `-c` per profile, outcome as data with
  command provenance on success and failure
- [x] 2.3 `compileShim` and the `NativeLinker` service with `ClangLinker`: input validation,
  structured driver invocation, executable written to the durable destination
- [x] 2.4 Tests against the pinned Clang: release object emission, failure provenance, scope
  cleanup, promotion, shim ABI (linked executable exits with the program result)

## 3. Package surface

- [x] 3.1 Exports map gains `./ToolchainPlan` (also in the index) and `./NativeToolchain` (deep
  only); release-candidate surface with the root-namespace exemption

## 4. Inspector lab

- [x] 4.1 Create the direct-link `/docs/labs/toolchain` lab: planned commands per profile,
  bitcode size, shim source, scope lifecycle; profile toggle
- [x] 4.2 Lab tests: release plan shows `-c`/`-O2`, debug plan shows `-O0 -g`, link plan combines
  program, shim, and destination

## 5. Verification

- [x] 5.1 Full compiler and docs suites pass; `pnpm check` and release-candidate green
- [x] 5.2 `openspec validate orchestrate-native-toolchain --type change --strict` passes
