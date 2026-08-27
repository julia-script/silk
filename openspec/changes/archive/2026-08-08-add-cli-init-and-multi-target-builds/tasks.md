## 1. Project Manifest and Build Selection

- [x] 1.1 Extend the `Project` actor with required semantic-version metadata and materialized build defaults for backend, target selectors, and output directory
- [x] 1.2 Add focused project-decoding tests for sparse defaults, explicit `[build]` values, malformed versions, empty target arrays, invalid backend ids, and invalid output directories
- [x] 1.3 Add a target-selector actor that resolves `host`, validates canonical ids, and deduplicates resolved targets in first-seen order
- [x] 1.4 Migrate every repository and release-candidate `silk.toml` fixture to include `version = "0.1.0"`

## 2. Backend Identity and Artifact Model

- [x] 2.1 Add stable `llvm` and `wasm` ids to the backend contract and replace target-first registry lookup with explicit id resolution plus compatibility validation
- [x] 2.2 Split backend output into closed LLVM-bitcode and final-WebAssembly-module artifact variants with shared target, symbol, and provenance data
- [x] 2.3 Extend LLVM compatibility to `wasm32-unknown-unknown` and emit target-correct deterministic IR and bitcode retaining `silk_main`
- [x] 2.4 Update the direct WebAssembly backend to return the final-module artifact variant without changing its validated bytes or WAT determinism
- [x] 2.5 Add backend registry, incompatible-pair, artifact-kind, and fresh-process LLVM-Wasm determinism tests

## 3. Durable Artifact Finalization

- [x] 3.1 Deepen the single Clang-owning toolchain boundary so it plans native and WebAssembly finalization without duplicating child-process integration
- [x] 3.2 Implement pinned LLVM-to-Wasm finalization with structured arguments, no native shim or host libraries, and exported `silk_main`
- [x] 3.3 Add an atomic filesystem commit path for validated direct-WebAssembly bytes that never invokes Clang
- [x] 3.4 Generalize driver requests and closed outcomes with explicit backend selection, artifact kind, durable path, and finalization-stage provenance
- [x] 3.5 Route native LLVM, LLVM Wasm, and direct Wasm artifacts through their compatible finalizers while preserving existing native behavior
- [x] 3.6 Add replaceable-service tests for command planning and failure provenance plus integration tests that instantiate both Wasm outputs and call `silk_main`
- [x] 3.7 Add evaluator/direct-Wasm/LLVM-Wasm parity coverage for successful results and traps, keeping backend-specific byte determinism assertions

## 4. Multi-Target Project Workflows

- [x] 4.1 Add a `BuildBatch` actor that preflights one backend against all resolved targets and returns ordered immutable single-target `BuildPlan` values
- [x] 4.2 Change project destinations to `build/<backend>/<target>/<profile>/<package>[.wasm]` and cover host resolution, extension selection, custom output roots, and backend collision avoidance
- [x] 4.3 Make `--backend` optional and `--target` repeatable for project compilation commands, with command-line targets replacing manifest targets
- [x] 4.4 Execute build plans sequentially, attempt every preflighted target, retain successful sibling artifacts, and aggregate exit classes with `2 > 1 > 0`
- [x] 4.5 Run project checks once per resolved target with target-qualified diagnostics and no backend, Clang, linker, or artifact work
- [x] 4.6 Keep `silk run` host-only, reject non-runnable backends before compilation, and preserve executed-program exit status
- [x] 4.7 Add deterministic per-target summaries and CLI/workflow tests for ordered success, mixed rejection, operational failure, duplicate targets, and invalid preflight

## 5. Safe Project Initialization

- [x] 5.1 Add the `ProjectInitializer` actor with typed options/errors, package-name derivation, explicit `--name`, managed-path planning, and complete collision preflight
- [x] 5.2 Generate the sparse manifest, exact `/build/` ignore rule, and canonical `src/main.silk` returning zero
- [x] 5.3 Implement exact-byte `.gitignore` merging and scoped rollback that restores pre-existing content and removes only invocation-created paths after failure, defect, or interruption
- [x] 5.4 Add initialization tests for new and existing directories, name overrides, invalid names, unrelated files, both protected-file collisions, ignore-rule idempotence, and injected mid-write rollback
- [x] 5.5 Register `silk init [path] [--name]`, add it to root help, and test the generated project through load, check, build, and run

## 6. Documentation, Migration, and Verification

- [x] 6.1 Update the CLI README with initialization, sparse and expanded manifests, backend/target precedence, multi-target examples, visible artifact paths, batch summaries, and migration notes
- [x] 6.2 Update LSP fixtures, editor tasks, release-candidate consumers, help assertions, and package exports/subpaths for the evolved project and toolchain actors
- [x] 6.3 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`, fixing only failures introduced by this change and recording any pre-existing failures
- [x] 6.4 Run `pnpm check` and `pnpm release:candidate` because package contents, exports, CLI behavior, and consumer fixtures change
- [x] 6.5 Validate the completed OpenSpec change and confirm every scenario has corresponding automated coverage or an explicit release-environment integration gate
