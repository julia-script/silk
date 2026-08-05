## 1. Canonical Target and Layout Actors

- [x] 1.1 Add the `Target` actor with the three native profiles, explicit WebAssembly profile,
  supported-host resolution, native/WebAssembly classification, deterministic encoding, and typed
  unsupported-target outcomes; cover every profile and failure with `@effect/vitest` tests.
- [x] 1.2 Add the `Layout` actor with immutable target-owned plans, canonical `I32` and `Bool`
  entries, reachability-based planning from discovered instances, deterministic verification and
  encoding, and tests for ordering, unused types, malformed entries, and all four targets.
- [x] 1.3 Export the new public actors through the compiler's explicit barrel and package subpaths,
  removing `Mir.TargetLayout` and every hardcoded/default layout value instead of adding
  compatibility aliases.

## 2. Analysis, Planning, and MIR

- [x] 2.1 Extend compilation and single-source snapshot requests with an optional canonical target
  ID; resolve the target at snapshot construction and preserve explicit unavailable states through
  the analysis facade.
- [x] 2.2 Run `Layout.plan` after `Instances.Discovery` and before `Lower.lower`, store the target and
  layout result on `Analysis.Snapshot`, and add facade queries proving MIR, evaluation, and codegen
  share that exact plan.
- [x] 2.3 Make `Lower.lower` require a completed plan and make every `Mir.Module` carry exactly one
  `Layout.Plan`; update hand-built samples and every direct MIR constructor.
- [x] 2.4 Extend MIR verification with layout validity and runtime-type coverage rules, and extend
  MIR encoding with the canonical target and ordered entries; update unit tests and all MIR goldens
  with fresh-process determinism coverage.
- [x] 2.5 Keep the interpreter on logical values while requiring verified target-aware MIR, and add
  positive shared-plan and negative malformed-plan evaluation tests.

## 3. Backend Contract and Both Emitters

- [x] 3.1 Replace the backend interface with one effectful emission operation over target-aware MIR
  and a codegen request; add declared target support, typed invalid-MIR and target-incompatibility
  failures, and target identity on emitted artifacts.
- [x] 3.2 Migrate the LLVM backend to read its target, `I32`, and `Bool` representations exclusively
  from `program.layout`, retaining target-triple emission and debug/release determinism without a
  fallback scalar map.
- [x] 3.3 Migrate the direct WebAssembly backend to accept only explicit
  `wasm32-unknown-unknown` MIR, validate and realize the same scalar plan, and reject native MIR
  before builder construction.
- [x] 3.4 Convert backend and facade call sites to the typed Effect channel, update tests to
  `@effect/vitest` Effect forms where needed, and refresh LLVM IR, WAT, bitcode, and WebAssembly
  digest goldens only after semantic parity tests pass.

## 4. Native Toolchain and Driver

- [x] 4.1 Remove `NativeToolchain.hostLayout`; carry the compiler-selected native target through
  backend, bitcode, object, runtime-shim, and link artifacts with typed compatibility checks.
- [x] 4.2 Make object and link command planning include the exact canonical native target, reject
  target-mismatched inputs before process invocation, and test plans for all three native profiles
  without requiring cross-target sysroots.
- [x] 4.3 Update the native driver to accept only native target IDs, default through `Target.host`,
  run and report layout between instances and MIR, and return closed target/layout/backend failures
  without invoking later stages.
- [x] 4.4 Extend driver and differential tests for supported-host native execution, unsupported and
  WebAssembly native-driver requests, phase ordering, artifact target propagation, and unchanged
  interpreter/native behavior.

## 5. Facade-Only Inspection

- [x] 5.1 Update the MIR CFG lab to display the snapshot's canonical target and ordered layout plan
  beside both hand-built and lowered MIR encodings, with component tests for matching values.
- [x] 5.2 Update the LLVM IR lab to display the same facade-owned target and scalar entries beside
  their emitted LLVM representation, including the planned four-byte `Bool` proof.
- [x] 5.3 Build the WebAssembly lab from an explicit `wasm32-unknown-unknown` snapshot and display
  its shared plan beside WAT/binary output; prove no lab imports phase modules or recreates layout
  facts.

## 6. Repository Gates

- [x] 6.1 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`; fix every regression and
  report any pre-existing failure precisely.
- [x] 6.2 Run `pnpm check` and `pnpm release:candidate` because compiler package contents and exports
  change, then validate the OpenSpec change strictly before handoff.
