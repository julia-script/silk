## Context

See `proposal.md` for motivation. The current project path is deliberately narrow: `Project.load` decodes `[package].name`, `root`, and optional `source-root`; `ProjectOptions` carries one optional target; `BuildPlan.make` derives one native destination under `.silk/build`; and `Workflow.build` executes one plan. `Driver.compile` accepts an optional backend but rejects non-native targets before MIR, then unconditionally sends the emitted artifact through object emission, native shim compilation, and linking. `BackendRegistry.forTarget` chooses the first backend that advertises a target, so target currently determines backend in practice.

The compiler already has two nominal backend values. LLVM emits deterministic LLVM bitcode and the direct WebAssembly backend emits validated final module bytes, but both are represented through one structurally misleading artifact shape. Target layout already supports `wasm32-unknown-unknown`; the missing seams are explicit backend identity, honest artifact kinds, compatible finalization, and project-level batch planning.

The CLI and compiler are Effect programs. Filesystem, child-process, and cleanup work must remain behind typed Effect boundaries, use scoped brackets for temporary or rollback state, and preserve the existing `0`/`1`/`2` command exit classes.

## Goals / Non-Goals

**Goals:**

- Keep a sparse project manifest while making every default materializable as immutable project data.
- Represent backend, target selector, canonical target, profile, destination, and artifact kind as separate concepts.
- Validate a complete target batch before executing any compilation.
- Give LLVM-native, LLVM-Wasm, and direct-Wasm artifacts honest, deterministic finalization paths.
- Make initialization safe in a non-empty directory and reversible on failure.

**Non-Goals:**

- Library projects or `init --lib`.
- Dependency resolution, package publication, workspaces, or configurable profile definitions.
- Parallel target compilation or cross-target frontend caching.
- Running Wasm artifacts from `silk run`.
- Selecting multiple backends in one build invocation; a build has one backend and one or more targets.

## Decisions

### 1. Decode one materialized project configuration

Extend the `Project` actor so `load` returns package metadata and materialized build defaults together with the entry. Keep TOML decoding and semantic validation at this boundary: required semantic-version metadata, stable backend id, non-empty target selectors, and manifest-relative output directory. The generated manifest writes only `[package]`; the loaded project contains the implicit `llvm`, `[host]`, and `build` defaults.

Command-line resolution happens after loading. An explicit backend replaces the manifest backend, and any repeated `--target` values replace the complete manifest target list. This gives one precedence rule rather than field-by-field append behavior.

Alternative considered: emit `[build]` defaults during initialization. Rejected because it makes conventional defaults noisy and weakens the Rust-like sparse project file the command is meant to create.

### 2. Resolve selectors before constructing an immutable build batch

Treat `host` as a selector at the manifest/CLI boundary, not as a canonical `Target.Id`. Resolve every selector, deduplicate by canonical target id while preserving first occurrence, resolve the selected backend by stable id, and validate every backend-target pair before returning a `BuildBatch`. A `BuildBatch` owns an ordered non-empty collection of ordinary single-target `BuildPlan` values; each plan owns its full backend-qualified destination.

`BuildPlan` remains the unit passed to compilation. `BuildBatch` is the project orchestration concept and does not leak arrays into the compiler driver.

Alternative considered: make `BuildPlan` itself contain arrays. Rejected because it would mix batch policy with one compiler invocation and weaken per-artifact atomicity.

### 3. Give backends stable ids and honest artifact variants

Add stable ids `llvm` and `wasm` to the backend contract and change registry lookup from `forTarget(target)` to explicit id resolution followed by compatibility validation. Target never chooses a backend. Extend LLVM's declared targets with `wasm32-unknown-unknown`.

Replace the current universal backend artifact record with a closed discriminated family containing shared provenance plus kind-specific payload:

- LLVM bitcode artifact: bitcode and inspection IR, requiring target-specific external finalization.
- WebAssembly module artifact: validated `.wasm` bytes and WAT inspection text, already final.

This lets downstream code branch on artifact kind rather than display name or backend identity. It also prevents direct Wasm bytes from being passed accidentally to Clang as LLVM bitcode.

Alternative considered: keep a single `bitcode` byte field and branch on backend id. Rejected because the bytes have different formats and backend identity is not the same as artifact finalization state.

### 4. Separate emission from durable finalization

The driver owns phase orchestration and delegates external Clang work to one owning boundary. Deepen and rename the current native-only toolchain actor as needed so the single Clang boundary plans both:

- native LLVM: bitcode → object → native shim → linked executable;
- LLVM Wasm: bitcode → standalone `.wasm` with no native shim or host libraries and exported `silk_main`.

Direct WebAssembly follows a filesystem-backed atomic commit path and never requires the Clang service. Both finalizers write to a sibling temporary destination and rename only after validation succeeds, preserving the old destination on failure.

The successful driver outcome records backend id, canonical target, final artifact kind, durable path, symbols, diagnostics, and actual phase report. Failure variants keep typed backend, target, storage, and external-command provenance.

Alternative considered: make each backend write its durable output. Rejected because backends are deterministic MIR-to-artifact transforms; filesystem and external-tool resource ownership belong at a later boundary.

### 5. Execute build and check batches sequentially

`Workflow.build` preflights one `BuildBatch`, then attempts every plan in deterministic order with sequential Effect composition. It retains successful sibling artifacts, collects one outcome per target, renders a stable summary after all attempts, and folds exit status with `2 > 1 > 0`. There is no batch-wide rollback because independently addressable outputs are useful after partial success and each plan is already atomic.

`Workflow.check` runs target-aware analysis once per canonical target, labels diagnostics by target, and uses the same ordered outcome fold without backend emission. `Workflow.run` bypasses the manifest target array, resolves the host target, requires the selected backend to support a native executable, builds that one plan, and preserves the program exit status.

Alternative considered: stop after the first failed target. Rejected because it hides independent target results and makes CI require repeated invocations to discover the complete batch state.

### 6. Model initialization as a rollback-capable filesystem operation

Add a `ProjectInitializer` actor with immutable options and a typed error family. It resolves the selected directory and package name, plans every mutation, reads and snapshots an existing `.gitignore`, and preflights collisions before acquiring rollback state. The scoped operation records only paths it creates plus the exact prior `.gitignore` bytes. Release restores that snapshot and removes only newly created paths on failure, defect, or interruption; success commits the initialization and suppresses rollback.

The initializer never silently normalizes package names and exposes no force option. It appends `/build/` only when the exact line is absent, preserving unrelated bytes and choosing the necessary line break without reformatting the file.

Alternative considered: reject every non-empty directory. Rejected because `init` must support adding Silk to an existing repository without touching unrelated files.

### 7. Keep actor seams explicit

New concepts receive actor modules rather than helper collections: project initialization, target selectors, backend identity/registry, build batches, and final artifact data each have one owning module. Public actors are explicitly re-exported from package barrels and public package subpaths where appropriate. Named public Effect operations use `Effect.fn`; synchronous immutable validation and transformations return typed `Result` values.

## Risks / Trade-offs

- [LLVM installations may lack the Wasm linker or target support] → Validate the pinned Clang invocation during finalization, retain complete command provenance, cover command planning with a replaceable child-process service, and gate the real integration path in the supported release environment.
- [Required version metadata breaks existing manifests] → Update every repository fixture and document the one-line `version = "0.1.0"` migration; no compatibility decoder is retained during alpha.
- [Visible output paths break scripts using `.silk/build`] → Update README, release-candidate consumers, tests, and editor tasks together; do not create a compatibility symlink.
- [Checking several targets repeats target-independent frontend work] → Accept sequential repetition initially for simple, honest semantics; a future shared frontend cache can sit beneath `BuildBatch` without changing the command contract.
- [Rollback touches an existing `.gitignore`] → Snapshot exact bytes before mutation and restore them through scoped cleanup; never reconstruct prior content semantically.
- [Two Wasm paths can drift semantically] → Run shared executable fixtures through evaluator, direct Wasm, and LLVM Wasm, comparing `silk_main` results and trapping behavior while keeping byte determinism gates backend-specific.

## Migration Plan

1. Introduce manifest/version/build data and update repository manifests and fixtures in one breaking migration.
2. Add backend ids, compatibility resolution, discriminated artifacts, and target-independent selection while preserving native LLVM behavior.
3. Add LLVM Wasm finalization and direct-Wasm durable commit, then verify shared Wasm execution parity.
4. Introduce `BuildBatch`, repeatable targets, backend-qualified destinations, sequential summaries, and host-only run behavior.
5. Add rollback-safe initialization and register the command after its filesystem tests pass.
6. Update README, help snapshots, LSP fixtures, release-candidate validation, editor integrations, and package exports.

Rollback is a source revert because the project is unreleased. Artifacts under the new visible `build/` tree can be deleted safely; existing source and manifests are not automatically downgraded.
