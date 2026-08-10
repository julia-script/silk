## 1. Establish the Measurement Baseline

- [x] 1.1 Confirm that the representative runner core and source-defined combinators from `make-effects-library-definable` are implemented, and record the measured commit, compiler configuration, LLVM version, and Wasm tool versions.
- [x] 1.2 Identify or add deterministic compiler entry points that emit HIR, MIR, LLVM IR before and after optimization, native assembly, Wasm bytes, and WAT without changing compilation semantics.
- [x] 1.3 Define a normalized artifact format that removes unstable symbols and metadata while preserving calls, allocations, tags, branches, ownership operations, and traps.

## 2. Build the Paired Silk Corpus

- [x] 2.1 Add a pure imperative-versus-piped pair and assert that `|>` is absent from MIR in both debug and release configurations.
- [x] 2.2 Add matched imperative/`Result` and Effect pairs for `map`, success and failure `mapBoth`, and `flatMap` sequencing.
- [x] 2.3 Add matched pairs for requirement provision and generic requirement adaptation.
- [x] 2.4 Exercise direct pipelines, named helper boundaries, and stored Effect values so the corpus distinguishes trivial inlining from realistic composition.
- [x] 2.5 Exercise both `Copy` and affine values, with observable cleanup checks that prove exactly-once ownership and `Drop` behavior.
- [x] 2.6 Add a synchronous-runtime structural guard proving current artifacts contain no scheduler, fiber, continuation, or suspension-dispatch representation, and document that a genuine may-suspend control is deferred until suspension exists.
- [x] 2.7 Give every pair independent behavioral assertions for value, failure, evaluation order, requirements, traps, and cleanup before comparing generated artifacts.

## 3. Capture Native and Wasm Evidence

- [x] 3.1 Add a fresh-process harness that compiles every corpus pair and records normalized HIR and MIR artifacts reproducibly.
- [x] 3.2 Capture unoptimized LLVM IR, optimized LLVM IR, native assembly, and code-size data for every LLVM-backed pair.
- [x] 3.3 Capture direct-Wasm WAT and binary-size data for every pair independently of the LLVM pipeline.
- [x] 3.4 Add structural checks for unexpected suspension machinery, heap allocations, statically avoidable indirect runner calls, intermediate wrappers, and closure environments.
- [x] 3.5 Verify that current artifacts remain scheduler- and continuation-free while non-suspending cases preserve evaluation order, failure short-circuiting, traps, and ownership semantics.
- [x] 3.6 If a fixture is large and stable enough for timing, collect repeatable benchmark distributions as supporting evidence without making timing a completion criterion.

## 4. Analyze and Record the Decision

- [x] 4.1 Classify each retained cost by the earliest layer where it appears and the layer, if any, where it disappears.
- [x] 4.2 Compare the final native and direct-Wasm structure of every Effect case with its imperative baseline against the criteria in `design.md`.
- [x] 4.3 Write `wayfinder/bootstrap-language/research/synchronous-effect-cost-spike.md` with reproduction commands, toolchain versions, artifact references, per-case findings, semantic-safety checks, and limitations.
- [x] 4.4 Select and justify exactly one recommendation: rely on backend optimization, propose shared MIR normalization, or propose a non-suspending runner specialization.
- [x] 4.5 If compiler work is justified, create a separate OpenSpec proposal with the proven applicability guards; otherwise add stable regression coverage for the zero-cost cases and explicitly close without an optimizer.
- [x] 4.6 Reconcile the Effect design direction and bootstrap roadmap with the evidence without changing normative language semantics inside this spike.

## 5. Verification

- [x] 5.1 Run the corpus behavior tests and artifact structural checks for both native LLVM and direct-Wasm backends.
- [x] 5.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`.
- [x] 5.3 Run `pnpm check` and strictly validate the completed OpenSpec change before handoff.
