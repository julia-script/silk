# bootstrap-compiler-driver Specification

## Purpose

The end-to-end LLVM orchestration path from a compilation request to a durable native or WebAssembly
artifact.

## Requirements

### Requirement: Driver outcomes identify LLVM artifacts

Every successful outcome SHALL retain backend identifier `llvm`, the target, artifact kind, durable
paths, symbols, diagnostics, and phase report. Executables and WebAssembly modules SHALL retain their
entry termination contract; native libraries SHALL retain their explicit C surface and companion
header and ABI manifest.

#### Scenario: Report an LLVM WebAssembly build

- **WHEN** LLVM produces a durable WebAssembly module
- **THEN** the outcome identifies the WebAssembly target, module path, entry contract, and executed finalization phases

### Requirement: Native corpus outcomes are independently pinned

The shared native acceptance corpus SHALL compare process results directly with committed case
expectations. Every case SHALL be assigned to exactly one CI shard, and the union of shards SHALL be
the complete corpus.

#### Scenario: Run a completing case

- **WHEN** a corpus case completes normally
- **THEN** its process status equals the case's pinned status

#### Scenario: Run a trapping case

- **WHEN** a corpus case is pinned as a trap
- **THEN** it must compile successfully and terminate abnormally at runtime

### Requirement: Driver phases are observable and deterministic

The driver SHALL report canonical frontend, realization, LLVM emission, and artifact-finalization
phases. Identical compiler, source, target, profile, and toolchain inputs SHALL preserve committed
structural and artifact determinism gates.

The existing Milestone A operation boundaries SHALL be named `Source.load`, `Hir.lower`,
`Preparation.prepare`, `Semantic.checkBody`, and `Evaluation.evaluate`. Corresponding trace and
phase-report identities SHALL name the work being performed rather than a generic measurement
wrapper. Fresh and reused body checks SHALL both pass through `Semantic.checkBody`; application
evaluation traces SHALL distinguish execution from cache reuse while preserving recorded-cost
budget admission. Module-level coordinators SHALL remain distinguishable from declaration checks.

#### Scenario: Observe source through compile-time evaluation

- **WHEN** a traced compilation loads and lowers source, prepares it, checks declarations and evaluates a selected static application
- **THEN** its spans expose the five Milestone A operation identities, measurement helpers add no wrapper spans, and reports retain their existing counters

#### Scenario: Reject invalid source

- **WHEN** frontend analysis produces an error diagnostic
- **THEN** no LLVM emission or artifact-finalization phase runs
