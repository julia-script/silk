## Purpose

Execution storage isolates source policy from compiler-owned suspension semantics.

## ADDED Requirements

### Requirement: Explicit selected storage

Artifacts SHALL declare their required storage component, allocator, capacity configuration, state initialization and lifetime. Selection SHALL use generic roots and typed contracts without privileged library names, implicit environment reads or process-global accounting. Synchronous artifacts without storage demand SHALL acquire no storage component.

#### Scenario: Independent library instances

- **WHEN** two library instances execute recursively or reentrantly
- **THEN** their capacity/accounting ownership remains explicit and independent; unsupported concurrent sharing is diagnosed

### Requirement: Storage outcomes and release

The implementation SHALL preserve frame layout/reuse and exactly-once cleanup after normal return, typed failure and cancellation. Recoverable acquisition failure SHALL release partial acquisitions. Later private-stack exhaustion SHALL remain fatal without a new public Effect requirement/error or guaranteed trap unwinding.

#### Scenario: Suspension cancellation

- **WHEN** a suspended execution is canceled
- **THEN** every exited frame owner is released exactly once through its selected component

### Requirement: Retained Wasm consumer

The changed generic storage ABI SHALL migrate LLVM-to-Wasm linear-memory providers, exhaustion and cleanup in the same replacement. No obsolete ABI fallback or disabled suspension SHALL remain.

#### Scenario: Wasm frame reuse

- **WHEN** an existing Wasm coroutine resumes and completes
- **THEN** it reuses and releases storage under the migrated contract without hosted dependencies
