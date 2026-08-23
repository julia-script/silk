# Implementation report: add-independent-execution-engine-parity

Status: **in progress**

## Scope

This layer consumes the target-neutral package, drive, park, Wake, affinity, and cleanup contracts
from the first three SLP-0001 changes. It owns complete transition authority and evaluator, native,
and direct-Wasm realization. Public API remains ordinary `silk.execution` source over the sealed
`Intrinsic` primitives; no Scheduler, Timer, reactor policy, transfer, or atomic primitive is added.

## Checkpoint 1: transition authority and evaluator continuation

- Added `ExecutionTransition`, the backend-independent composition of `ExecutionLifecycle` and
  `WakeCell`. It assigns stable package/root/generation identities, validates complete lifecycle and
  cleanup edges, exposes deterministic representation-free inspection, and supplies private state
  tags without fixing an ABI.
- Added negative transition tests for dormant drive, duplicate Wake, premature registration reuse,
  DestroyPending cleanup dominance, and late cancelled-Wake final release.
- Changed external-park reachability from intrinsic actor/name spelling to the closed
  `ExecutionPark` builtin-operation identity, resolving the Layer 3 Medium finding.
- Extended the evaluator's existing independent-call machine with an explicit per-execution retained
  stack/transfer record. `ExecutionPark` now registers one Wake, retains its guard, relinquishes the
  independently rooted machine, and resumes the same generator/frame stack only after eligibility.
- Implemented evaluator Wake latch, post-suspension notification, Eligible publication, guard drop
  immediately before resume, completion, dormant/late-Wake cleanup, and deferred Notifying cleanup.
  Canonical transition trace events use allocation-order package/root identities, never object
  identity.

Tasks 1.1 and 1.3 are complete. Task 1.2 remains open until callback/provenance/loan-negative MIR
fixtures are added. Evaluator tasks remain open until the full ordering/cleanup matrix and two-root
non-LIFO fixture pass.

## Verification history

- Initial focused command could not resolve unbuilt workspace package `@silk-effect/llvm/Bitcode`;
  building `@silk-effect/llvm` relocalized the same prerequisite to `@silk-effect/wasm/Binary`.
  Building both workspace packages supplied fresh-worktree outputs; this was environment setup, not
  a semantic repair.
- Focused transition/Wake/package/external-parking sequence — **PASS**, 4 files / 33 tests.
- Compiler source and test TypeScript checks — **PASS**.
- Targeted Biome check over all changed files — **PASS** after mechanical formatting.
- Generated toolchain identity was refreshed after compiler source changed; direct TypeScript checks
  then passed. The formal hard-gate loop has not started.

## Attempt budget

Hard-gate root-cause fixes used: **0/3**. Fresh-worktree build prerequisites and normal generated
identity refresh occurred before the formal gate sequence.
