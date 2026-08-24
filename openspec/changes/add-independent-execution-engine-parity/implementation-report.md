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

## Checkpoint 2: canonical external relay and direct-Wasm resume

- Final MIR now retains an `ExecutionPark` relay as an explicit external transfer origin. The prior
  origin-only finalization discarded the otherwise valid park frame and caused direct Wasm to omit
  its suspension runtime entirely. Orphan verification now treats only the closed `ExecutionPark`
  operation as that origin; ordinary suspendable calls still require a reachable transfer source.
- Coroutine verification accepts the park guard's purpose-bound success release and excludes that
  consumed guard from restored payload slots. Zero-lane guards correctly require no frame release.
- Direct Wasm persists the independent continuation head in its exact execution package, restores
  it only for a later Eligible drive, and relinquishes without running the source continuation.
  Registration receives the sealed Wake pointer, latch occurs during registration, and notification
  runs only after `onSuspend` returns.
- Wasm readiness delivery reconstructs the exact represented callback and hidden environment from
  package-layout facts and invokes it through the stored endpoint borrow before publishing Eligible.
  It does not recognize a source actor or declaration spelling.
- Coroutine frames now use deterministic fixed-size slots with a non-LIFO-safe free list. A later
  drive of one root cannot rewind or overwrite another parked root's continuation frame.
- The evaluator/Wasm latched-resume acceptance program agrees on result `42`, one stable logical
  root, and `Initialize, Drive, Register, Latch, Park, Notify, Eligible, Resume, Drive, Complete`.

Native realization, the complete destruction/cancellation matrix, two-root alternating acceptance,
and reactor availability remain open; task 3.2 therefore remains unchecked despite the first
working direct-Wasm external-resume path.

## Verification history

- Initial focused command could not resolve unbuilt workspace package `@silk-effect/llvm/Bitcode`;
  building `@silk-effect/llvm` relocalized the same prerequisite to `@silk-effect/wasm/Binary`.
  Building both workspace packages supplied fresh-worktree outputs; this was environment setup, not
  a semantic repair.
- Focused transition/Wake/package/external-parking sequence — **PASS**, 4 files / 33 tests.
- Compiler source and test TypeScript checks — **PASS**.
- Targeted Biome check over all changed files — **PASS** after mechanical formatting.
- Post-checkpoint focused regression — **PASS**, 4 files / 27 tests, including evaluator-to-Wasm
  latched external resume and existing synchronous Execution package completion/failure behavior.
- Generated toolchain identity was refreshed after compiler source changed; direct TypeScript checks
  then passed. The formal hard-gate loop has not started.

## Attempt budget

Hard-gate root-cause fixes used: **0/3**. Fresh-worktree build prerequisites and normal generated
identity refresh occurred before the formal gate sequence.
