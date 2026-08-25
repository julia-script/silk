## Why

Silk already has the compiler-owned `Execution`, `Wake`, and `Shared` substrate needed for
independent suspension, but applications must currently rebuild task ownership, readiness, and
completion protocols by hand. A canonical single-threaded Fiber API and explicit user-selected
Scheduler turn that substrate into a usable structured-concurrency model without giving the
compiler scheduling policy.

## What Changes

- Add an ordinary source-defined `Scheduler` service used by Fiber operations and supplied by the
  application.
- Add `LocalScheduler`, a deterministic single-threaded FIFO provider whose explicit `execute`
  operation owns the root Execution and privately drives it to completion.
- Add affine `Fiber<A, E>` handles with child creation, consuming observation, structured
  cancellation, and cooperative yielding.
- Define allocation, task-identity exhaustion, cancellation, stalled-root, cleanup, and fatal-trap
  behavior explicitly.
- Add the minimal allocation-safe storage foundations required by the scheduler, including
  callback-scoped mutable HashMap entry access and an intrusive preallocated ready queue.
- Prove the design first with one end-to-end root/fork/join fixture, then expose the reusable
  standard-library actors and differential engine coverage.
- Keep `Execution` and `Wake` as the only compiler-owned scheduling substrate, extending Execution
  only with an exactly-once, target-neutral initial-readiness notification; no implicit scheduler,
  compiler-selected policy, or source-name recognition is introduced.

## Capabilities

### New Capabilities

- `bootstrap-single-threaded-fibers`: Defines explicit scheduler entry, Scheduler service
  provision, Fiber lifecycle and observation, FIFO readiness, structured cancellation, allocation
  failure, cleanup, and evaluator/native/Wasm parity.

### Modified Capabilities

None.

## Impact

- Adds canonical Silk modules for the Scheduler protocol, Fiber API, and LocalScheduler provider.
- Extends ordinary collection source where needed for allocation-safe in-place task-table access.
- Adds evaluator-first semantic fixtures and corpus coverage for target-specific execution parity.
- Updates the language reference and generated standard-library source manifest.
- Adds one sealed `Execution.notifyInitial` operation needed to make an already-published Execution
  ready without activating it; it does not add a scheduler-specific intrinsic, change `main`,
  select a scheduler in the compiler, or preserve a compatibility path for the test-local
  scheduler prototypes.
