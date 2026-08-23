## Why

The selected execution and wake contracts are useful only if evaluation, native, and direct Wasm
agree on non-LIFO resumption, typed outcomes, logical stack roots, traps, and exact cleanup. This
slice realizes the target-neutral contract across all supported engines.

Source: [SLP-0001, revision 31](../../../proposals/0001-independently-resumable-effect-executions/proposal.md),
SHA-256 `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`,
realization slice 4 of 5. Depends on `add-external-wake-parking`.

## What Changes

- Add verified MIR operations and deterministic inspection data for package construction, drive,
  nested transfer, park, wake, notification, resume, completion, cancellation, and destroy.
- Give each explicit Execution an evaluator logical-stack context rooted at first drive and restored
  across later owner-selected drives.
- Lower execution-owned continuation storage and the complete wake/package state machine in native
  and direct Wasm while retaining the existing cheaper nested/LIFO path.
- Differentially verify alternating explicit executions in non-LIFO order, every wake-ordering and
  cleanup-matrix row, typed failure, illegal-state traps, fatal stack exhaustion, and local reactor
  delivery.
- Preserve target-private layout, pooling, growth increments, and ABI details without changing the
  caller-funded procurement or no-unwind contracts.

## Capabilities

### New Capabilities

- `bootstrap-independent-execution-engine-parity`: define verified execution/wake MIR and exact
  evaluator, native, and Wasm behavioral parity.

### Modified Capabilities

- `bootstrap-evaluation`: model execution-local stacks, owner-selected drives, wake ordering,
  notification reentrancy, cancellation, and cleanup as the deterministic oracle.
- `bootstrap-backend`: realize the verified state machine and execution-owned storage in native and
  Wasm with typed-outcome, trap, cleanup, and resume parity.

## Impact

This affects MIR validation and inspection, evaluator state, native runtime slices, Wasm runtime
support, continuation lowering, engine acceptance corpus coverage, and deterministic artifacts. It
adds no public Scheduler, Fiber, Deferred, Timer, Coroutine, root adapter, or stable runtime ABI.
