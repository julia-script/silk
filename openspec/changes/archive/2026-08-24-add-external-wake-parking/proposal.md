## Why

Independent drive alone cannot wait for source-owned conditions without polling or a lost-wakeup
race. This slice adds the fixed-layout affine readiness capability and race-free register-before-
suspend protocol selected by SLP-0001.

Source: [SLP-0001, revision 31](../../../proposals/0001-independently-resumable-effect-executions/proposal.md),
SHA-256 `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`,
realization slice 3 of 5. Depends on `add-independent-execution-packaging`.

## What Changes

- Add opaque affine, initially non-thread-transferable `Intrinsic.Wake`, consuming `wake`, and
  race-free `park` with a once NonParking registration callback and compiler-retained guard `G`.
- Reuse one stable wake-control cell and one fixed detached endpoint per Execution, one generation
  at a time, without allocating during park, wake, or notification.
- Specify Registering, Latched, Dormant, Notifying, Eligible, Cancelled, and DestroyPending ordering,
  including wake-during-registration and reentrant destruction during endpoint notification.
- Make a late Wake after execution destruction a consuming no-op while retaining the indivisible
  cancelled Allocation until all Execution, Wake, and transient authorities are gone.
- Keep readiness payloads, queues, timers, Deferred state, unlink policy, and source allocation in
  ordinary Silk; wake never drives an execution inline.

## Capabilities

### New Capabilities

- `bootstrap-external-wake-parking`: define Wake ownership, race-free parking, generation reuse,
  notification ordering, cancellation, and exact reclamation.

### Modified Capabilities

- `bootstrap-semantic-facts`: assign sealed Wake values to the canonical local-affinity lattice and
  propagate that fact through source-owned state and suspended frames.
- `bootstrap-intrinsic-boundary`: admit only opaque Wake, consuming wake, and callback-shaped park
  while keeping scheduling actors and payload policy outside the compiler.
- `bootstrap-ownership`: retain and clean registration guard `G`, endpoint values, Wake authority,
  internal loans, and execution frames exactly once across every ordering and destruction path.

## Impact

This affects suspension reachability, intrinsic catalog metadata, semantic/HIR/MIR transitions,
runtime state verification, cleanup planning, and diagnostics. It remains same-thread and non-
atomic at the source type level; cross-thread transfer and the parallel memory model are explicitly
out of scope.
