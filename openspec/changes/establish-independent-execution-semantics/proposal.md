## Why

Silk's existing suspension machinery can resume only a known nested child; ordinary source cannot
own, defer, select, or destroy an unfinished Effect execution. This first SLP-0001 slice establishes
the target-neutral semantic facts and affine lifecycle that every storage, wake, and engine slice
depends on.

Source: [SLP-0001, revision 31](../../../proposals/0001-independently-resumable-effect-executions/proposal.md),
SHA-256 `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`,
realization slice 1 of 5.

## What Changes

- Add opaque affine, initially non-thread-transferable `Intrinsic.Execution<A>` and the
  compiler-owned `Intrinsic.Detached` and `Intrinsic.NonParking` static properties.
- Admit one exact Effect or callable representation bound conjoined with sealed static properties
  without turning the executable bound into nominal conformance or a general intersection type.
- Distinguish direct, nested-transfer, and external-park reachability and make explicit Execution
  construction a propagation delimiter.
- Define the owner-neutral Initial, Running, Dormant, Notifying, Eligible, Completed, and Destroyed
  lifecycle, including legal drive states and exact affine ownership outcomes.
- Keep Scheduler, Fiber, Deferred, timer, Coroutine, and complete-program entry adaptation outside
  this semantic substrate.

## Capabilities

### New Capabilities

- `bootstrap-independent-execution-semantics`: define suspension-mode summaries, the explicit
  execution delimiter, static admission properties, and the owner-neutral affine lifecycle.

### Modified Capabilities

- `bootstrap-semantic-facts`: publish deterministic Detached, NonParking, suspension-mode, and
  execution facts without actor-name privilege.
- `bootstrap-representation-parameters`: admit sealed-property conjuncts on one exact executable
  representation while preserving exact identity through specialization.
- `bootstrap-ownership`: track one affine execution obligation, legal transfers, internal loans,
  completion-loan escape rejection, and exact lifecycle cleanup obligations.

## Impact

This affects semantic analysis, specialization, reachability summaries, ownership, diagnostics,
inspection encodings, and the sealed intrinsic type catalog. It introduces no storage layout,
construction operation, wake capability, Scheduler policy, or backend lowering; those belong to
dependent slices.
