## Why

SLP-0002 was split from SLP-0001 to remove the shared-state wall for source-defined scheduling.
The handoff is incomplete until a connected ordinary-Silk witness proves that ready-inbox callbacks
and Deferred state need no compiler-known queue, Deferred, or Scheduler.

Source: [SLP-0002, revision 6](../../../proposals/0002-allocation-backed-local-shared-ownership/proposal.md),
SHA-256 `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`,
realization slice 6 of 6. Depends on `add-local-shared-standard-library` and
`add-local-shared-engine-parity`; it supplies evidence to the separate SLP-0001 handoff.

## What Changes

- Add a readable ordinary-Silk pressure program with a fixed-capacity ready inbox shared by multiple
  dormant callbacks and a Deferred-style shared value/waiter state.
- Extract readiness callbacks during `Shared.withMut` and invoke them only after access is restored,
  proving the selected all-exclusive conflict policy is sufficient.
- Verify sequential readiness, one-time publication, unpublished affine cleanup, dormant-execution
  cleanup, and allocation/release parity across evaluation, native, and Wasm.
- Audit compiler artifacts to prove no actor-specific recognition of ReadyInbox, Deferred, Scheduler, or Shared.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-language-pressure-programs`: add the SLP-0001 local scheduling-state sufficiency witness and its findings gate.

## Impact

This affects pressure-program source, support corpus entries, findings documentation, and
cross-engine acceptance tests. It does not implement execution transfer, parking, wake ordering, or
the public Scheduler/Deferred APIs owned by SLP-0001.
