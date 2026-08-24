## Context

The packaging slice provides one fixed endpoint and combined Allocation; the semantic slice provides
external-park reachability and NonParking. The remaining race is between registration,
relinquishment, wake, notification, and destruction. See `proposal.md` and the three delta specs.

## Goals / Non-Goals

**Goals:**

- Make register-before-suspend and wake-before-dormant linearizable in the target-neutral machine.
- Keep late Wake safe without trusting source unlink cleanup.
- Reuse one fixed endpoint and stable cell across sequential park generations.

**Non-Goals:**

- Define cross-thread Wake, atomics, Scheduler policy, timer APIs, payload transport, or fairness.
- Add a second package, explicit destroy/cancel intrinsic, or allocation during notification.

## Decisions

### Use one opaque affine Wake per generation

Park manufactures one purpose-bound Wake referring to stable package control state. The callback may
store it in pre-existing heterogeneous source state because its representation is fixed. `wake`
consumes it and never transports payload or directly resumes continuation code.

Alternatives rejected: storing an exact callback is impossible in pre-existing nominal state;
general callback boxing is broader; owner sweeps lose task-specific push readiness.

### Latch readiness until suspension ownership returns

The cell enters Registering before invoking source. Wake during registration records Latched. Park
retains `G` and frames, then the complete `onSuspend` callback receives Execution ownership. Only
after that callback returns can Latched readiness enter notification. If `onSuspend` destroys the
Execution, cancellation wins and notification is suppressed.

Alternatives rejected: invoking the endpoint during registration lets an owner select a still-
Running execution; registering after relinquishment loses notifications.

### End cell mutation before endpoint invocation

Live wake takes a transient retain, resolves the generation state, enters Notifying, and releases
the mutable cell operation before borrowing `O` for `R(&O)`. An invocation retain protects endpoint
storage. Callback return either moves a live execution to Eligible or completes DestroyPending
cleanup.

Alternatives rejected: holding mutable cell access across arbitrary source code introduces
reentrancy and aliasing hazards; making Eligible before callback return permits reentrant drive.

### Cancel values but retain the indivisible Allocation behind late Wake

Execution drop first marks Cancelled, then cleans every value. Each external Wake and transient
operation retains only reclamation authority for the complete Allocation. A cancelled wake is a
consuming no-op. `G` may unlink promptly but is excluded from the safety count.

Alternatives rejected: trusting `G` makes memory safety depend on arbitrary source cleanup;
separately allocating a wake header reverses the one-package decision; raw pointers make late wake
unsafe.

### Reuse by generation only after Wake authority ends

Once notification consumes the generation Wake and a legal drive resumes past park, a later park may
reset the same cell. Verification rejects any transition that begins a new generation while the old
Wake or transient operation remains live.

Alternatives rejected: allocating a cell per park hides recurring cost; reusing early aliases two
readiness events and risks ABA-like behavior.

## Risks / Trade-offs

- **Forgotten cancelled Wake retains the whole package** → document and test the intentional cost;
  ensure all values are already cleaned and only inert bytes remain.
- **Callback reentrancy creates a use-after-free** → model Notifying/DestroyPending explicitly and
  verify the invocation retain dominates endpoint cleanup and package release.
- **A source wrapper signals while holding Shared access** → pressure tasks require extract-then-
  signal and inspect that no external callback runs under access.
- **Local implementation accidentally promises atomics** → keep Wake local-affinity in semantic
  facts and test same-thread ordering only; parallel transfer is a later SLP.
