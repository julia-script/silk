## Purpose

Defines verified MIR and exact evaluator, native, and direct-Wasm parity for explicit independent
executions, external parking, wake ordering, cleanup, and traps.

## ADDED Requirements

### Requirement: MIR verifies every independent-execution transition

MIR SHALL represent package initialization, drive entry, nested transfer, external park,
registration completion, suspension ownership transfer, wake, notification, eligibility, resume,
completion, cancellation, DestroyPending, and final release as target-neutral verified operations or
edges. Validation SHALL reject invalid state transitions, duplicate authorities, mismatched package
provenance, callback contract violations, escaping completion loans, premature generation reuse,
and cleanup that precedes live borrows. Inspection SHALL encode logical states, operation identities,
and cleanup edges deterministically without backend addresses, field offsets, Scheduler identities,
or a stable runtime ABI.

#### Scenario: Verify a complete park and resume path

- **WHEN** MIR initializes, drives, parks, transfers suspension ownership, wakes, notifies, becomes Eligible, resumes, and completes one Execution
- **THEN** validation accepts one ordered authority path and inspection records every logical transition and cleanup edge deterministically

#### Scenario: Reject drive from Dormant

- **WHEN** MIR attempts a drive transition from Dormant without a completed notification
- **THEN** validation rejects the transition before any engine lowering

#### Scenario: Reject early endpoint cleanup

- **WHEN** a DestroyPending path would clean endpoint state before the notification invocation retain ends
- **THEN** validation rejects the cleanup order and retains the causal state edge

#### Scenario: Repeat MIR inspection

- **WHEN** the same complete source and target plan are analyzed repeatedly
- **THEN** independent-execution MIR and cleanup inspection are byte-identical

### Requirement: Supported engines agree on owner-selected execution behavior

Evaluation, native, and direct Wasm SHALL agree on body non-execution before first drive, owner-
selected non-LIFO drive order, nested transfer behavior, park relinquishment, one-shot readiness,
Notifying/Eligible ordering, typed completion data, cancellation, never-driven cleanup, dormant
cleanup, DestroyPending cleanup, and defined fatal traps. Each Execution SHALL preserve its own
logical stack root and CallDepth across drives. Native and Wasm MAY choose different physical
layouts, segment growth, and pooling, but MUST preserve the same observable results and ordered
ownership events.

#### Scenario: Alternate two executions in non-LIFO order

- **WHEN** a source owner drives two Initial executions, parks each, wakes the second before the first, and drives them in readiness order
- **THEN** all engines produce the same outcomes, continuation order, and per-execution logical depth without treating owner frames as ancestors

#### Scenario: Cover wake during registration

- **WHEN** registration consumes Wake before returning and onSuspend retains the execution
- **THEN** all engines relinquish exactly once, notify only after onSuspend returns, and permit only a later owner drive

#### Scenario: Cover late cancelled Wake

- **WHEN** an execution is destroyed before readiness and the retained Wake is consumed later
- **THEN** all engines invoke no endpoint, perform no continuation access, and release inert storage at the same logical authority point

#### Scenario: Cover reentrant destruction

- **WHEN** endpoint publication destroys the Notifying execution
- **THEN** all engines defer endpoint cleanup until callback return and never make the execution Eligible

#### Scenario: Preserve typed outcomes

- **WHEN** a reified body completes with Success or Failure data after any number of parks
- **THEN** all engines deliver the same `Result` value and cleanup order without a Pending or stack-growth failure member

#### Scenario: Preserve illegal-state traps

- **WHEN** execution drives Dormant or Notifying state or exhausts its post-construction logical stack
- **THEN** every engine follows the selected fatal no-unwind path and runs no drive outcome callback

### Requirement: Same-thread reactor delivery remains target neutral

Native and Wasm SHALL provide a same-thread polling/reactor path capable of retaining a source
registration, extracting Wake after short source access, and consuming it without making Wake
transferable or adding mandatory atomics. Engine support MUST NOT require a compiler-known Timer,
event loop, Scheduler, or host callback policy. A target that cannot supply the selected local
reactor path SHALL reject that target-specific pressure program through explicit availability
evidence rather than silently block or transfer Wake across threads.

#### Scenario: Deliver one timer Wake locally

- **WHEN** an explicit source driver polls a same-thread timer reactor and consumes the registered Wake
- **THEN** native and Wasm publish readiness and later resume the same Execution without thread transfer or inline drive

#### Scenario: Keep timer policy ordinary

- **WHEN** the timer provider and reactor actor are renamed or replaced while using the same Wake contract
- **THEN** engine behavior and intrinsic inventory remain unchanged

#### Scenario: Reject unavailable reactor delivery

- **WHEN** a target cannot implement the selected same-thread reactor pressure path
- **THEN** target availability rejects the program explicitly and does not substitute blocking, worker-thread Wake, or hidden scheduler behavior
