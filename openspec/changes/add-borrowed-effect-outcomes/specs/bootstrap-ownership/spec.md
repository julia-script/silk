## MODIFIED Requirements

### Requirement: Failure payloads obey ordinary detached ownership

Every value admitted as an Effect failure type SHALL use its ordinary Copy, move, Drop, union-tag,
and cleanup behavior. `fail`, propagation, selective recovery, whole-channel recovery, and re-fail
SHALL transfer one ordinary payload without a row wrapper. A failure payload SHALL preserve its complete lifetime-bearing type and ordinary ownership. Borrowed payloads SHALL be admitted only when their referents survive propagation, handlers, retry, unwind and cleanup. References to destroyed locals or consumed environment-owned storage SHALL be rejected.

#### Scenario: Propagate an affine ordinary failure once

- **WHEN** an affine failure payload crosses nested Effect calls before recovery
- **THEN** ownership transfers one payload and schedules exactly one cleanup if it remains unconsumed

#### Scenario: Reject an escaping borrowed failure

- **WHEN** `fail` publishes a reference to an owner destroyed before the handler or later payload use
- **THEN** the ordinary borrow-escape diagnostic rejects it before executable lowering

#### Scenario: Recover a structural failure union

- **WHEN** a handler receives one selected alternative from an ordinary failure union
- **THEN** its pattern narrowing, moves, and cleanup use the same ownership rules as that union in any other value position

#### Scenario: Forward external borrowed failures

- **WHEN** a nested generic failure retains an external source valid through propagation and recovery
- **THEN** the payload is admitted and its source loan remains live after the temporary computation ends

### Requirement: Suspension transfers one ownership obligation per live value

After concrete specialization and suspendability-aware MIR normalization, ownership SHALL derive
the exact MIR-local live set needed after a deferred child transfers and later completes. The set
SHALL include compiler-generated temporaries as well as locals corresponding to source bindings.
Copy values MAY be copied; affine values SHALL occupy one field in exactly one state of the
invocation's reusable coroutine frame; and shared or exclusive borrows and provider references
SHALL retain their exact root, access, and loan dependencies until resumption or exit. A referent
that remains borrowed across suspension SHALL retain a stable logical location for the borrow's
lifetime regardless of private frame placement or relocation. A value MUST NOT remain independently
owned by both the running state and suspended state.

#### Scenario: Hold one owner per recursive level

- **WHEN** every level of a suspended recursive Effect creates one affine owner used after its child completes
- **THEN** ownership places each owner in exactly one active invocation frame state and rejects any duplicate use from another state

#### Scenario: Retain an exclusive provider dependency

- **WHEN** source code intentionally holds an ordinary exclusive provider reference across its deferred child
- **THEN** ownership keeps that provider immovable and exclusively borrowed until the parent resumes and ends the loan

#### Scenario: Preserve a borrow across private frame growth

- **WHEN** a valid source borrow remains live while the private execution stack grows, segments, or relocates implementation storage
- **THEN** the borrow continues to identify the same referent with unchanged access and lifetime

#### Scenario: Reject an unverified partial suspension

- **WHEN** a partial owner would cross suspension without a verified frame initialization, stable-placement or remainder-cleanup plan
- **THEN** analysis rejects the suspension before executable lowering

#### Scenario: Preserve partial suspension

- **WHEN** a partial owner would remain live across a potentially suspending child call
- **THEN** the frame preserves definite, missing and conditional component state with live flag storage; resume cannot read missing components and cancellation cleans only the initialized remainder in established order

#### Scenario: Cancel before restoration

- **WHEN** a field moves to a destination before suspension and cancellation occurs before restoration
- **THEN** the destination retains the transferred child and the frame cleans only its live remainder, retaining every referent through cleanup

#### Scenario: Complete installation atomically

- **WHEN** a moved component is reinitialized or replaced
- **THEN** cleanup, installation and ownership-state update commit without suspension between them
