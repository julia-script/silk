# bootstrap-external-wake-parking Specification

## Purpose

Defines affine Wake readiness, race-free external parking, notification ordering, cancellation, and
whole-package reclamation for independently resumable executions.

## Requirements

### Requirement: Park registers readiness before relinquishment

`Intrinsic.park<G,F>` SHALL be callable only while an explicit Execution is Running and SHALL invoke
one NonParking `F: once fn(Wake) -> G` with the generation's sole opaque affine Wake. The
registration callback SHALL either store or consume that Wake and return one ordinary guard `G`.
The runtime SHALL retain `G` and every live frame value before relinquishing the Execution through
the drive suspension callback. `park` SHALL return unit only after a later legal resume, and SHALL
drop `G` exactly once immediately before resumed source continues. Park, registration, and
relinquishment MUST NOT allocate or consult Scheduler policy.

#### Scenario: Register then become dormant

- **WHEN** registration stores the Wake and returns `G` without signaling
- **THEN** the runtime retains `G`, the complete suspension callback receives the Dormant Execution, and source after park does not continue

#### Scenario: Drop the guard before resumed source

- **WHEN** a later wake makes the execution Eligible and the owner legally drives it
- **THEN** the runtime drops `G` exactly once before `park` returns unit and source rechecks its durable condition

#### Scenario: Retain guard cleanup on dormant destroy

- **WHEN** the owner drops the Dormant Execution before readiness
- **THEN** cancellation occurs first and `G` is cleaned exactly once during dormant destruction

#### Scenario: Reject parking outside explicit ownership

- **WHEN** an unowned complete entry or a runtime-invoked NonParking callback reaches `Intrinsic.park`
- **THEN** static analysis rejects the missing delimiter or failed NonParking contract before execution

### Requirement: Wake-before-dormant ordering is race free

Each park generation SHALL share one stable wake-control cell between the Execution and its sole
Wake. Consuming Wake during registration SHALL latch readiness but MUST NOT invoke the fixed endpoint
until the complete drive suspension callback returns. The Execution SHALL relinquish exactly once
even when readiness is latched. After suspension ownership is established, a live latched or later
Wake SHALL begin exactly one notification. Affine ownership MUST prevent a second signal for the
generation.

#### Scenario: Signal inside registration

- **WHEN** registration consumes Wake before returning `G`
- **THEN** readiness is Latched, the suspension callback first receives and stores the Execution, and only after that callback returns may endpoint notification begin

#### Scenario: Destroy a latched execution in onSuspend

- **WHEN** Wake was consumed during registration and the suspension callback drops the returned Execution
- **THEN** destruction cancels the latched notification, endpoint invocation is suppressed, and continuation and endpoint values are cleaned exactly once

#### Scenario: Signal after dormancy

- **WHEN** a retained Wake is consumed after the Execution is Dormant
- **THEN** exactly one notification begins and no continuation code runs inline

#### Scenario: Reject a second readiness signal

- **WHEN** safe source attempts to signal the same generation twice
- **THEN** the second use is rejected as use-after-move because the first `wake` consumed the sole affine Wake

### Requirement: Notification is non-drivable and reentrant destruction is deferred

Consuming a live Wake SHALL transiently retain the control cell, finish cell mutation, mark the
Execution Notifying, and invoke its fixed `R(&O)` exactly once under an invocation retain. The
endpoint MAY publish ordinary-source readiness but MUST NOT park and `wake` MUST NOT drive the
Execution inline. An indirect drive while Notifying SHALL trap. If endpoint code reentrantly causes
Execution destruction, destruction SHALL record DestroyPending without dropping borrowed `O` or
`R` or releasing the package; after `R` returns, the runtime SHALL clean the execution and package
instead of making it Eligible. Otherwise callback return SHALL make the live Execution Eligible.

#### Scenario: Publish readiness then become eligible

- **WHEN** a live Wake invokes an endpoint that publishes one source identity and returns normally
- **THEN** the Execution remains Notifying during the callback and becomes Eligible only after return

#### Scenario: Trap reentrant drive

- **WHEN** endpoint code indirectly retrieves the Notifying Execution and attempts to drive it before returning
- **THEN** drive performs the defined fatal state trap without replacing the endpoint or running continuation code

#### Scenario: Defer reentrant destroy cleanup

- **WHEN** endpoint publication reentrantly destroys the Notifying Execution
- **THEN** the package records DestroyPending, keeps borrowed endpoint state alive through callback return, then cleans and releases instead of becoming Eligible

#### Scenario: Keep wake allocation free

- **WHEN** a live or cancelled Wake is consumed
- **THEN** notification or no-op completion uses only existing package and source state and introduces no allocator or failure channel

### Requirement: Cancellation makes late Wake safe and retains indivisible storage

Destroying an Execution with an outstanding Wake SHALL first mark the generation Cancelled, then
clean `O`, `R`, `G`, the body, and every live frame value exactly once. Any retained Wake SHALL keep
the complete indivisible combined Allocation as inert cancelled-cell storage and consuming or
dropping that Wake SHALL be a safe no-op that releases its authority. Reclamation SHALL occur only
after Execution, Wake, Registering/notification transient, and invocation authorities are all gone;
source guard `G` MUST NOT be trusted for memory safety. The cell MUST NOT begin a later generation
until the prior Wake plus every Registering, notification, and invocation authority is discharged.

#### Scenario: Consume Wake after execution drop

- **WHEN** an external source retains Wake after the corresponding Dormant Execution was destroyed
- **THEN** consuming Wake invokes no endpoint, touches no cleaned continuation value, and may release the inert combined Allocation when it owns the final authority

#### Scenario: Forget a cancelled Wake

- **WHEN** source neither consumes nor drops a Wake after Execution destruction
- **THEN** every package value is already cleaned but the complete combined Allocation remains retained as inert storage

#### Scenario: Drop an eligible execution

- **WHEN** Wake was consumed, notification completed, and the owner drops the Eligible Execution before drive
- **THEN** endpoint notification is not repeated, `G` and frames are cleaned once, and any stale source identity remains ordinary source policy

#### Scenario: Reuse the cell for a later generation

- **WHEN** an Eligible execution is driven, resumes past park, and later reaches another park
- **THEN** the same stable cell is reinitialized only after the prior Wake is consumed or dropped and every Registering, notification, and invocation retain is discharged, so no prior authority crosses the generation boundary

### Requirement: Wake remains local and payload free

`Intrinsic.Wake` SHALL be opaque, affine, fixed-layout, and initially non-thread-transferable. It
SHALL carry readiness authority only: it MUST NOT transport `A`, failure values, task identities,
Scheduler tokens, timer payloads, or callback representations visible to source. Source-owned state
SHALL carry payload and policy. Same-thread Deferred, timer reactor, or Coroutine port code MAY
store and consume Wake after releasing short shared-state access. Cross-thread movement MUST remain
rejected until a parallel-memory proposal defines transfer and atomic ordering.

#### Scenario: Store Wake in heterogeneous source state

- **WHEN** a Deferred or timer registration created before park receives the opaque Wake
- **THEN** it stores one uniform readiness value without naming the Execution endpoint representation

#### Scenario: Extract before signaling

- **WHEN** source shared state contains Wake and a producer becomes ready
- **THEN** source extracts Wake under short access, ends access, and then consumes it without invoking an external callback inside the access region

#### Scenario: Publish affinity without inventing a transfer consumer

- **WHEN** inspection observes Wake directly or through source-owned aggregate state in the local model
- **THEN** it reports canonical `LocalExecution` affinity and lowering contains no cross-thread transfer or atomic guarantee; a concrete transfer diagnostic remains deferred until a transfer consumer exists
