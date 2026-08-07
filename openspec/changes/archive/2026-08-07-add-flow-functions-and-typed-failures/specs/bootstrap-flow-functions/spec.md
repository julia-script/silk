## Purpose

Define Silk's lazy statically shaped flow values and its exact owned typed-failure channel, including
one-layer execution, propagation, recovery, and separation from unrecoverable traps.

## ADDED Requirements

### Requirement: Flow calls are lazy and run evaluates one layer

Calling a `flow fn` SHALL construct a flow without entering its body. `run` SHALL execute exactly one
flow layer and produce that layer's success value or propagate its owned typed failure. The first
executable slice SHALL keep these flow values statically shaped and MUST NOT fabricate a general
runtime closure object.

#### Scenario: Delay a body until run

- **WHEN** a flow call is bound before a later `run`
- **THEN** evaluation records capture at construction and enters the body only at `run`

### Requirement: Failure rows are normalized nominal contracts

The `!` row of a `flow fn` SHALL be a deterministic normalized set of owned canonical nominal
types. A body MUST NOT originate or propagate a failure absent from its declared row. Ordinary `fn`
functions and executable entries SHALL have empty failure rows.

#### Scenario: Normalize an equivalent row

- **WHEN** equivalent failure members are written in a different order or repeated
- **THEN** semantic facts publish the same canonical row and discriminants

#### Scenario: Reject an undeclared propagated failure

- **WHEN** a flow body runs a flow whose residual row contains an undeclared member
- **THEN** analysis rejects the body before HIR execution or backend emission

### Requirement: Fail transfers one owned payload abortively

`fail move value` SHALL consume one owned nominal payload, have success type `Never`, stop the current
flow execution, and transfer the payload to the nearest matching handler or caller. It MUST NOT be
catchable as a trap or copy an affine payload.

#### Scenario: Propagate an owned failure

- **WHEN** an inner flow fails with a declared nominal payload and no local handler matches
- **THEN** the payload leaves the inner execution once and the caller observes the same failure member

### Requirement: Exact-member catch subtracts and composes rows

`Flow.catch<E>(flow, handler)` and its equivalent pipeline spelling
`flow |> Flow.catch<E>(handler)` SHALL accept only an `E` present in the protected row and a
statically known `flow fn` handler accepting owned `E` and producing the same success type. The
resulting row SHALL be the protected row without `E`, union the handler row. Success bypasses the
handler; nonmatching members propagate unchanged.

#### Scenario: Recover one member

- **WHEN** a protected flow fails with `E`
- **THEN** the handler owns the payload and its successful result becomes the protected result

#### Scenario: Preserve another member

- **WHEN** the protected flow fails with another declared member
- **THEN** the handler is not entered and that member remains in the residual row

### Requirement: Traps remain outside typed failure

Bounds violations, integer traps, impossible compiler states, and violated unsafe contracts SHALL
remain abnormal traps. `Flow.catch` MUST NOT intercept them and the typed-failure ABI MUST NOT
translate them into nominal payloads.

#### Scenario: Arithmetic trap bypasses catch

- **WHEN** a protected flow divides by zero
- **THEN** execution traps without invoking a typed failure handler
