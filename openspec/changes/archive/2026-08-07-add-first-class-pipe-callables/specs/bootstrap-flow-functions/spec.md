## ADDED Requirements

### Requirement: Effect combinators accept ordinary callable values

`Effect.map`, `flatMap`, `tap`, `catch`, and other higher-order Effect operations SHALL accept
ordinary callable values and automatic sections under explicit callable contracts. Their direct
data-first forms and piped section forms SHALL be semantically identical. `map` SHALL preserve a
returned Effect as a nested success value, while `flatMap` and the Effect-specific behavior of
`tap` SHALL compose execution according to their declared contracts.

#### Scenario: Map with an arithmetic section

- **WHEN** `succeed(2) |> Effect.map(I32.add(2))` is run
- **THEN** the section maps the success to `4` without pipeline-specific callback syntax

#### Scenario: Keep effectful logging out of map

- **WHEN** an effectful logging function is passed to `Effect.map`
- **THEN** its Effect result remains nested rather than being executed implicitly

### Requirement: Callable captures derive composed Effect access

An Effect combinator that stores a callback SHALL incorporate the callback environment's shared,
exclusive, or consuming access into the resulting Effect's run access. Retry MUST reject a composed
Effect whose callback or input Effect is take-once. Dropping the composed Effect without running it
SHALL release the stored callback exactly once.

#### Scenario: Make map take-once

- **WHEN** `Effect.map` captures a mapper that consumes one owned capture
- **THEN** the mapped Effect is take-once and a second run is rejected before invoking the mapper

#### Scenario: Preserve exclusive callback state

- **WHEN** a mapped Effect uses a `mut fn` callback across repeated runs
- **THEN** each run requires exclusive Effect access and observes the callback's retained mutations

### Requirement: Logging remains effectful

Semantic logging SHALL remain an Effect operation with its declared Logger requirement. Ordinary
functions that add logging MUST return or execute an Effect through the existing effect model; this
change MUST NOT introduce an eager non-effect trace or debugging intrinsic.

#### Scenario: Propagate a temporary semantic log honestly

- **WHEN** a previously eager computation adds a Logger operation
- **THEN** its Effect and Logger requirements propagate to the execution boundary rather than bypassing the type system
