## MODIFIED Requirements

### Requirement: Logging remains effectful

Semantic logging SHALL remain an Effect operation with its declared Logger requirement. A log call
SHALL dispatch one complete semantic message in one Logger invocation rather than expose an API for
incrementally appending byte fragments,
so native, in-memory, browser, and telemetry providers can implement the same contract. Ordinary
functions that add logging MUST return or execute an Effect through the existing effect model; this
requirement MUST NOT introduce an eager non-effect trace, debugging intrinsic, or stdout shortcut.

#### Scenario: Propagate a temporary semantic log honestly

- **WHEN** a previously eager computation adds a Logger operation
- **THEN** its Effect and Logger requirements propagate to the execution boundary rather than bypassing the type system

#### Scenario: Compose logging through an Effect pipeline

- **WHEN** `Effect.log` is sequenced through `flatMap`, `tap`, recovery, or service provision
- **THEN** its event executes at the composed position and its Logger and failure channels remain explicit

## ADDED Requirements

### Requirement: Effect logging is ordinary source-defined API

`Effect.log` and its level-selecting sibling SHALL resolve to canonical ordinary Silk declarations.
The compiler MUST NOT select logging behavior from their names, actor, or standard-library origin.
Equivalent user code invoking the Logger service SHALL receive the same typing, ownership,
execution, and cleanup behavior.

#### Scenario: Navigate to Effect.log

- **WHEN** a program calls or navigates to `Effect.log`
- **THEN** the target is canonical shipped Silk source compiled through ordinary requirement and Effect composition paths
