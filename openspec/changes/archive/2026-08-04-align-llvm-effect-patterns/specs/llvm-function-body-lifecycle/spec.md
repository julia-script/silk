## Purpose

Defines function-body construction as a scoped transaction whose reservation, draft, validation, commit, and cleanup remain correct under every Effect exit.

## ADDED Requirements

### Requirement: Successful body construction commits atomically

Function-body construction SHALL expose a draft only to the supplied action and its creating fiber. A successful action and validation SHALL commit one complete immutable body, close the draft, and release the function's build reservation.

#### Scenario: Valid body commits

- **WHEN** the action creates a valid body and completes successfully
- **THEN** the function contains the completed body
- **AND** the draft can no longer be used
- **AND** the build reservation is released

### Requirement: Unsuccessful construction always cleans up

Function-body construction SHALL close its draft, release its build reservation, and leave the function declaration retryable when the action fails, dies, or is interrupted, or when validation or commit fails.

#### Scenario: Action fails with a typed error

- **WHEN** the body action fails before commit
- **THEN** no partial body is visible
- **AND** a later valid construction attempt for the same function can succeed

#### Scenario: Action dies

- **WHEN** the body action terminates with a defect
- **THEN** the defect is preserved
- **AND** the draft is closed and the build reservation is released
- **AND** a later valid construction attempt for the same function can succeed

#### Scenario: Action is interrupted

- **WHEN** the fiber constructing a body is interrupted
- **THEN** interruption is preserved
- **AND** the draft is closed and the build reservation is released
- **AND** a later valid construction attempt for the same function can succeed

### Requirement: Concurrent construction remains exclusive

At most one function-body construction transaction SHALL hold a reservation for a function at a time.

#### Scenario: Second construction overlaps the first

- **WHEN** a second fiber attempts to build a function while its first construction transaction is active
- **THEN** the second operation fails with `LlvmError`
- **AND** it does not disturb the active transaction

### Requirement: Action contracts are preserved

Function-body construction SHALL preserve the action's success value, typed error union, and service requirements while adding only the LLVM failures required for reservation, validation, and commit.

#### Scenario: Action requires a service and returns a value

- **WHEN** a body action uses an Effect service and returns a value after building valid IR
- **THEN** the construction effect retains that service requirement
- **AND** successful construction returns the action's value
