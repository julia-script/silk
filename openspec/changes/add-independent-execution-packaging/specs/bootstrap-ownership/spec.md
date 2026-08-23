## ADDED Requirements

### Requirement: Execution construction and drive have consuming cleanup matrices

Ownership SHALL model `executionFromAllocation` as one all-or-nothing consuming transition over the
Allocation, body, endpoint state, and endpoint callback. It SHALL model `drive` as consuming the
Execution, affine branch state, and both take-once callbacks, with exactly one callback receiving
the branch state. Completion SHALL transfer `A`, clean the unused suspension callback and remaining
package values, and discharge the Execution obligation. Suspension SHALL clean the unused
completion callback and transfer the same Execution obligation through `onSuspend`. Dropping an
Initial or returned Execution SHALL clean all live values exactly once in dependency-safe order.

#### Scenario: Consume initializer inputs

- **WHEN** a valid initializer executes
- **THEN** Allocation, body, endpoint state, and endpoint callback sources end and one Initial Execution owns all corresponding obligations

#### Scenario: Complete through one branch

- **WHEN** drive completes
- **THEN** the completion callback receives the sole branch state and `A`, the suspension callback is cleaned once, and no Execution obligation remains

#### Scenario: Suspend through one branch

- **WHEN** drive externally parks
- **THEN** the suspension callback receives the sole branch state and same Execution obligation, while the completion callback is cleaned once

#### Scenario: Clean a never-driven body

- **WHEN** an Initial Execution owning affine captures is dropped
- **THEN** every capture and endpoint value is cleaned once before the package Allocation and no callback obligation is invoked as control flow

#### Scenario: Preserve no-unwind trap semantics

- **WHEN** execution-stack growth or an illegal intrinsic state traps
- **THEN** ownership publishes no promised cleanup or recoverable failure beyond Silk's language-wide no-unwind contract
