## ADDED Requirements

### Requirement: Evaluation executes referent places through backing identity

Evaluation SHALL resolve referent places to the original backing storage, copy sealed-Copy targets
without consuming their owner, preserve call-scoped loan suspension and restoration, and perform
exclusive replacement with ordinary cleanup.

#### Scenario: Evaluate a scalar referent read

- **WHEN** a shared `u32` referent is read
- **THEN** evaluation returns the exact scalar value and leaves the backing owner available

#### Scenario: Evaluate an exclusive replacement

- **WHEN** an exclusive referent is assigned a compatible replacement
- **THEN** subsequent reads observe the replacement and the previous value is cleaned up once

#### Scenario: Evaluate a nested reborrow

- **WHEN** a value-reference parameter is reborrowed for a helper call
- **THEN** the child aliases the original backing identity
- **AND** the parent is restored when the call returns
