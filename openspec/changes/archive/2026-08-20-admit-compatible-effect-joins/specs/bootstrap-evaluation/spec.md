## ADDED Requirements

### Requirement: Evaluation executes finite Effect composites exactly

Evaluation SHALL construct only the selected member of a finite Effect composite, preserve its
laziness, run exactly that member when requested, and retain its exact success or typed-failure
identity under the normalized joined channels. Dropping or completing the value SHALL clean only
the active member and SHALL introduce no allocation event.

#### Scenario: Run the selected success member

- **WHEN** a branch selects one of two compatible lazy Effects and the result is run
- **THEN** evaluation enters only the selected body and returns its exact success value

#### Scenario: Preserve selected failure identity

- **WHEN** the selected member fails with one member of the joined failure union
- **THEN** evaluation retains that exact failure member and payload while closing an unhandled entry failure with status one

#### Scenario: Drop a selected affine capture

- **WHEN** a composite holding one affine capture is dropped without running
- **THEN** evaluation records exactly one cleanup for that selected capture and none for inactive alternatives
