## ADDED Requirements

### Requirement: Backends realize referent places consistently

Native and Wasm backends SHALL realize verified referent reads, reborrows, and replacements using
the existing reference representation and ordinary address, load, store, and cleanup operations.
They SHALL require no new runtime entry point or intrinsic and SHALL agree with evaluation.

#### Scenario: Execute scalar Display receiver access

- **WHEN** an integer Display witness reads `self.*`
- **THEN** native and Wasm output agrees with evaluation

#### Scenario: Execute exclusive replacement

- **WHEN** verified MIR replaces an exclusive referent
- **THEN** native and Wasm update the same backing storage and preserve cleanup behavior

#### Scenario: Preserve projected address identity

- **WHEN** a referent is followed by field or index projections, including a zero-lane target
- **THEN** both backends derive the same canonical address without inventing runtime state
