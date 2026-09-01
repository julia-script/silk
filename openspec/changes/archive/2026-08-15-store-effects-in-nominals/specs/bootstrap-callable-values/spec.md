## MODIFIED Requirements

### Requirement: Effect values cross ordinary higher-order boundaries

Closed Effect values SHALL be valid ordinary parameter, result, local-binding, capture, generic-
argument, and concretely represented nominal-field values without exposing or erasing their hidden
construction-site identity. Passing, capturing, or storing an Effect SHALL preserve its success,
failure, requirement, and run-access contracts and the ownership of every hidden environment field.
A structural Effect contract has no standalone target layout; a concrete represented environment MAY
contribute inline lanes only through its complete enclosing nominal realization.

#### Scenario: Implement map as an ordinary function

- **WHEN** a generic source function accepts one Effect and one unary callable and returns an Effect that runs the input later
- **THEN** its returned Effect retains both hidden environments and derives the strongest required shared, exclusive, or consuming run access

#### Scenario: Preserve a take-once input

- **WHEN** a source combinator captures an Effect that owns an affine value consumed during execution
- **THEN** the composition remains take-once and ownership rejects a second run without requiring compiler knowledge of the combinator's name

#### Scenario: Store a concrete Effect realization

- **WHEN** a complete nominal specialization stores one Effect representation in a field
- **THEN** its runner and environment remain lazy, inline, statically targeted, and unavailable through the structural contract alone
