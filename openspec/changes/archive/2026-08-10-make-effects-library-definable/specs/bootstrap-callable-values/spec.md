## ADDED Requirements

### Requirement: Effect values cross ordinary higher-order boundaries

Closed Effect values SHALL be valid ordinary parameter, result, local-binding, capture, and generic-
argument values without exposing or erasing their hidden construction-site identity. Passing or
capturing an Effect SHALL preserve its success, failure, requirement, and run-access contracts and
the ownership of every hidden environment field.

#### Scenario: Implement map as an ordinary function

- **WHEN** a generic source function accepts one Effect and one unary callable and returns an Effect that runs the input later
- **THEN** its returned Effect retains both hidden environments and derives the strongest required shared, exclusive, or consuming run access

#### Scenario: Preserve a take-once input

- **WHEN** a source combinator captures an Effect that owns an affine value consumed during execution
- **THEN** the composition remains take-once and ownership rejects a second run without requiring compiler knowledge of the combinator's name

### Requirement: Effectful channel callbacks are ordinary callables

Generic source combinators SHALL accept ordinary shared, exclusive, or consuming callbacks that
return values or Effects. Specialization and ownership SHALL derive callback invocation count,
captured state, failure and requirement rows, and cleanup from the callable contract and function
body rather than an Effect-specific callback category.

#### Scenario: Compose an effectful failure callback

- **WHEN** source-defined recovery invokes an ordinary callback returning `Effect<A ! F ? S>` on the failure branch
- **THEN** the composition retains the untouched success path and exposes normalized failure `F` and requirements `R | S`
