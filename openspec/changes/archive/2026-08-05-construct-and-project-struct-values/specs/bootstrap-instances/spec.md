## ADDED Requirements

### Requirement: Runtime aggregate reachability follows nominal values

Instance discovery SHALL follow canonical nominal types appearing in reachable parameters, results,
bindings, struct constructions, projections, and cleanup behavior. It SHALL recursively include
each nominal field type needed to realize those values while continuing to omit declarations and
types that no runtime instance reaches.

#### Scenario: Discover a factory's aggregate result

- **WHEN** `main` calls a reachable factory returning `Token` and projects `Token.kind`
- **THEN** discovery records the factory instance, canonical `Token` runtime type, and its recursively required field types

#### Scenario: Omit an unused nominal declaration

- **WHEN** another valid struct is declared but never appears in a reachable value path
- **THEN** it remains in the declaration-wide catalog but is absent from runtime aggregate reachability

### Requirement: Aggregate-bearing instance keys stay canonical

Function instances whose contracts contain nominal structs SHALL key those types by canonical
module and declaration identity, never by field shape, literal spelling, source field order, or
backend representation. Repeated discovery SHALL produce identical worklist and instance ordering.

#### Scenario: Distinguish equal-shaped parameters

- **WHEN** reachable functions accept equal-shaped structs declared in different modules
- **THEN** discovery records distinct canonical instance keys for the two nominal parameter types
