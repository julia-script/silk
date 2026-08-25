## ADDED Requirements

### Requirement: HIR retains scalar enum and member identity

Typed HIR SHALL represent an enum value with its canonical enum type and declared member identity.
HIR for member construction, `value`, equality, and enum-member patterns SHALL retain resolved source
provenance and SHALL NOT erase the value to an untyped integer. Invalid or recovered enum expressions
SHALL become explicit unavailable HIR while unrelated declarations remain complete.

#### Scenario: Lower a member value and value conversion

- **WHEN** a function returns `Status.value(Status.Unknown)`
- **THEN** HIR records the `Status` type, `Unknown` member identity, exact representation result type, and source spans

#### Scenario: Retain damage locally

- **WHEN** one qualified member expression names an unknown member
- **THEN** that expression's HIR is unavailable with its diagnostic identity while valid sibling functions retain complete HIR
