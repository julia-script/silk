## ADDED Requirements

### Requirement: Referent projection facts retain target and provenance

Semantic analysis SHALL require a reference-typed subject for `.*` and publish the referent target
type, shared or exclusive access, borrow provenance, availability, span, and projection-chain
identity. A failed projection SHALL retain an explicit failed fact instead of fabricating a target.

#### Scenario: Resolve a shared scalar referent

- **WHEN** `value.*` is analyzed for `value: &u32`
- **THEN** its target fact is `u32` with shared access and the provenance of `value`

#### Scenario: Resolve a chained referent place

- **WHEN** `value.*.field` is analyzed for a reference to a record
- **THEN** the field fact retains the referent projection in its canonical place chain

#### Scenario: Reject a non-reference subject

- **WHEN** `value.*` is analyzed and `value` is not reference-typed
- **THEN** analysis reports the dedicated invalid-referent diagnostic
- **AND** the projection fact is unavailable
