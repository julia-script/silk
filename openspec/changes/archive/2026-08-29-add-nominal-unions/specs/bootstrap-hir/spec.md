## ADDED Requirements

### Requirement: HIR retains nominal union construction and selection identity

Typed HIR SHALL represent union construction with the canonical applied parent, selected canonical
variant, ordered specialized field initializers, source provenance, and precise nominal result type.
HIR match selections SHALL retain hierarchical coverage identities, field bindings, omissions,
access mode, and active-variant cleanup without erasing a union to a structural member or numeric tag.

#### Scenario: Lower one generic variant construction

- **WHEN** analysis accepts `Result<i32, Problem>.Success { value: 42 }`
- **THEN** HIR records the applied `Result` identity, `Success` variant identity, specialized `value: i32` field, and nominal `Result<i32, Problem>` result

#### Scenario: Retain direct nested coverage

- **WHEN** a match selects `HttpError.Dns` directly from `HttpError | OutOfMemoryError`
- **THEN** HIR retains both the outer structural member and inner nominal variant selection with exact bindings and cleanup
