## ADDED Requirements

### Requirement: HIR retains typed referent places

HIR SHALL represent referent projection explicitly with its subject, target type, access,
provenance, source span, and place-chain identity. HIR SHALL distinguish Copy reads, reborrows, and
replacement contexts without rewriting the projection into an intrinsic call.

#### Scenario: Retain a scalar Copy read

- **WHEN** `self.*` is elaborated for `self: &i32`
- **THEN** HIR contains a typed referent place followed by an ordinary Copy read

#### Scenario: Retain a borrowed chained place

- **WHEN** `&mut value.*.field` is elaborated
- **THEN** HIR retains the referent and field projections plus exclusive borrow provenance

#### Scenario: Omit invalid referent HIR

- **WHEN** semantic analysis cannot establish a reference subject
- **THEN** no executable referent-place HIR is produced for that expression
