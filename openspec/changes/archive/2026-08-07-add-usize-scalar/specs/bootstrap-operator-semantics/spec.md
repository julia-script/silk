## ADDED Requirements

### Requirement: Operators resolve homogeneously for Usize

The established prefix, arithmetic, equality, and ordering pipeline SHALL resolve binary `Usize`
operators only when both operands are `Usize`. It SHALL select unsigned checked semantics and a
`Usize` arithmetic or `Bool` comparison result without introducing overload lookup, implicit
conversion, or backend-specific operator identity. Unary minus on `Usize` SHALL be rejected.

#### Scenario: Resolve checked multiplication

- **WHEN** both operands of `*` have canonical type `Usize`
- **THEN** operator facts select checked unsigned multiplication returning `Usize`

#### Scenario: Reject unary minus

- **WHEN** unary `-` is applied to a `Usize` expression
- **THEN** operator analysis reports that the prefix operation is unavailable for that type
