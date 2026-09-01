## ADDED Requirements

### Requirement: Operators resolve homogeneously across integers

Arithmetic and comparison operators SHALL resolve only for compatible operands of the same integer type. They SHALL select that type's signed or unsigned checked semantics without implicit conversion, overload lookup, truthiness, or operand reordering. Prefix negation SHALL support signed integers only; logical negation SHALL support `bool` only.

#### Scenario: Resolve unsigned multiplication

- **WHEN** both operands of `*` are `u32`
- **THEN** the operator selects checked unsigned multiplication returning `u32`

#### Scenario: Reject mixed widths

- **WHEN** operands have types `i32` and `i64`
- **THEN** operator analysis rejects them without conversion

## REMOVED Requirements

### Requirement: Operators resolve to compiler-known actor operations

**Reason**: The `I32`/`Bool`-specific resolver is replaced by the complete lowercase integer-family resolver.

**Migration**: Use homogeneous lowercase integer operands and actors.

### Requirement: Operators resolve homogeneously for Usize

**Reason**: `usize` now follows the same rule as every integer type.

**Migration**: Use the complete integer operator requirement.
