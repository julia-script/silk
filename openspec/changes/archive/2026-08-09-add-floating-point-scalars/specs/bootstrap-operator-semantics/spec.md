## ADDED Requirements

### Requirement: Operators resolve homogeneously for floats

Arithmetic, negation, equality, and ordering operators SHALL resolve for two operands of the same float width using conservative IEEE semantics. They MUST NOT mix widths, convert implicitly, or search source overloads.

#### Scenario: Resolve f64 division

- **WHEN** both `/` operands are `f64`
- **THEN** the operator selects canonical IEEE `f64` division

#### Scenario: Reject mixed float widths

- **WHEN** operands are `f32` and `f64`
- **THEN** analysis rejects them without conversion
