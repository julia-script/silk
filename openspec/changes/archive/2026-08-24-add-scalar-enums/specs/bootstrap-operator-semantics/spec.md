## ADDED Requirements

### Requirement: Scalar enum operators preserve nominal boundaries

Operator analysis SHALL admit `==` and `!=` only for two operands of the same canonical scalar enum
type and SHALL produce `bool`. It SHALL admit no implicit enum/integer comparison, cross-enum
equality, arithmetic, truthiness, or direct ordering. Backing-value operations SHALL require the
explicit enum `value` conversion first.

#### Scenario: Accept homogeneous enum inequality

- **WHEN** `!=` receives two values of the same enum type
- **THEN** analysis returns `bool` and execution compares their canonical members

#### Scenario: Reject enum ordering

- **WHEN** `<` receives two values of the same enum type
- **THEN** analysis reports the enum-ordering diagnostic rather than selecting the representation integer operator
