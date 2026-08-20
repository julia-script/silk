## ADDED Requirements

### Requirement: HIR retains operator evidence and conditional right regions

HIR SHALL record the exact interface operation, applied capability, provider, substituted operand
types, result type, and witness evidence selected by a custom operator before specialization. `&&`
and `||` SHALL remain typed conditional HIR whose right operand is a distinct region rather than an
eager call or a purity-restricted expression.

#### Scenario: Inspect a custom operator selection

- **WHEN** a concrete or generic custom operator is accepted
- **THEN** HIR identifies the marked operation and ordinary conformance question without runtime dispatch data

#### Scenario: Preserve an effectful right operand

- **WHEN** a valid right operand executes an Effect conditionally
- **THEN** HIR retains the run site inside only the short-circuit right region
