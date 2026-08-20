## ADDED Requirements

### Requirement: Custom operators select explicitly marked interface operations

An interface operation MAY declare one supported operator marker. Operator analysis SHALL select that operation through the same static conformance and specialization evidence used by ordinary bound calls, for concrete and generic operands. Operation spelling alone SHALL grant no compiler privilege.

#### Scenario: Multiply a vector by a scalar

- **WHEN** a conformance marks an operation with `*` and declares vector and scalar operands with a vector result
- **THEN** `vector * 2` selects that operation statically and preserves its declared result type

#### Scenario: Reject an unmarked same-named operation

- **WHEN** an interface declares a function named `multiply` without the operator marker
- **THEN** `*` does not select it merely because of its name

### Requirement: Short-circuit right operands use ordinary branch analysis

The right operand of `&&` and `||` SHALL be analyzed as an ordinary conditionally executed branch with normal type, Effect, ownership, loan, and cleanup rules. Runtime execution SHALL remain left-to-right and SHALL skip the right branch when the left value determines the result.

#### Scenario: Move only on the executed path

- **WHEN** a right operand moves an affine value and the short-circuit condition skips it
- **THEN** the skipped path retains ownership while the executed path records the move and its cleanup join

#### Scenario: Run an effectful right branch

- **WHEN** the surrounding context permits an Effect-producing boolean right operand
- **THEN** analysis composes its ordinary Effect contract and execution runs it only when required
