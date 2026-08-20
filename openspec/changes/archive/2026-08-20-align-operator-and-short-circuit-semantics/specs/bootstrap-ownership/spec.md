## ADDED Requirements

### Requirement: Short-circuit branches join ownership path-locally

Ownership SHALL analyze the right operand of `&&` and `||` as a conditional branch. Moves, loans,
mutation, and cleanup obligations inside that operand SHALL apply only on the executed path, and a
value used after the expression SHALL be valid only when it remains live on every reaching path.

#### Scenario: Clean a skipped affine operand

- **WHEN** a short-circuit condition skips a right operand that would consume an affine value
- **THEN** the skipped path retains and cleans that value exactly once

#### Scenario: Reject a use after a conditional move

- **WHEN** one reaching path moves a value in the right operand and source uses it afterward
- **THEN** ownership reports the ordinary use-after-move diagnostic with the move provenance

#### Scenario: End a conditional loan on its path

- **WHEN** a right operand creates a lexical borrow and then completes
- **THEN** the borrow ends before the branch joins and does not remain active on the skipped path
