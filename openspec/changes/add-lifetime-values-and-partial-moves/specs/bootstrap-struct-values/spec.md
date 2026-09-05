## RENAMED Requirements

- FROM: `### Requirement: Bootstrap structs move only as whole values`
- TO: `### Requirement: Structs support complete and eligible partial moves`

## MODIFIED Requirements

### Requirement: Structs support complete and eligible partial moves

User-defined struct values SHALL remain affine unless their explicit sealed Copy implementation is admitted. Moving a complete struct SHALL consume its source place and later use SHALL be rejected. Ordinary reads of Copy fields SHALL preserve owner initialization. Eligible definitely initialized visible fields SHALL support partial moves with independent sibling use, unchanged-type restoration, and exact remainder cleanup under ordinary ownership rules. Whole-value uses SHALL require completeness. Consuming an affine bound struct SHALL require explicit move; a fresh literal or call result SHALL flow directly because it has no source binding that could be copied accidentally.

#### Scenario: Move a complete struct through a call

- **WHEN** a caller passes `move token` to an owning nominal parameter
- **THEN** the callee receives the complete value and every later use of the caller's `token` is a use-after-move

#### Scenario: Return a newly constructed struct

- **WHEN** a factory returns a complete struct literal
- **THEN** the value crosses the return boundary as one owned nominal value without an intermediate partial state

#### Scenario: Refuse an implicit nominal copy

- **WHEN** an affine bound struct is passed to an owning parameter without `move`
- **THEN** ownership rejects the transfer and leaves the source binding live

#### Scenario: Move one eligible nested field

- **WHEN** code attempts `move outer.inner` from a definitely initialized visible field without a conflicting loan or enclosing user Drop
- **THEN** ownership transfers inner and preserves initialization and cleanup for the outer remainder

#### Scenario: Refuse moving one nested field

- **WHEN** a requested nested field move crosses an enclosing user Drop hook
- **THEN** ownership rejects the move despite any promised later restoration
