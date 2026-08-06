## ADDED Requirements

### Requirement: Writes require exclusive live ownership

Ownership checking SHALL permit a write only when its root is one live mutable owner and no conflicting
access is active. Replacement SHALL transfer the right-hand value into the place, discharge the old
non-Copy value exactly once, and leave the complete root initialized. Moving from a place and writing
it later SHALL NOT provide partial-initialization semantics during bootstrap.

#### Scenario: Replace a move-only element

- **WHEN** a mutable array element is replaced by a complete move-only value
- **THEN** the old element receives one cleanup action and the array remains one complete live owner

#### Scenario: Reject mutation through an immutable root

- **WHEN** a field or index place is structurally valid but its root binding is immutable
- **THEN** ownership rejects the write without changing root liveness or cleanup

### Requirement: Loop ownership is a deterministic fixed point

Ownership SHALL analyze a structured loop until its header state reaches a deterministic fixed point.
Every path that repeats SHALL re-enter with compatible liveness and complete initialization; every
path that exits SHALL carry the appropriate live owners. A value moved on one repeating path MUST be
reinitialized before that path continues, otherwise the loop is rejected.

#### Scenario: Reassign before continuing

- **WHEN** an iteration moves a mutable binding, assigns a complete replacement, and continues
- **THEN** the next iteration begins with the binding live and initialized

#### Scenario: Reject a conditionally missing owner

- **WHEN** one path moves a non-Copy binding and continues without replacing it while another path retains it
- **THEN** ownership reports the incompatible loop-header state rather than widening it to available

### Requirement: Loop cleanup follows lexical exits

The cleanup plan SHALL attach exact ordered releases to iteration fallthrough, `continue`, `break`,
and `return` according to the lexical regions each transfer leaves. Loop repetition MUST NOT duplicate
cleanup obligations, and a `break` MUST preserve owners declared outside the loop for subsequent use.

#### Scenario: Clean nested loop exits

- **WHEN** an inner loop breaks while its outer iteration remains active
- **THEN** only inner-loop locals are released and outer-loop owners remain live
