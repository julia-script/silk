## MODIFIED Requirements

### Requirement: Representation-bearing nominals use ordinary aggregate ownership

A nominal containing a callable representation field SHALL derive Copy, affine ownership, moves,
partial moves, and cleanup from its admitted `impl Copy` and the concrete realized capture
fields. A callable representation with only Copy snapshots or shared borrows MAY participate in a
Copy aggregate; an owned affine or exclusive capture SHALL keep the aggregate affine. Take-once
invocation SHALL consume the complete callable value and transfer its environment; extracting an initialized callable field first SHALL leave the eligible containing aggregate partial. Access validation remains independent.

#### Scenario: Copy a reusable stored callable

- **WHEN** an aggregate validly implements `Copy` and its concrete callable realization contains only Copy captures
- **THEN** an ordinary read duplicates the complete aggregate and neither source has cleanup

#### Scenario: Transfer an affine callable field

- **WHEN** source moves a definitely initialized affine callable field out of an eligible aggregate
- **THEN** ownership transfers the callable and capture obligations while preserving initialized sibling fields

#### Scenario: Reject direct affine callable extraction

- **WHEN** source tries to move a callable field through a shared reference or an enclosing user Drop boundary
- **THEN** ownership rejects the move without leaving borrowed storage or a Drop receiver incomplete
