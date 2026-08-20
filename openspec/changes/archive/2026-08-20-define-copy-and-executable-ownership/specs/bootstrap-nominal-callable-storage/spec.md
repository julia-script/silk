## ADDED Requirements

### Requirement: Representation-bearing nominals use ordinary aggregate ownership

A nominal containing a callable representation field SHALL derive Copy, affine ownership, moves,
partial-move rejection, and cleanup from its admitted `impl Copy` and the concrete realized capture
fields. A callable representation with only Copy snapshots or shared borrows MAY participate in a
Copy aggregate; an owned affine or exclusive capture SHALL keep the aggregate affine. Take-once
invocation SHALL consume the complete aggregate, and access validation remains independent.

#### Scenario: Copy a reusable stored callable

- **WHEN** an aggregate validly implements `Copy` and its concrete callable realization contains only Copy captures
- **THEN** an ordinary read duplicates the complete aggregate and neither source has cleanup

#### Scenario: Reject direct affine callable extraction

- **WHEN** source attempts to move an affine callable field out of an otherwise live aggregate
- **THEN** ownership reports the ordinary aggregate partial-move diagnostic

## REMOVED Requirements

### Requirement: Representation-bearing nominals remain move-only

**Reason:** Executable representation is no longer a blanket affine classification; ordinary sealed Copy and aggregate ownership decide it.

**Migration:** Keep affine executable aggregates unchanged, and add a valid empty `impl Copy` only when every concrete realization is Copy and cleanup-free.
