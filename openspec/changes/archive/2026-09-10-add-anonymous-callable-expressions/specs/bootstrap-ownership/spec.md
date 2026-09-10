## ADDED Requirements

### Requirement: Anonymous environments obey capture ownership and cleanup

Constructing an anonymous callable SHALL acquire each implicit capture exactly once in
first-reference source order. Copy snapshots and shared loans SHALL permit shared repeated
invocation; exclusive loans SHALL remain live with the environment and require exclusive repeated
invocation; moved affine owners SHALL transfer into a consuming environment. Borrowed captures MUST
NOT escape their permitted region. Dropping an uninvoked environment SHALL release loans and clean
owned captures exactly once in reverse acquisition order. A successful consuming invocation SHALL
transfer or clean every owned capture exactly once, and a second consuming invocation MUST be
rejected before it can duplicate an owner.

#### Scenario: Reuse a shared capture

- **WHEN** an anonymous callable reads one shared-borrowed outer value twice across two invocations
- **THEN** both calls observe the same valid loan and the loan ends when the callable's last use ends

#### Scenario: Mutate an exclusive capture

- **WHEN** a `mut fn` anonymous callable updates an exclusively borrowed outer value across sequential invocations
- **THEN** mutation persists between calls and no competing access is accepted while the environment remains live

#### Scenario: Drop an uninvoked moved capture

- **WHEN** a `once fn` anonymous callable captures `move token` and leaves its region without invocation
- **THEN** the environment drops `token` exactly once

#### Scenario: Reject an escaping borrowed environment

- **WHEN** an anonymous callable captures a local borrow and is returned beyond that borrow's valid region
- **THEN** ownership rejects the escape under the same region rules as other stored callable environments
