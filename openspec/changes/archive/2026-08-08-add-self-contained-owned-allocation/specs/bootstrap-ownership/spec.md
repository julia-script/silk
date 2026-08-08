## ADDED Requirements

### Requirement: Allocation cleanup is one affine obligation

Ownership SHALL treat every successful `Allocation`, `RawBuffer<T>`, construction guard, and owner
containing them as affine. Moves SHALL transfer one cleanup obligation; explicit drop and automatic
Drop SHALL consume it; a failed request SHALL create none. A live lexical Slot loan SHALL prevent
moving, dropping, or reborrowing its backing buffer incompatibly. Restricted hooks SHALL run before
field cleanup, and cleanup plans SHALL cover every structured exit while preserving the original
typed failure. Traps SHALL add no unwind promise.

#### Scenario: End a slot loan before buffer transfer

- **WHEN** a lexical slot projection ends before its backing RawBuffer moves into another owner
- **THEN** ownership ends the exclusive loan first and transfers one live cleanup obligation with the buffer

#### Scenario: Reject duplicate early drop

- **WHEN** source drops one allocation and later reads, moves, or drops the consumed binding
- **THEN** ownership emits one stable use-after-move diagnostic and publishes no conflicting cleanup plan

#### Scenario: Clean a typed failure path

- **WHEN** an allocating Effect fails after acquiring several affine locals
- **THEN** ownership schedules restricted hooks and field cleanup in the specified order before propagating the unchanged failure payload
