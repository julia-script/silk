## MODIFIED Requirements

### Requirement: Vector access is checked

Reading a supported non-union Copy element SHALL require only a shared `Vector<T>` borrow, return
the element by value, and validate the index against the current length using the same checked
access contract as fixed arrays. A successful read MUST NOT mutate or move the vector, allocate,
change cleanup state, or require an exclusive borrow. Access to an index at or beyond the length
SHALL be rejected before element storage is read.

#### Scenario: In-bounds shared read

- **WHEN** a program reads index `i` with `i < length` through a shared vector borrow
- **THEN** it observes the element most recently stored at `i` and may read the same vector again through another shared alias

#### Scenario: Out-of-bounds read is rejected

- **WHEN** a program reads an index at or beyond the current length
- **THEN** the checked-access contract rejects it identically in the evaluator, LLVM, and Wasm engines before element storage is read

#### Scenario: Shared read has no ownership side effects

- **WHEN** a program reads an element from a live vector and later moves or drops that vector
- **THEN** the move or drop observes the original storage and releases every initialized element and allocation exactly once

#### Scenario: Reject an unsupported element type

- **WHEN** `Vector.get` is instantiated for a move-only or structural-union element type
- **THEN** semantic validation rejects the read without changing the vector

