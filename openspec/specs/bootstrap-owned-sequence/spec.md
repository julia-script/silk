# bootstrap-owned-sequence Specification

## Purpose

Define the behavior contract of `Vector<T>`, the first growable owned sequence, implemented as
ordinary Silk standard-library code over the owned-allocation substrate with no compiler-known
collection behavior.

## Requirements

### Requirement: Vector construction and append grow atomically

`Vector<T>` SHALL support creating an empty vector without allocating, appending owned elements
with amortized geometric growth, and reporting length and capacity. Growth SHALL allocate a
replacement buffer, move exactly the initialized elements, commit only after every move succeeds,
and then release the old buffer. Append SHALL fail only with the typed `OutOfMemory` failure of the
underlying allocator requirement.

#### Scenario: Grow across a reallocation

- **WHEN** appends exceed the current capacity
- **THEN** the vector reallocates, every previously appended element is observable at its original index afterward, and the old buffer is released exactly once

#### Scenario: Failed growth preserves the vector

- **WHEN** append requires growth and the replacement allocation fails
- **THEN** append fails with `OutOfMemory`, the vector retains its prior elements, length, and capacity, and no element or allocation is leaked or dropped twice

#### Scenario: Empty vector costs nothing

- **WHEN** a vector is created and dropped without any append
- **THEN** no allocation is requested and cleanup releases nothing

### Requirement: Vector access is checked

Reading a recursively Copy element SHALL require only a shared `Vector<T>` borrow, return the element
by value, and validate the index against the current length using the same checked access contract
as fixed arrays. Supported elements SHALL include structural unions exactly when every member is
Copy and cleanup-free. A successful read MUST NOT mutate or move the vector, allocate, change
cleanup state, or require an exclusive borrow. Access to an index at or beyond the length SHALL be
rejected before element storage is read.

#### Scenario: In-bounds shared read

- **WHEN** a program reads index `i` with `i < length` through a shared vector borrow
- **THEN** it observes the element most recently stored at `i` and may read the same vector again through another shared alias

#### Scenario: Read an all-Copy structural union

- **WHEN** a vector element is a structural union whose nominal members contain only Copy fields
- **THEN** shared `Vector.get` returns the same active member and payload while leaving the vector available for another read

#### Scenario: Out-of-bounds read is rejected

- **WHEN** a program reads an index at or beyond the current length
- **THEN** the checked-access contract rejects it identically in the evaluator, LLVM, and Wasm engines before element storage is read

#### Scenario: Shared read has no ownership side effects

- **WHEN** a program reads an element from a live vector and later moves or drops that vector
- **THEN** the move or drop observes the original storage and releases every initialized element and allocation exactly once

#### Scenario: Reject a move-only element type

- **WHEN** `Vector.get` is instantiated for a move-only nominal or structural-union element type
- **THEN** compiler verification rejects the read before evaluation or backend emission without changing the vector

### Requirement: Vector ownership and release are deterministic

`Vector<T>` SHALL be an affine owner: moving it transfers the storage and elements to the
destination, automatic cleanup on every structured exit releases live vectors exactly once, and
release SHALL destroy exactly the initialized elements before releasing the backing allocation.
Explicit `drop` SHALL release at that statement.

#### Scenario: Move transfers the whole sequence

- **WHEN** a vector moves through an ordinary function boundary and its original binding leaves scope
- **THEN** only the destination remains live and eventual cleanup destroys each element and the buffer exactly once

#### Scenario: Elements are destroyed before storage

- **WHEN** a live vector of Drop-bearing elements is released on any structured exit
- **THEN** each initialized element's Drop runs before the backing allocation is released, and uninitialized capacity is not treated as an element

#### Scenario: Early drop releases immediately

- **WHEN** source explicitly drops a live vector before its block ends
- **THEN** elements and storage are released at that statement and automatic cleanup does not release them again

### Requirement: Vector ordering is stable and deterministic

`Vector<T>` SHALL support ordering its elements in place for any element type carrying an `Order`
witness. The order SHALL be total and stable: two elements that compare equal SHALL keep their input
order relative to one another. The order SHALL be deterministic — the same input SHALL always
produce the same output, and the evaluator, LLVM, and Wasm SHALL agree on that output — because
every comparison and every exchange is decided by run boundaries alone and never by an address, a
capacity, or an engine detail. Ordering SHALL move each element at most once per exchange, so no
element is duplicated, leaked, or dropped twice, and SHALL NOT require the element type to be `Copy`
to move an element. Ordering allocates a scratch buffer and therefore SHALL carry the typed
`OutOfMemory` failure and the allocator requirement.

`Vector<T>` SHALL support searching a sorted vector for an element through the same `Order` witness,
returning an optional index that is present only when a matching element exists. The search SHALL
return the lowest matching index when several elements compare equal, so repeated searches over one
vector answer identically.

#### Scenario: Order an unsorted vector

- **WHEN** a program orders a vector whose elements are not in order
- **THEN** every element afterward compares no greater than the element following it, and the length and every element value are unchanged

#### Scenario: Equal elements keep their input order

- **WHEN** a vector holding several elements that compare equal is ordered
- **THEN** those elements keep their input order relative to one another, and ordering the result again leaves it unchanged

#### Scenario: Ordering an empty or one-element vector

- **WHEN** a program orders a vector holding no element or exactly one element
- **THEN** the vector is unchanged and no comparison is required

#### Scenario: Three engines agree on one order

- **WHEN** the same program orders the same input on the evaluator, on LLVM, and on Wasm
- **THEN** the three engines observe the same element at every index

#### Scenario: Ordering releases every allocation it acquires

- **WHEN** a program orders a vector and the vector is later released
- **THEN** every allocation the ordering acquired is released exactly once, and each element is destroyed exactly once

#### Scenario: Search a sorted vector

- **WHEN** a program searches a sorted vector for an element it holds and for one it does not
- **THEN** the present element yields its index and the missing element yields an absent value

### Requirement: Vector is ordinary library code

`Vector<T>` SHALL be implemented entirely in Silk standard-library source over `Allocator`,
`Allocation`, unsafe typed storage, and restricted `Drop`. Compiler phases, MIR, the evaluator, and
the backends MUST NOT contain vector-specific operations, layouts, or branches, and no iterable or
iterator abstraction is required.

#### Scenario: No collection primitive in published artifacts

- **WHEN** MIR, evaluator traces, or backend output for a vector-using program are inspected
- **THEN** they contain only the existing allocation, storage, call, and cleanup forms with no vector-shaped operation
