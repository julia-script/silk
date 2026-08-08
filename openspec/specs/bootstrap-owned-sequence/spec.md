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

Reading an element SHALL validate the index against the current length using the same checked
access contract as fixed arrays; access to an index at or beyond the length SHALL be rejected
before any storage is touched.

#### Scenario: In-bounds read

- **WHEN** a program reads index `i` with `i < length`
- **THEN** it observes the element most recently stored at `i`

#### Scenario: Out-of-bounds read is rejected

- **WHEN** a program reads an index at or beyond the current length
- **THEN** the checked-access contract rejects it identically in the evaluator, LLVM, and Wasm engines

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

### Requirement: Vector is ordinary library code

`Vector<T>` SHALL be implemented entirely in Silk standard-library source over `Allocator`,
`Allocation`, unsafe typed storage, and restricted `Drop`. Compiler phases, MIR, the evaluator, and
the backends MUST NOT contain vector-specific operations, layouts, or branches, and no iterable or
iterator abstraction is required.

#### Scenario: No collection primitive in published artifacts

- **WHEN** MIR, evaluator traces, or backend output for a vector-using program are inspected
- **THEN** they contain only the existing allocation, storage, call, and cleanup forms with no vector-shaped operation
