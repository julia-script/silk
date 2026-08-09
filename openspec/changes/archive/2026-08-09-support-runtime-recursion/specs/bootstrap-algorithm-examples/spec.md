## ADDED Requirements

### Requirement: Quicksort is an executable recursive algorithm

The in-place quicksort example SHALL recursively partition and sort its committed signed integers
through evaluation, native LLVM, and direct WebAssembly. It MUST NOT be rewritten as an iterative
fixture or granted an algorithm-specific recursion exception.

#### Scenario: Sort through recursive partitions

- **WHEN** quicksort processes `[9, -3, 5, 1, 0, -8, 7, 2]`
- **THEN** all three engines return the fingerprint for `[-8, -3, 0, 1, 2, 5, 7, 9]`
