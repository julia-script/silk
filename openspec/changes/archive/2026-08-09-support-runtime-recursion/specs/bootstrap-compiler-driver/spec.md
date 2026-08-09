## ADDED Requirements

### Requirement: Differential gates execute terminating recursion

The compiler driver corpus SHALL execute representative direct recursion, mutual recursion, generic
same-argument recursion, and recursion over a mutable slice through evaluation, native LLVM, and
direct WebAssembly. Completing programs SHALL agree on results and caller-visible mutations, while
fresh-process compiler artifacts remain deterministic.

#### Scenario: Compare recursive quicksort engines

- **WHEN** the committed in-place quicksort recursively partitions its mutable slice
- **THEN** evaluation, native execution, and direct WebAssembly produce the same sorted fingerprint

#### Scenario: Preserve monomorphic recursive identity

- **WHEN** a generic recursive function calls itself with its current concrete type arguments
- **THEN** one monomorphic instance is reused while each runtime invocation receives a distinct activation frame
