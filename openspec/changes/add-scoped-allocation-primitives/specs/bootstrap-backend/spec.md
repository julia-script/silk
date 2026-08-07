## ADDED Requirements

### Requirement: Native and Wasm realize the same allocation contract

Native LLVM and direct WebAssembly lowering SHALL realize the compiler-planned size, alignment,
address width, typed-slot stride, typed allocation failure, slot operations, drop hooks, and exactly-once cleanup
contract. Each backend MAY choose target machinery for acquisition and release, but MUST NOT change
logical ownership, synthesize an allocator, zero storage implicitly, expose reclaim tickets, or
reorder cleanup.

#### Scenario: Execute equivalent successful allocation

- **WHEN** one accepted fixture allocates, initializes, reads, and drops typed storage on native and Wasm targets
- **THEN** both executions produce the evaluator's result, drop-hook-before-release order, and absence of live allocation records

#### Scenario: Execute equivalent exhaustion

- **WHEN** deterministic providers reject the same allocation ordinal on native and Wasm targets
- **THEN** both executions surface `OutOfMemory` and perform the same prior-resource cleanup without exposing a backend trap as the typed failure

### Requirement: Backend storage preserves alignment and zero-size identity

Native storage SHALL satisfy the selected target alignment and WebAssembly storage SHALL use
checked aligned linear-memory positions. Both backends SHALL preserve logical identities for
zero-sized allocations and MUST NOT infer ownership or aliasing from address inequality.

#### Scenario: Access a padded aggregate slot

- **WHEN** a typed allocation stores multiple values whose nominal layout has padding
- **THEN** each backend accesses every slot at the compiler-planned aligned stride and returns the same values
