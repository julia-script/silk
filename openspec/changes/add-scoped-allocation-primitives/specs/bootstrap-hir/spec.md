## ADDED Requirements

### Requirement: HIR retains explicit allocation and initialization semantics

HIR SHALL retain established named scope wrapper identities, validated `Layout` and `SlotLayout<T>`
formation, allocator role and destination scope, allocation success and `OutOfMemory`, affine
resource identity, unsafe lexical slot selection and access, restricted drop-hook calls, explicit
drop, and cleanup provenance in source evaluation order. HIR MUST NOT contain LLVM types,
WebAssembly offsets, allocator implementation objects, backend instructions, or forgeable reclaim
tickets.

#### Scenario: Inspect a typed slot allocation

- **WHEN** a valid source function allocates storage and initializes one typed slot
- **THEN** HIR exposes the ordered scope, layout, allocation, slot, and initialization operations with their canonical types and source spans

#### Scenario: Keep reclaim identity private

- **WHEN** HIR for an allocation is encoded or inspected
- **THEN** it identifies the logical allocation and origin relationship without exposing a source-constructible reclaim token
