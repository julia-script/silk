## ADDED Requirements

### Requirement: C-layout records follow the selected target C aggregate ABI

For every valid C-layout record, the nominal layout catalog SHALL retain declaration-order fields, field offsets, size, alignment, internal padding, and tail padding matching the equivalent C record on the selected supported native target. Fixed-width scalars SHALL keep their named representation, pointer-sized scalars and raw pointers SHALL use target pointer width and alignment, fixed arrays SHALL use the element stride, and nested C-layout records SHALL use their cataloged aggregate layout. Layout planning SHALL reuse the existing aligned aggregate placement authority rather than create a backend-specific second decision.

#### Scenario: Lay out a mixed C record

- **WHEN** a C-layout record contains an `i8`, `i64`, raw pointer, fixed array, and nested C-layout record
- **THEN** its offsets, size, alignment, internal padding, and tail padding equal the equivalent record reported by the host C compiler

#### Scenario: Keep C layout backend-neutral

- **WHEN** native and WebAssembly consumers inspect one valid C-layout record's compiler layout fact for the same target
- **THEN** both receive the identical backend-neutral layout entry selected before lowering
