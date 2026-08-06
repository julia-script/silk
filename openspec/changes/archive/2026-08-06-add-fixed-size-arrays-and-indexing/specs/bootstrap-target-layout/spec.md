## ADDED Requirements

### Requirement: Fixed arrays have compiler-owned repeated-element layout

The target-aware layout phase SHALL compute an array's element stride, total size, alignment, and
index offsets from the selected element layout and canonical length before MIR lowering. Total size
or offset overflow SHALL make the array layout explicitly unavailable. A zero-length array SHALL
have size zero while retaining the element alignment and canonical type.

#### Scenario: Lay out a padded struct array

- **WHEN** `Array<Pair, 3>` uses a selected `Pair` size and alignment
- **THEN** the array layout records three equal element strides and a checked total size derived once by the compiler

### Requirement: Array calling paths use canonical element selectors

Compiler-owned calling shapes SHALL recursively flatten Copy scalar leaves in ascending array-index
order. Each lane path SHALL distinguish canonical field selectors from array-element selectors, so
nested arrays and structs have one unambiguous deterministic path vocabulary. Backends MUST NOT
derive or reorder these paths.

#### Scenario: Flatten an array of structs

- **WHEN** `Array<Pair, 2>` is reachable and `Pair` has two scalar fields
- **THEN** its calling shape contains index-zero fields in declaration order followed by index-one fields in declaration order

#### Scenario: Preserve zero lanes

- **WHEN** `Array<I32, 0>` crosses an internal function boundary
- **THEN** its calling shape retains the logical array identity with zero scalar lanes
