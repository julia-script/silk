## ADDED Requirements

### Requirement: Concrete floating primitives use the Intrinsic namespace

Every concrete `f32` and `f64` arithmetic, comparison, classification, total-order, bit-conversion,
transcendental, and numeric-conversion primitive SHALL be a type-specific member of `Intrinsic`.
Source-defined floating actor modules and numeric interfaces SHALL wrap those primitives without
changing their deterministic semantics or engine parity.

#### Scenario: Specialize generic floating addition

- **WHEN** a generic numeric addition is instantiated with `f64`
- **THEN** its canonical conformance selects the concrete `f64` intrinsic with no runtime type dispatch

#### Scenario: Preserve bit conversion

- **WHEN** a source wrapper converts an `f32` to and from its bit representation
- **THEN** evaluation, native LLVM, and direct WebAssembly preserve the same bits through the concrete intrinsics
