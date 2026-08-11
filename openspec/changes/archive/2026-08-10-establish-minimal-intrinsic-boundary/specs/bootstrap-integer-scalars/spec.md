## ADDED Requirements

### Requirement: Concrete integer primitives use the Intrinsic namespace

Each admitted integer type and concrete primitive operation SHALL have one unambiguous
type-specific member of `Intrinsic`. Its name and contract SHALL identify the concrete input and
result types without overload resolution or runtime type inspection. Existing arithmetic,
comparison, bitwise, shift, rotate, checked, wrapping, saturating, negate, and conversion semantics
MUST remain unchanged.

#### Scenario: Call one concrete primitive

- **WHEN** source calls `Intrinsic.i32Add` with two `i32` values
- **THEN** it produces the same `i32` value or overflow trap on every execution engine

#### Scenario: Reject a mismatched concrete type

- **WHEN** `Intrinsic.i32Add` receives a value of another integer type
- **THEN** analysis rejects the call without conversion or generic inference

### Requirement: Generic integer APIs are source-defined

The standard library SHALL define ordinary numeric interfaces and actor-module functions that can
operate over supported integer types. Primitive integer types SHALL have canonical standard-library
conformances mapping interface operations to their concrete intrinsics. Generic calls SHALL
monomorphize to the selected concrete operation without a runtime union, type tag, service slot, or
numeric registry.

#### Scenario: Specialize generic addition

- **WHEN** a generic `add<T: Integer>` call is instantiated with `i32`
- **THEN** specialization selects the canonical `i32` conformance and lowers to `Intrinsic.i32Add`

#### Scenario: Reject a non-integer type

- **WHEN** a type without the Integer conformance is passed to the generic addition API
- **THEN** conformance analysis rejects the call before MIR lowering
