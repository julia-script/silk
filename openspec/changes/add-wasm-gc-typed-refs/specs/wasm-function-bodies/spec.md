# wasm-function-bodies Delta

## ADDED Requirements

### Requirement: GC instruction coverage
The system SHALL provide constructors, encoding, text rendering, and validation for the GC
instruction set: struct allocation and field access (including packed `_s`/`_u` accessors and
default allocation only for defaultable fields), array allocation (fixed, default, and from
data or element segments), array access and bulk operations, `ref.eq`, `ref.i31` with
`i31.get_s`/`i31.get_u`, and `any.convert_extern`/`extern.convert_any`.

#### Scenario: Struct field round-trip
- **WHEN** a body allocates a struct, sets a mutable field, and reads it back
- **THEN** definition succeeds and both emitted representations agree

#### Scenario: Packed field access requires signedness
- **WHEN** a body reads an `i8` field with plain `struct.get`
- **THEN** definition fails with `WasmError` because packed fields require `_s` or `_u`

#### Scenario: Immutable field write rejected
- **WHEN** a body applies `struct.set` to an immutable field
- **THEN** definition fails with `WasmError`

### Requirement: Casts and typed calls
The system SHALL validate `ref.test` and `ref.cast` against the operand's hierarchy,
`br_on_cast`/`br_on_cast_fail` against both the cast relationship and the target label types,
`br_on_null`/`br_on_non_null`/`ref.as_non_null` with correct null-tracking, and
`call_ref`/`return_call_ref` against a concrete function type popped as a typed reference.

#### Scenario: call_ref through a typed reference
- **WHEN** a body obtains a `(ref $f)` via `ref.func` and applies `call_ref $f`
- **THEN** definition succeeds with the callee's parameters and results

#### Scenario: Cross-hierarchy cast rejected
- **WHEN** a body casts an `externref` operand to a struct heap type
- **THEN** definition fails with `WasmError` because the types share no hierarchy

### Requirement: Subtype-aware validation
The system SHALL replace exact type equality in stack typing with the specification's subtype
judgment: a value of a subtype SHALL be accepted wherever a supertype is expected, across
abstract heap-type hierarchies, declared supertypes, and nullability.

#### Scenario: Concrete reference widens to funcref
- **WHEN** a body places a `(ref $f)` from `ref.func` where `funcref` is expected
- **THEN** definition succeeds

#### Scenario: Supertype where subtype is required
- **WHEN** a body supplies an `anyref` where a struct reference is expected
- **THEN** definition fails with `WasmError`
