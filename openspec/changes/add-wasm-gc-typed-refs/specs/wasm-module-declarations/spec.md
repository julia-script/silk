# wasm-module-declarations Delta

## RENAMED Requirements

- FROM: `### Requirement: Structurally interned function types`
- TO: `### Requirement: Structurally interned types`

## MODIFIED Requirements

### Requirement: Structurally interned types
The system SHALL intern function, struct, and array types — including recursive groups of
mutually referring types — using the specification's iso-recursive canonicalization, so that
structurally equivalent definitions receive the same handle and emit one type-section entry.
Types MAY declare supertypes and finality; a declared supertype MUST structurally match per the
subtype rules, and a non-empty recursive group emits as one group entry.

#### Scenario: Duplicate function type
- **WHEN** a caller creates two function types with identical parameter and result sequences
- **THEN** both calls return the same handle and the emitted type section contains one entry

#### Scenario: Equivalent recursive groups interned
- **WHEN** a caller defines two recursive groups with structurally identical struct/array
  members
- **THEN** the members of both groups resolve to the same handles and one group is emitted

#### Scenario: Invalid supertype rejected
- **WHEN** a caller declares a struct type whose supertype is final or structurally
  incompatible
- **THEN** the declaration fails with `WasmError`

## ADDED Requirements

### Requirement: Typed reference declarations
The system SHALL accept parameterized reference types (`ref` / `ref null` over abstract heap
types or concrete type handles) wherever the baseline accepted `funcref` or `externref`:
locals, globals, table element types, block types, and element segments. Non-defaultable
(non-nullable) types SHALL be rejected where the specification requires a default value.

#### Scenario: Table of concrete function references
- **WHEN** a caller declares a table whose element type is a nullable reference to a concrete
  function type
- **THEN** the declaration is committed and emission encodes the concrete heap type

#### Scenario: Non-defaultable local rejected
- **WHEN** a function declares a local of a non-nullable reference type
- **THEN** definition fails with `WasmError` — a deliberate builder restriction: the
  specification permits such locals with initialization tracking, which the builder does not
  yet implement
