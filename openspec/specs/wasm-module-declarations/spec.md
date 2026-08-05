# wasm-module-declarations Specification

## Purpose

Declare every kind of WebAssembly module entity — function types, imports, functions, tables,
memories, globals, exports, the start function, and element and data segments — through handles
whose final indices are computed at emission.

## Requirements

### Requirement: Structurally interned function types
The system SHALL intern function types by their parameter and result value-type sequences so
that structurally equal types receive the same handle and emit one type-section entry.

#### Scenario: Duplicate function type
- **WHEN** a caller creates two function types with identical parameter and result sequences
- **THEN** both calls return the same handle and the emitted type section contains one entry

### Requirement: Declaration-order independence
The system SHALL resolve all index spaces at emission time so that references between entities
remain valid regardless of the order in which imports and definitions were declared.

#### Scenario: Import added after function references
- **WHEN** a function body referencing a defined function is committed and a function import is
  declared afterwards
- **THEN** emission succeeds and the call references the defined function even though imported
  functions occupy the lowest function indices

### Requirement: Imports
The system SHALL declare imported functions, tables, memories, and globals with module and field
names, returning handles usable anywhere a defined entity's handle is accepted. Imported
memories and tables carry the same `shared` and address-type options as defined ones.

#### Scenario: Imported function is callable
- **WHEN** a caller imports a function and references its handle in a call instruction of a
  committed body
- **THEN** emission resolves the call to the imported function's index

#### Scenario: Imported shared memory
- **WHEN** a caller imports a shared memory with a maximum and uses it in atomic operations
- **THEN** the import is committed and bodies validate against it

### Requirement: Definitions
The system SHALL declare defined functions (by function type), tables (element type, limits,
and an optional 64-bit address type), memories (limits, allowing multiple memories, an optional
`shared` flag, and an optional 64-bit address type), and globals (value type, mutability, and a
constant initializer expression). A shared memory MUST declare a maximum size, and limits of a
64-bit entity are validated against 64-bit bounds.

#### Scenario: Two memories
- **WHEN** a caller declares two memories
- **THEN** emission succeeds and produces a memory section with two entries

#### Scenario: Global with extended constant initializer
- **WHEN** a caller declares a global whose initializer uses extended constant expression
  operations over another global
- **THEN** the declaration is committed and emission encodes the initializer expression exactly

#### Scenario: Shared memory requires a maximum
- **WHEN** a caller declares a shared memory without a maximum size
- **THEN** the declaration fails with `WasmError`

#### Scenario: 64-bit memory declared
- **WHEN** a caller declares a 64-bit memory with limits above the 32-bit page bound
- **THEN** the declaration is committed and emission encodes 64-bit limits flags

### Requirement: Exports and start function
The system SHALL export functions, tables, memories, and globals under caller-chosen names,
SHALL reject duplicate export names at or before emission, and SHALL designate at most one
start function whose type is `[] -> []`.

#### Scenario: Duplicate export name
- **WHEN** two exports are declared with the same name
- **THEN** the operation or emission fails with `WasmError` and no invalid module is produced

#### Scenario: Invalid start signature
- **WHEN** a function whose type is not `[] -> []` is designated as the start function
- **THEN** the operation fails with `WasmError`

### Requirement: Element and data segments
The system SHALL declare active, passive, and declarative element segments and active and
passive data segments, validating active-segment offset expressions and target references.

#### Scenario: Passive data segment
- **WHEN** a caller declares a passive data segment with arbitrary bytes
- **THEN** the bytes are preserved exactly in emitted binary output

### Requirement: Tags
The system SHALL declare exception tags referencing an interned function type with an empty
result sequence, SHALL support importing and exporting tags like any other entity, and SHALL
reject a tag whose type has results.

#### Scenario: Tag declared and exported
- **WHEN** a caller declares a tag with type `[i32] -> []` and exports it
- **THEN** emission produces a tag section entry and a tag export

#### Scenario: Tag with results rejected
- **WHEN** a caller declares a tag whose function type has a non-empty result sequence
- **THEN** the declaration fails with `WasmError`

#### Scenario: Imported tag is throwable
- **WHEN** a caller imports a tag and references its handle in a committed `throw`
- **THEN** emission resolves the throw to the imported tag's index, imports first
