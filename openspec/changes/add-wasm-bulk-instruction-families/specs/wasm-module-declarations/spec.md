# wasm-module-declarations Delta

## MODIFIED Requirements

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
