## ADDED Requirements

### Requirement: Native foreign calls preserve C-layout record storage

Native lowering SHALL represent a pointer to a C-layout record as the existing target pointer lane and SHALL materialize the pointee with the compiler-selected C aggregate layout. A foreign call through a mutable pointer SHALL invalidate or reload affected Silk places so native field writes are observable without an adapter, shadow record, or generated C runtime shim.

#### Scenario: Read a record filled by C

- **WHEN** a native Silk program passes a mutable pointer to a C-layout record to a linked C function that writes distinct field values
- **THEN** the following Silk field reads observe those values from the same storage

#### Scenario: Call the system clock

- **WHEN** a supported native program calls `clock_gettime` with a pointer to a C-layout `Timespec`
- **THEN** the executable links through the system C runtime and reads a valid result and populated fields
