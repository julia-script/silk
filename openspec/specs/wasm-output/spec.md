# wasm-output Specification

## Purpose

Emit one committed module state as both canonical `.wat` text and `.wasm` binary bytes, with
emission-time module-level validation guaranteeing that emitted modules are valid WebAssembly.
## Requirements
### Requirement: Binary encoding
The system SHALL encode committed module state as a `Uint8Array` containing a well-formed
WebAssembly binary module: magic and version header, sections in canonical order with correct
LEB128 sizes, and index spaces resolved from handles.

#### Scenario: Encoded module instantiates
- **WHEN** a caller encodes a valid committed module
- **THEN** the bytes are accepted by the pinned validation oracle and by a WebAssembly runtime

### Requirement: Text rendering
The system SHALL render committed module state as WebAssembly text format, using declared
entity and local names as `$`-identifiers where present and indices otherwise, such that the
text parses to a module equivalent to the binary encoding.

#### Scenario: Text and binary agree
- **WHEN** a committed module is both rendered as text and encoded as binary
- **THEN** parsing the text with the pinned oracle produces a module byte-identical to the
  binary encoding

### Requirement: Emit-time module validation
The system SHALL validate module-level constraints at emission — including export-name
uniqueness, active-segment offset typing against their target, start-function signature, and
limits well-formedness — and SHALL fail with `WasmError` rather than emit an invalid module.

#### Scenario: Invalid module never emitted
- **WHEN** committed state violates a module-level constraint detectable only at emission
- **THEN** both emitters fail with `WasmError` and produce no output

### Requirement: Name custom section
The system SHALL encode declared module, function, and local names into the binary `name`
custom section, and omit the section entirely when no names were declared.

#### Scenario: Names survive the binary
- **WHEN** a module with named functions is encoded and inspected with the pinned oracle
- **THEN** the `name` section reports the declared function names

### Requirement: Deterministic output
The system SHALL produce byte-identical binary output and character-identical text output for
the same successfully committed operation order, across repeated emissions and process runs.

#### Scenario: Repeat an emission
- **WHEN** two equivalent builders receive the same operations in the same order
- **THEN** their text renderings and binary encodings are identical

### Requirement: Extended-feature encodings
The system SHALL encode and render the SIMD (`0xFD` prefix), atomic (`0xFE` prefix), shared
and 64-bit limits, and 64-bit memarg forms exactly as the binary and text formats specify, and
these forms SHALL satisfy the same determinism, oracle-validation, and text-to-binary
round-trip guarantees as the baseline output.

#### Scenario: Shared memory limits flags
- **WHEN** a module with a shared memory is encoded
- **THEN** the memory's limits carry the shared flag and the binary passes oracle validation

#### Scenario: Extended forms round-trip
- **WHEN** a module using SIMD constants, atomic operations, and a 64-bit memory is rendered as
  text and assembled by the pinned oracle
- **THEN** the resulting bytes equal the builder's binary encoding

### Requirement: Exception and branch-hint encodings
The system SHALL encode the tag section and tag import/export descriptors, `try_table` with its
catch-clause list, `throw`/`throw_ref`, the `exnref` type, and tag names in the `name` custom
section; and SHALL emit the `metadata.code.branch_hint` custom section, immediately before the
code section, carrying each hinted instruction's function index and byte offset. All new forms
SHALL satisfy the same determinism, oracle-validation, and text-to-binary round-trip guarantees
as the baseline output.

#### Scenario: Exception forms round-trip
- **WHEN** a module using tags, `try_table`, and `throw` is rendered as text and assembled by
  the pinned oracle
- **THEN** the resulting bytes equal the builder's binary encoding

#### Scenario: Branch hints round-trip
- **WHEN** a module with hinted branches is rendered as text with hint annotations and
  assembled by the pinned oracle
- **THEN** the resulting bytes equal the builder's binary encoding, including the hint custom
  section

#### Scenario: No hints, no section
- **WHEN** a module contains no hinted instructions
- **THEN** no `metadata.code.branch_hint` section is emitted

### Requirement: GC type and reference encodings
The system SHALL encode recursive type groups, sub types with supertype lists and finality,
struct and array composite types with packed storage types and per-field mutability, and
parameterized reference types (abstract and concrete heap types) exactly as the binary format
specifies, and SHALL render the corresponding text forms (`(rec …)`, `(sub …)`,
`(struct (field …))`, `(array …)`, `(ref null? <heaptype>)`). All new forms SHALL satisfy the
same determinism, oracle-validation, and text-to-binary round-trip guarantees as the baseline
output.

#### Scenario: Recursive group round-trips
- **WHEN** a module defining a recursive group of mutually referring struct types is rendered
  as text and assembled by the pinned oracle
- **THEN** the resulting bytes equal the builder's binary encoding

#### Scenario: Shorthand references stay canonical
- **WHEN** a module uses only baseline `funcref`/`externref`/`exnref` declarations
- **THEN** its emitted bytes are identical to the pre-GC encoding of the same module

