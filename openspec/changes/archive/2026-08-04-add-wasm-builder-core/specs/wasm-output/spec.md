# wasm-output Delta

## Purpose

Emit one committed module state as both canonical `.wat` text and `.wasm` binary bytes, with
emission-time module-level validation guaranteeing that emitted modules are valid WebAssembly.

## ADDED Requirements

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
