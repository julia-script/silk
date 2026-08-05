# wasm-builder-foundation Delta

## Purpose

Provide an Effect-native, concurrency-safe owner of WebAssembly module state with opaque
owner-checked handles, typed failures, and optional entity names, independent of any external
toolchain at runtime.

## ADDED Requirements

### Requirement: Effect-native builder lifecycle
The system SHALL create opaque builder sessions through Effect and SHALL express every public
state mutation, validation failure, and emission operation in the Effect error channel.
Recoverable public failures MUST use a typed `WasmError` whose reason discriminates rejected
input, invalid state or ownership, and validation failures, and MUST NOT throw ordinary
exceptions or expose `unknown` as the public error type.

#### Scenario: Create and use a builder
- **WHEN** a caller creates a builder and performs valid declarations against it
- **THEN** the operations succeed as Effects and return handles owned by that builder

#### Scenario: Reject invalid input
- **WHEN** a caller supplies an invalid declaration or definition input
- **THEN** the operation fails with `WasmError` in the Effect error channel and the builder state
  is unchanged

### Requirement: Builder-owned handles
The system SHALL associate every public handle (type, function, table, memory, global, element
segment, data segment) with its owning builder and SHALL reject use of a handle against a
different builder before mutating state.

#### Scenario: Cross-builder handle use
- **WHEN** a handle created by one builder is passed to an operation on another builder
- **THEN** the operation fails with `WasmError` and neither builder is modified

### Requirement: Safe concurrent mutation
The system SHALL serialize mutations to a shared builder so concurrent fibers cannot corrupt
interning tables, lose declarations, or partially apply an operation. Output SHALL be
deterministic for the same successfully committed operation order.

#### Scenario: Concurrent independent declarations
- **WHEN** two fibers perform valid independent declarations against one builder
- **THEN** both declarations are committed exactly once and all resulting handles remain valid

### Requirement: Optional entity names
The system SHALL accept an optional UTF-8 name when declaring any function, table, memory,
global, element segment, or data segment, and locals within a function definition, and SHALL
preserve those names for text rendering and the binary `name` custom section. Because names
become text-format identifiers, the system SHALL reject a name already used within the same
index space (or within one definition's locals) when it is declared.

#### Scenario: Named and anonymous entities coexist
- **WHEN** a caller declares one named function and one unnamed function
- **THEN** both are committed, and the recorded name is retrievable only for the named one

#### Scenario: Duplicate name in one index space
- **WHEN** a caller declares two functions with the same name
- **THEN** the second declaration fails with `WasmError` while the same name remains usable in
  other index spaces

### Requirement: Runtime independence
The published package SHALL require only `effect` at runtime and SHALL return data rather than
performing filesystem writes or invoking external WebAssembly tools.

#### Scenario: Use the package without WebAssembly tooling installed
- **WHEN** a consumer imports and runs the package in a supported JavaScript runtime without
  `wasm-tools` or any WebAssembly toolchain installed
- **THEN** builder creation, validation, text rendering, and binary encoding remain available
