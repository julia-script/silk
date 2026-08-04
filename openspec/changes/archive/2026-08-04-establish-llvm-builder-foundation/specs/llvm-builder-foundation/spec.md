## Purpose

Provide an Effect-native, deterministic foundation for constructing LLVM modules and emitting minimal interoperable textual IR and bitcode without production LLVM or Zig dependencies.

## ADDED Requirements

### Requirement: Effect-native builder lifecycle
The system SHALL create opaque builder sessions through Effect and SHALL express every public state mutation, validation failure, and finalization operation in the Effect error channel. Recoverable public failures MUST use `SilkError` and MUST NOT throw ordinary exceptions or expose `unknown` as the public error type.

#### Scenario: Create and use a builder
- **WHEN** a caller creates a builder with valid options and performs valid operations
- **THEN** the operations succeed as Effects and return values owned by that builder

#### Scenario: Reject invalid builder input
- **WHEN** a caller supplies an invalid option or operation input
- **THEN** the operation fails with `SilkError` in the Effect error channel

### Requirement: Module-owned values
The system SHALL associate every public handle with its owning builder or function body and SHALL reject use of a handle in a different owner before mutating state.

#### Scenario: Cross-builder handle use
- **WHEN** a handle created by one builder is passed to an operation on another builder
- **THEN** the operation fails with `SilkError` and neither builder is modified

### Requirement: Byte-exact names and wide values
The system SHALL preserve arbitrary byte sequences used as LLVM names and strings, SHALL offer UTF-8 string convenience inputs, and SHALL represent integer values outside JavaScript's safe-integer range without loss.

#### Scenario: Preserve a non-UTF-8 name
- **WHEN** a caller supplies a byte name containing values that are not valid UTF-8
- **THEN** text escaping and bitcode output preserve the original bytes exactly

#### Scenario: Preserve a wide integer
- **WHEN** a caller supplies a supported integer larger than `Number.MAX_SAFE_INTEGER`
- **THEN** all subsequent queries and serialized output preserve its exact value

### Requirement: Safe concurrent mutation
The system SHALL serialize mutations to a shared builder so concurrent fibers cannot corrupt interning tables, allocate duplicate indices, or partially apply an operation. Output SHALL be deterministic for the same successfully committed operation order.

#### Scenario: Concurrent independent mutations
- **WHEN** two fibers perform valid independent mutations against one builder
- **THEN** both mutations are committed exactly once and all resulting handles remain valid

### Requirement: Minimal textual LLVM module
The system SHALL render a newly created builder as syntactically valid textual LLVM IR containing its configured source filename, target triple, data layout, and module assembly when those values are present.

#### Scenario: Render an empty module
- **WHEN** a caller renders a builder with no declarations
- **THEN** the result is accepted by the supported `llvm-as` validation toolchain

### Requirement: Minimal LLVM bitcode module
The system SHALL encode a newly created builder as a `Uint8Array` containing an LLVM bitstream with the LLVM IR magic value, valid block structure, little-endian words, and configured module records.

#### Scenario: Encode an empty module
- **WHEN** a caller encodes a builder with no declarations
- **THEN** the result is accepted by the supported `llvm-dis` and `llvm-bcanalyzer` validation toolchain

#### Scenario: Repeat an encoding
- **WHEN** two equivalent builders receive the same operations in the same order
- **THEN** their textual IR and bitcode bytes are identical

### Requirement: Runtime independence
The published package SHALL require only `effect` at runtime and SHALL return data rather than performing filesystem writes or invoking Zig or LLVM tools.

#### Scenario: Use the packed package without compiler tools
- **WHEN** a consumer imports and runs the package in a supported JavaScript runtime without Zig or LLVM installed
- **THEN** builder creation, textual rendering, and bitcode encoding remain available
