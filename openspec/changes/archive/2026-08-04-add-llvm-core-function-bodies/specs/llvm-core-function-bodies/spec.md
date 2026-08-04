## Purpose

Provide safe construction of executable LLVM SSA function bodies, including core instructions and control flow, with equivalent textual IR and bitcode output.

## ADDED Requirements

### Requirement: Atomic function-body lifecycle
The system SHALL build each function body in an isolated draft, SHALL commit it only after the build action succeeds and final validation passes, and SHALL make the draft unusable after completion or failure.

#### Scenario: Commit a valid body
- **WHEN** a body action succeeds with a structurally valid function body
- **THEN** the complete body becomes visible on the function declaration exactly once

#### Scenario: Roll back a failed body
- **WHEN** a body action fails or final validation rejects the body
- **THEN** no partial blocks or instructions are committed to the function declaration

### Requirement: Function-local ownership
The system SHALL associate arguments, blocks, instructions, and local values with their function-body owner and SHALL reject values from another function body.

#### Scenario: Use a foreign local value
- **WHEN** an instruction operand belongs to a different function body
- **THEN** instruction creation fails with `SilkError` without changing either body

### Requirement: Core SSA instructions
The system SHALL construct the pinned builder's core unary, arithmetic, cast, comparison, select, extract-value, and insert-value instructions with validated result and operand types.

#### Scenario: Build a typed arithmetic chain
- **WHEN** a caller combines compatible arguments and constants with core arithmetic, cast, and comparison operations
- **THEN** each result has the LLVM type implied by the operation and can be used by subsequent instructions

#### Scenario: Reject incompatible operands
- **WHEN** a caller supplies operand types that violate an instruction's LLVM constraints
- **THEN** the instruction fails with `SilkError` before allocating a result value

### Requirement: Structured control flow
The system SHALL construct basic blocks and the supported branch, conditional branch, switch, return, and unreachable terminators, and SHALL require every committed block to end in exactly one terminator.

#### Scenario: Build conditional control flow
- **WHEN** a body branches on an `i1` value to two blocks that both terminate
- **THEN** the body commits with stable block and value numbering

#### Scenario: Reject an unterminated block
- **WHEN** final validation finds a block without a terminator
- **THEN** body construction fails with `SilkError` and the draft is not committed

### Requirement: Phi nodes
The system SHALL support normal and fast-math phi nodes, including forward value references, and SHALL validate that each incoming value corresponds to a declared predecessor and has the phi result type.

#### Scenario: Merge branch values
- **WHEN** a phi node receives compatible values from each predecessor block
- **THEN** it produces a value of the declared phi type and serializes with the correct incoming pairs

### Requirement: Calls and returns
The system SHALL construct direct and indirect calls with supported tail kinds, calling conventions, attributes, and operand bundles, and SHALL validate arguments and returns against function signatures.

#### Scenario: Call a declared function
- **WHEN** a caller supplies arguments matching a declared function type
- **THEN** the call has the declared return type and serializes with the selected call settings

#### Scenario: Reject an invalid return
- **WHEN** a return value does not match the enclosing function return type
- **THEN** return construction fails with `SilkError`

### Requirement: Core body serialization
The system SHALL emit every core instruction and control-flow construct supported by this capability in textual LLVM IR and LLVM bitcode with deterministic relative value encoding.

#### Scenario: Round-trip a control-flow graph
- **WHEN** a representative multi-block function is rendered and encoded
- **THEN** LLVM accepts both forms and decodes them to equivalent control flow, instructions, and types
