# llvm-advanced-ir Specification

## Purpose

Extend function construction to the memory, aggregate, vector, atomic, assembly, intrinsic, and flagged operations required by production LLVM backends.

## Requirements

### Requirement: Memory and address operations
The system SHALL construct alloca, load, store, GEP, structured GEP, and address-space-aware operations with validated pointee types, indices, alignments, access kinds, and result types.

#### Scenario: Compute and load an aggregate field
- **WHEN** a caller computes a valid structured GEP and loads the selected field
- **THEN** the pointer and loaded value have the types implied by the aggregate layout

#### Scenario: Reject an invalid GEP path
- **WHEN** a GEP index path cannot select a child of the declared source type
- **THEN** construction fails with `LlvmError` before an instruction is added

### Requirement: Aggregate and vector operations
The system SHALL construct supported aggregate builds, element extraction/insertion, value extraction/insertion, vector shuffles, and vector splats while preserving fixed and scalable vector semantics.

#### Scenario: Shuffle compatible vectors
- **WHEN** two compatible vectors and a valid mask are supplied
- **THEN** the shuffle result has the expected vector type and exact mask encoding

### Requirement: Atomic operations
The system SHALL construct atomic and volatile loads/stores, fences, compare-exchange, and atomic read-modify-write operations and SHALL validate ordering, failure-ordering, synchronization-scope, alignment, and operation constraints.

#### Scenario: Build compare-exchange
- **WHEN** a caller supplies compatible pointer, comparison, replacement, ordering, and alignment inputs
- **THEN** the operation returns the LLVM compare-exchange result type and preserves every memory-access setting

#### Scenario: Reject an invalid failure ordering
- **WHEN** compare-exchange uses a failure ordering forbidden by LLVM
- **THEN** construction fails with `LlvmError`

### Requirement: Instruction flags and call kinds
The system SHALL preserve supported fast-math, no-wrap, exact, in-bounds, weak, volatile, tail-call, and calling-convention settings and SHALL reject settings that do not apply to the selected operation.

#### Scenario: Encode fast floating-point arithmetic
- **WHEN** a floating-point operation is created with supported fast-math flags
- **THEN** its textual and bitcode forms contain the equivalent flags

### Requirement: Inline assembly and varargs
The system SHALL construct inline-assembly calls and variable-argument access with exact assembly bytes, constraints, dialect, side-effect, alignment-stack, unwind, and type information.

#### Scenario: Build an inline-assembly call
- **WHEN** a caller supplies a valid assembly signature, assembly bytes, constraints, and options
- **THEN** the resulting call preserves those inputs in text and bitcode

### Requirement: Intrinsic resolution
The system SHALL resolve every intrinsic supported by the pinned builder, including overloaded signatures and canonical attributes, and SHALL provide typed conveniences for memory copy, move, and set operations.

#### Scenario: Resolve a memory intrinsic
- **WHEN** a caller requests a supported memory intrinsic for compatible pointer and length types
- **THEN** the system reuses a canonical declaration and emits a correctly typed call

### Requirement: Advanced constant expressions
The system SHALL support the constant casts, arithmetic, GEP, block-address, local-equivalent, no-CFI, and assembly expressions supported by the pinned builder.

#### Scenario: Serialize an in-bounds constant GEP
- **WHEN** a caller builds a valid in-bounds GEP from constant operands
- **THEN** the expression's type, indices, in-range information, text, and bitcode are preserved

### Requirement: Advanced IR serialization
The system SHALL emit every construct in this capability in deterministic textual LLVM IR and LLVM bitcode.

#### Scenario: Validate an advanced module
- **WHEN** a representative module combines memory, vector, atomic, intrinsic, and assembly operations
- **THEN** supported LLVM tools accept both emitted forms and report equivalent IR
