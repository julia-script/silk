# llvm-debug-metadata Specification

## Purpose

Allow LLVM modules to carry deterministic metadata and source-level debug information, including references and attachments, while supporting complete stripping.

## Requirements

### Requirement: Metadata identities
The system SHALL support metadata strings, tuples, constants, named metadata, uniqued nodes, distinct nodes, local nodes, optional references, and builder-owned metadata identities.

#### Scenario: Reuse a uniqued metadata node
- **WHEN** equivalent uniqued metadata is created twice in one builder
- **THEN** both operations return the same metadata identity

#### Scenario: Preserve distinct metadata nodes
- **WHEN** two equivalent nodes are explicitly created as distinct
- **THEN** they retain separate identities and separate serialized entries

### Requirement: Metadata forward references
The system SHALL create and resolve supported metadata forward references and SHALL reject unresolved, multiply resolved, cross-builder, or invalid-kind references before output is produced.

#### Scenario: Resolve a recursive type graph
- **WHEN** a forward reference is resolved exactly once to a compatible metadata node
- **THEN** all users serialize using the resolved metadata identity

#### Scenario: Encode with an unresolved reference
- **WHEN** output is requested while a reachable metadata forward reference remains unresolved
- **THEN** output fails with `LlvmError` rather than emitting malformed metadata

### Requirement: Debug information model
The system SHALL construct the debug files, compile units, subprograms, lexical blocks, locations, basic types, composite types, derived types, subroutine types, enumerators, subranges, expressions, local variables, parameters, and global variables supported by the pinned builder.

#### Scenario: Describe a compiled function
- **WHEN** a caller creates a compile unit, subprogram, lexical scope, source locations, and local variables
- **THEN** the resulting debug graph retains all specified files, lines, columns, scopes, flags, and type references

### Requirement: Metadata attachments
The system SHALL attach supported metadata kinds to globals and instructions and SHALL preserve branch weights, unpredictability markers, debug locations, subprograms, and global-variable expressions.

#### Scenario: Attach changing debug locations
- **WHEN** sequential instructions use repeated and changed debug locations
- **THEN** bitcode uses semantically equivalent location records without changing instruction numbering

### Requirement: Metadata stripping
The system SHALL offer a builder mode that omits debug names, debug nodes, debug locations, and debug attachments while retaining executable module semantics.

#### Scenario: Compare stripped and debug-preserving output
- **WHEN** equivalent modules are built in stripped and debug-preserving modes
- **THEN** their executable declarations and instructions are equivalent and only the preserving output contains debug information

### Requirement: Metadata serialization
The system SHALL emit reachable metadata, metadata kinds, named metadata, and attachments in deterministic textual LLVM IR and LLVM bitcode.

#### Scenario: Validate debug output
- **WHEN** a representative module with recursive debug types and instruction attachments is rendered and encoded
- **THEN** supported LLVM tools accept both forms and expose equivalent metadata graphs
