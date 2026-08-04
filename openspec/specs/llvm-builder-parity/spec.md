# llvm-builder-parity Specification

## Purpose

Define the evidence required to claim a complete, deterministic, interoperable Effect port of the supported pinned Zig LLVM builder and bitcode writer behavior.

## Requirements

### Requirement: Pinned upstream parity
The system SHALL maintain a machine-readable manifest mapping every supported construct in the pinned Zig `Builder.zig`, `bitcode_writer.zig`, and `ir.zig` baseline to an implementation location, verification coverage, or documented intentional deviation.

#### Scenario: Audit the parity manifest
- **WHEN** the manifest is checked against the pinned source inventory
- **THEN** every supported upstream construct has exactly one recorded disposition and no implementation claim lacks verification evidence

### Requirement: Documented semantic deviations
The system SHALL document JavaScript-platform differences and behavior the pinned Zig builder itself marks unsupported or incomplete, and SHALL NOT describe those exclusions as implemented parity.

#### Scenario: Encounter an unsupported upstream operation
- **WHEN** a caller requests a construct recorded as intentionally unsupported
- **THEN** the public API either omits the operation or fails with a documented `SilkError` rather than emitting approximate IR

### Requirement: LLVM interoperability
The system SHALL validate representative output with the supported LLVM assembler, disassembler, verifier, and bitcode analyzer and SHALL compare semantic structure after round trips.

#### Scenario: Run the interoperability suite
- **WHEN** the pinned compatibility toolchain runs against the complete fixture corpus
- **THEN** every textual fixture assembles, every bitcode fixture disassembles, verification succeeds, and paired forms describe equivalent modules

### Requirement: Differential Zig evidence
The system SHALL compare port output with checked-in fixtures produced from the pinned Zig implementation for every covered type, declaration, instruction, constant, attribute, metadata record, and bitstream primitive.

#### Scenario: Run differential fixtures
- **WHEN** the package test suite evaluates the pinned Zig fixtures
- **THEN** byte-sensitive cases match exactly and semantic cases match their documented canonical comparison

### Requirement: Deterministic complete output
The system SHALL produce identical text and bytes for equivalent operation sequences across repeated processes on every supported runtime and architecture.

#### Scenario: Rebuild the fixture corpus
- **WHEN** the same corpus is built repeatedly in fresh processes
- **THEN** all textual and binary outputs are byte-for-byte identical

### Requirement: Measured hot-path exceptions
The system SHALL maintain benchmark evidence for any imperative or untraced encoding loop and SHALL remove such exceptions when they no longer provide a meaningful measured benefit.

#### Scenario: Evaluate a hot-path exception
- **WHEN** a release candidate includes an imperative or `Effect.fnUntraced` hot path
- **THEN** the benchmark suite identifies the path, workload, baseline, and measured benefit

### Requirement: Releasable public package
The system SHALL publish every public actor through an explicit root namespace and package subpath export, include usage and compatibility documentation, preserve required upstream notices, and pass the repository release-candidate validation.

#### Scenario: Validate the packed package
- **WHEN** the release-candidate suite packs and imports `@silk-effect/llvm`
- **THEN** all documented root and deep imports resolve as self-contained ESM without source files or undeclared runtime dependencies

### Requirement: Reproducible upstream updates
The system SHALL document how to update the pinned Zig baseline, regenerate the source inventory and fixtures, review semantic changes, and update the parity manifest.

#### Scenario: Detect upstream drift
- **WHEN** maintainers select a different Zig commit as the candidate baseline
- **THEN** the update workflow reports added, removed, and changed upstream constructs before the package parity claim is advanced
