# bootstrap-owned-bytes Specification

## Purpose

Define the canonical owned, encoding-neutral byte sequence used by portable source APIs without
granting collection policy or text semantics to the compiler.

## Requirements

### Requirement: Bytes is an ordinary owned nominal value

Canonical Silk source SHALL define nominal `Bytes` as an owned wrapper over `Vector<u8>`. `Bytes`
SHALL be move-only, SHALL recursively release its vector storage through ordinary Drop behavior,
and MUST NOT receive compiler-known layout, collection, encoding, or cleanup treatment.

#### Scenario: Move owned bytes

- **WHEN** one `Bytes` value is moved to another binding
- **THEN** the source becomes unavailable and the destination owns the same initialized byte sequence

#### Scenario: Drop bytes exactly once

- **WHEN** a live `Bytes` value reaches a structured exit
- **THEN** its vector storage is released exactly once through ordinary source-defined cleanup

### Requirement: Bytes exposes a minimal sequence API

`Bytes` SHALL provide empty construction, copying from `&[u8]`, `length`, append from `&[u8]`,
`asSlice`, and `asMutSlice`. Empty construction, length, and borrowed views SHALL be infallible.
Copying and growth SHALL report `OutOfMemoryError` and require `&mut Allocator` only when allocation may
occur. The API MUST NOT claim that its contents are UTF-8 or another text encoding.

#### Scenario: Copy arbitrary octets

- **WHEN** source constructs `Bytes` from a slice containing any `u8` values
- **THEN** the result owns the exact ordered octets independent of text validity

#### Scenario: Append bytes with explicit allocation effects

- **WHEN** appending a slice requires storage growth
- **THEN** the operation uses the provided allocator and either preserves the ordered result or reports `OutOfMemoryError`

#### Scenario: Borrow bytes without copying

- **WHEN** source calls `asSlice` or `asMutSlice` on a live `Bytes` owner
- **THEN** it receives a returned lexical view of the initialized sequence without allocation

### Requirement: Bytes behavior is target-neutral and observable

Equivalent accepted `Bytes` programs SHALL produce the same length, byte order, mutations,
allocation failures, ownership diagnostics, and cleanup behavior in MIR structure, native LLVM
execution, and LLVM-generated WebAssembly execution. Tooling SHALL resolve the nominal type and its operations to
canonical Silk source.

#### Scenario: Compare byte behavior across engines

- **WHEN** one program copies, appends, mutates through a borrowed view, and reads a byte sequence
- **THEN** native and LLVM-generated WebAssembly execution return the same bytes and length

#### Scenario: Navigate Bytes source

- **WHEN** an editor requests definition or hover information for `Bytes.asSlice`
- **THEN** tooling reports the canonical source declaration rather than a compiler-synthesized collection operation
