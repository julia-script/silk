# bootstrap-mir Specification

## Purpose
MIR: the monomorphic, backend-neutral basic-block control-flow graph over logical Silk types that
every backend and the interpreter consume — its data model, structural invariants and verifier,
the explicit target-layout input, and the deterministic textual encoder, stabilized against
hand-built samples before lowering exists.
## Requirements
### Requirement: MIR is a backend-neutral CFG over logical types

A MIR module SHALL represent each function as a basic-block control-flow graph over logical Silk
types with typed locals: explicit operations (integer literals, moves, canonical-target calls,
drops) and explicit terminators (return, jump, conditional branch, trap), with cleanup paths as
marked cleanup blocks. MIR MUST NOT contain LLVM types, instructions, intrinsics, attributes,
metadata nodes, or physical field offsets, and MUST NOT adopt WebAssembly stack or structured
control shapes. The operation vocabulary is restricted to the frozen slice's needs while its
closed unions leave room for the full pinned vocabulary.

#### Scenario: Model a straight-line function

- **WHEN** a hand-built sample models `main` returning a called constant
- **THEN** its function has one entry block whose operations are a literal and a canonical-target call ending in a return terminator, all over logical `I32`

#### Scenario: Model a cleanup path

- **WHEN** a hand-built sample routes an exit through a cleanup block
- **THEN** the cleanup block is explicitly marked, contains ordered drops, and ends in a jump — with no target-specific representation anywhere

### Requirement: Target layout is a separate emission-time input

The target-layout input SHALL be defined alongside MIR — target triple, pointer width,
endianness, and logical-type size and alignment rules — and SHALL NOT be part of the MIR module
itself; it is consumed only at emission time.

#### Scenario: Keep MIR layout-free

- **WHEN** a MIR module is constructed and encoded
- **THEN** neither the module nor its encoding depends on any target-layout value, while a layout value can be constructed independently for emission

### Requirement: Every operation carries provenance

Every MIR operation and terminator SHALL carry a source span, and compiler-generated operations
SHALL inherit the nearest causative span and be explicitly marked generated.

#### Scenario: Mark generated cleanup

- **WHEN** a sample models compiler-generated drops in a cleanup block
- **THEN** each drop carries its causative span and an explicit generated marker, while programmer-written operations are unmarked

### Requirement: The verifier reports structural violations as data

A MIR verifier SHALL check structural invariants — a present entry block, terminator targets
naming existing blocks, and every referenced local being declared — and SHALL return an ordered,
deterministic collection of violations rather than throwing. Valid samples SHALL verify clean.

#### Scenario: Verify the samples clean

- **WHEN** the hand-built sample modules are verified
- **THEN** the verifier returns no violations

#### Scenario: Report a broken graph deterministically

- **WHEN** a module references a missing block and an undeclared local
- **THEN** the verifier returns both violations in deterministic order with their function and block identities

### Requirement: MIR encodes deterministically

MIR SHALL expose a deterministic textual encoder covering functions, blocks, operations,
terminators, types, and provenance including generated markers. Identical modules SHALL encode
byte-identically across fresh processes, gated by committed golden files over the hand-built
samples.

#### Scenario: Match the MIR golden encodings

- **WHEN** the hand-built samples are encoded
- **THEN** each encoding equals its committed golden text byte-for-byte

#### Scenario: Repeat encoding

- **WHEN** the same sample is constructed and encoded repeatedly in fresh processes
- **THEN** the encoded texts are byte-identical

### Requirement: Lowering constructs MIR from elaborated instances

Lowering SHALL construct one MIR program module from the discovered instances in discovery
order: each instance's HIR body linearized into basic blocks in evaluation order (arguments
before their call), concrete drops and cleanup edges inserted exactly as the ownership phase's
cleanup plan directs (none in the frozen slice, where every exit releases nothing), and source
provenance attached to every lowered operation. An instance whose HIR body is unavailable SHALL
lower to an explicit generated trap rather than a fabricated body. Lowered programs SHALL verify
clean against the MIR structural verifier and SHALL encode deterministically, gated by committed
golden files.

#### Scenario: Lower a nested call program

- **WHEN** `main` returning `identity(identity(42))` is lowered
- **THEN** the program contains `main` and `identity` functions whose blocks evaluate arguments before calls, reference canonical targets, and end in returns, verifying clean

#### Scenario: Lower an unavailable body to a trap

- **WHEN** a discovered instance's HIR body is unavailable
- **THEN** its lowered function is a single block ending in a generated trap carrying the causative span

#### Scenario: Match the lowered golden encoding

- **WHEN** a committed fixture program is lowered and encoded
- **THEN** the encoding equals the committed golden text byte-for-byte and repeated fresh runs are byte-identical

