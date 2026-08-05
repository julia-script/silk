## MODIFIED Requirements

### Requirement: MIR is a backend-neutral CFG over logical types

A MIR module SHALL represent each function as a basic-block control-flow graph over logical Silk
types with typed locals: explicit operations (integer literals, moves, canonical-target calls,
drops) and explicit terminators (return, jump, conditional branch, trap), with cleanup paths as
marked cleanup blocks. MIR SHALL carry a compiler-selected target and layout table, but MUST NOT
contain LLVM types, instructions, intrinsics, attributes, metadata nodes, WebAssembly value types,
or backend-owned physical representations. Physical facts in the canonical layout table belong to
Silk rather than to any backend. The operation vocabulary is restricted to the frozen slice's needs
while its closed unions leave room for the full pinned vocabulary.

This prohibition constrains MIR's own operation and control vocabulary, not the compiler-owned
layout facts attached to the program. MIR preserves the control structure lowering derived from
the source, and backends MAY rely on the structural guarantees the verifier and lowering establish.
Consuming that shape is each backend's own responsibility, done as its target demands: a backend
targeting an arbitrary-CFG form emits the blocks directly, while one targeting structured control
flow recovers the source's constructs from the same graph. Neither target's control shape belongs
in MIR.

#### Scenario: Model a straight-line function

- **WHEN** a hand-built sample models `main` returning a called constant
- **THEN** its function has one entry block whose operations are a literal and a canonical-target call ending in a return terminator, all over logical `I32`, while the program carries the selected target and `I32` layout

#### Scenario: Model a cleanup path

- **WHEN** a hand-built sample routes an exit through a cleanup block
- **THEN** the cleanup block is explicitly marked, contains ordered drops, and ends in a jump without introducing a backend-specific representation

### Requirement: MIR encodes deterministically

MIR SHALL expose a deterministic textual encoder covering the selected target, complete ordered
layout table, functions, blocks, operations, terminators, types, and provenance including generated
markers. Identical target-aware modules SHALL encode byte-identically across fresh processes, gated
by committed golden files over the hand-built samples.

#### Scenario: Match the MIR golden encodings

- **WHEN** the hand-built samples are encoded
- **THEN** each encoding includes the canonical target and layout table and equals its committed golden text byte-for-byte

#### Scenario: Repeat encoding

- **WHEN** the same target-aware sample is constructed and encoded repeatedly in fresh processes
- **THEN** the encoded texts are byte-identical

## ADDED Requirements

### Requirement: MIR carries the completed compiler layout plan

Every lowered MIR program SHALL carry exactly one complete layout plan containing its canonical
target and the entries computed for its discovered runtime instances. The verifier SHALL reject a
program whose function types or operations reference a runtime type missing from the plan or whose
scalar facts conflict with the plan's target profile. MIR MUST NOT duplicate the target outside the
plan.

#### Scenario: Lower with the completed plan

- **WHEN** discovered instances using `I32` and `Bool` lower successfully
- **THEN** the resulting MIR program carries the selected target and verified entries for both types

#### Scenario: Reject a missing type layout

- **WHEN** a hand-built MIR program uses `Bool` but omits its layout entry
- **THEN** verification reports the missing layout deterministically as data

## REMOVED Requirements

### Requirement: Target layout is a separate emission-time input

**Reason**: The compiler now owns target selection and layout before MIR lowering, so keeping a
second emission-time input would permit consumers to disagree about the program's representation.

**Migration**: Construct MIR with the canonical target and completed layout table, then pass only
that target-aware MIR program and the codegen request to backend emission.
