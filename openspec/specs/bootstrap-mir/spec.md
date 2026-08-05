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

This prohibition constrains MIR's own vocabulary — no target's instruction, type, or control
constructs appear in the data model — and does NOT mean the graph is shapeless. MIR preserves
the control structure lowering derived from the source, and backends MAY rely on the structural
guarantees the verifier and lowering establish. Consuming that shape is each backend's own
responsibility, done as its target demands: a backend targeting an arbitrary-CFG form emits the
blocks directly, while one targeting structured control flow recovers the source's constructs
from the same graph. Neither target's control shape belongs in MIR.

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
before their call, statements in source order), each `let` binding materialized as one typed
local, and concrete drops inserted exactly as the ownership phase's cleanup plan directs —
one generated `Drop` per release, in the plan's release order, before the exit's terminator.
Source provenance SHALL be attached to every lowered operation. An instance whose HIR body is
unavailable, or whose ownership verdict is a violation, SHALL lower to an explicit generated
trap rather than a fabricated body. Lowered programs SHALL verify clean against the MIR
structural verifier and SHALL encode deterministically, gated by committed golden files.

#### Scenario: Lower a nested call program

- **WHEN** `main` returning `identity(identity(42))` is lowered
- **THEN** the program contains `main` and `identity` functions whose blocks evaluate arguments before calls, reference canonical targets, and end in returns, verifying clean

#### Scenario: Lower an unavailable body to a trap

- **WHEN** a discovered instance's HIR body is unavailable
- **THEN** its lowered function is a single block ending in a generated trap carrying the causative span

#### Scenario: Match the lowered golden encoding

- **WHEN** a committed fixture program is lowered and encoded
- **THEN** the encoding equals the committed golden text byte-for-byte and repeated fresh runs are byte-identical

#### Scenario: Lower bindings to locals with exit drops

- **WHEN** a body binding `first` and `second` and returning a call result is lowered
- **THEN** each binding occupies one typed local, and the return exit carries generated `Drop` operations for `second` then `first` before the return terminator, each with its causative span and generated marker

#### Scenario: Lower an ownership violation to a trap

- **WHEN** a discovered instance's ownership verdict is a violation
- **THEN** its lowered function is a single block ending in a generated trap carrying the violation diagnostic's span

### Requirement: Binary arithmetic is a trapping MIR operation

MIR SHALL represent arithmetic as one binary operation carrying the closed operator (`Add`,
`Subtract`, `Multiply`, `Divide`, `Remainder`), typed left and right operand locals, a typed
destination local, and provenance. The operation's semantics SHALL be trapping: signed overflow,
division by zero, and `-2147483648` divided or remaindered by `-1` abort the function exactly
like an explicit trap terminator, in every build mode. Division SHALL truncate toward zero and
remainder SHALL take the dividend's sign. Lowering SHALL map HIR builtin calls to binary
operations after their operands, the verifier SHALL check operand and destination locals like
every other operation, and the encoder SHALL cover the operator vocabulary, gated by committed
golden files.

#### Scenario: Lower a built-in call to a binary operation

- **WHEN** `main` returning `I32.add(40, 2)` is lowered
- **THEN** the block computes both literal operands and one `Add` binary operation into the returned local, verifying clean

#### Scenario: Verify binary operand references

- **WHEN** a hand-built module's binary operation references an undeclared local
- **THEN** the verifier reports that violation deterministically

#### Scenario: Match the arithmetic golden encoding

- **WHEN** a committed arithmetic fixture is lowered and encoded
- **THEN** the encoding equals the committed golden text byte-for-byte, naming each binary operator

### Requirement: Comparisons and user branches lower to real control flow

The MIR type vocabulary SHALL grow to `I32` and `Bool`, and the binary operator vocabulary SHALL
grow with the non-trapping comparisons `Equals`, `NotEquals`, `LessThan`, `LessOrEqual`,
`GreaterThan`, and `GreaterOrEqual`, producing `Bool` from two `I32` operands. `Bool.not` SHALL
lower through existing operations rather than a new operation kind. Lowering a conditional
statement SHALL produce a user-authored `Branch` terminator on the condition local, arm blocks
in taken-then-otherwise order, and a join block where fall-through control continues; arm-local
drops follow the cleanup plan, and lowered programs SHALL verify clean and encode
deterministically, gated by committed golden files.

#### Scenario: Lower a conditional to a diamond

- **WHEN** `pub fn main() -> I32 { if I32.equals(1, 1) { return 1 } return 0 }` is lowered
- **THEN** the entry block computes the comparison and ends in a branch whose taken block returns `1` and whose otherwise path reaches the trailing return, verifying clean

#### Scenario: Keep comparisons non-trapping

- **WHEN** any comparison operation executes at any operand values
- **THEN** it produces a boolean result and never traps

#### Scenario: Match the branching golden encoding

- **WHEN** the committed conditional fixture is lowered and encoded
- **THEN** the encoding equals the committed golden text byte-for-byte

