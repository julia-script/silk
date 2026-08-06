# bootstrap-mir Specification

## Purpose
MIR: the monomorphic, backend-neutral basic-block control-flow graph over logical Silk types that
every backend and the interpreter consume — its data model, structural invariants and verifier,
the compiler-owned target/layout plan, and the deterministic textual encoder, stabilized against
hand-built samples before lowering exists.
## Requirements
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


### Requirement: MIR carries canonical nominal logical types

MIR locals, parameters, call results, and function results SHALL accept canonical nominal struct
types alongside built-in scalar types. Nominal identity SHALL remain the defining module and
declaration name; MIR MUST NOT replace it with structural field shapes, physical offsets, or backend
types. Every nominal MIR type SHALL have the same reachable catalog entry and calling shape in the
program's completed layout plan.

#### Scenario: Lower a nominal factory contract

- **WHEN** a reachable factory returns `Token`
- **THEN** its MIR result and every receiving local carry the canonical `Token` logical type and reference one selected plan entry

### Requirement: Aggregate construction and projection are explicit MIR operations

MIR SHALL represent construction as one destination nominal local plus declaration-ordered source
locals identified by canonical fields. MIR SHALL represent projection as one typed destination,
one nominal source local, and one canonical field identity. Whole-value moves, calls, returns, and
drops SHALL continue to use ordinary MIR operations over the widened logical type vocabulary.

#### Scenario: Lower a reordered literal canonically

- **WHEN** HIR constructs a struct whose source initializers were reordered
- **THEN** MIR construction operands follow canonical declaration order with their field identities and source provenance

#### Scenario: Lower a chained projection

- **WHEN** HIR reads `token.span.start`
- **THEN** MIR contains two ordered projection operations whose intermediate and final locals have the declared nominal and scalar types

#### Scenario: Lower whole-value cleanup

- **WHEN** ownership plans a live aggregate release
- **THEN** MIR emits one whole-value drop carrying generated provenance and no per-backend cleanup decision

### Requirement: MIR verifies aggregate consistency

MIR verification SHALL reject aggregate operations whose nominal type, field identity, operand type,
declaration order, layout entry, calling shape, or local type disagree. It SHALL also reject a
nominal call or return that does not match the compiler-selected lane shape. Violations SHALL remain
ordered deterministic data.

#### Scenario: Reject a mismatched construction field

- **WHEN** a construction operand names a field from another nominal type
- **THEN** verification reports the canonical field/type mismatch before evaluation or emission

#### Scenario: Reject a missing aggregate ABI shape

- **WHEN** a nominal parameter or result lacks its selected calling shape
- **THEN** verification reports the missing plan fact and no backend receives the module

### Requirement: Aggregate MIR encoding is deterministic

The textual MIR encoding SHALL include canonical nominal types, field identities, construction and
projection operands, whole-value moves and drops, calling shapes, and provenance in stable order.
Equivalent aggregate programs SHALL encode byte-identically across fresh processes.

#### Scenario: Repeat aggregate MIR lowering

- **WHEN** one nested construction-and-projection program is lowered repeatedly
- **THEN** its logical types, operations, lane shapes, field order, and encoding are byte-identical
