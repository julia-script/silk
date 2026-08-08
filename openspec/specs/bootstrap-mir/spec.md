# bootstrap-mir Specification

## Purpose
MIR: the monomorphic, backend-neutral basic-block control-flow graph over logical Silk types that
every backend and the interpreter consume — its data model, structural invariants and verifier,
the compiler-owned target/layout plan, and the deterministic textual encoder, stabilized against
hand-built samples before lowering exists.
## Requirements
### Requirement: MIR is a backend-neutral structured control DAG over logical types

A MIR module SHALL represent each function as a structured control DAG over logical Silk types and
typed locals. Ordered operation blocks, conditional regions, loop regions, cleanup regions, and
terminal outcomes SHALL retain canonical identities and provenance. Child and continuation edges
MUST be acyclic and deterministically ordered. Repetition SHALL exist only as the semantics of an
explicit loop region whose condition and body are themselves DAG regions; arbitrary block back-edges
MUST fail verification.

MIR SHALL carry the compiler-selected target and layout plan but MUST NOT contain LLVM or WebAssembly
types, instructions, labels, nesting depths, or backend-owned physical representations. A backend
SHALL receive the preserved DAG and convert it into its own control form without recovering source
structure from flattened control flow.

#### Scenario: Model a straight-line function

- **WHEN** a hand-built function returns a called constant
- **THEN** its entry region contains ordered literal and call operations ending in a return outcome over logical `I32`

#### Scenario: Model structured repetition without a cycle

- **WHEN** MIR represents a `while` loop with a conditional `continue` and `break`
- **THEN** one loop region owns acyclic condition and body regions whose terminal outcomes name repeat or exit ports without a graph back-edge

#### Scenario: Reject an arbitrary cycle

- **WHEN** a hand-built MIR region directly or indirectly lists itself as a child or continuation
- **THEN** verification reports the cycle deterministically before evaluation or emission

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

### Requirement: MIR carries canonical logical array types

MIR locals, parameters, calls, and function results SHALL accept logical arrays identified by
canonical element type and length. Every reachable array use SHALL reference the same selected layout
and calling shape; MIR MUST NOT replace an array with an untyped scalar bundle.

#### Scenario: Lower an array factory

- **WHEN** a reachable function returns `Array<I32, 3>`
- **THEN** its MIR result and receiving locals retain that exact logical array type

### Requirement: Array construction and checked indexing are explicit MIR operations

MIR SHALL represent complete array construction with ascending canonical element operands. It SHALL
lower each readable Copy place chain to one checked read carrying the root aggregate local, ordered
field or index selectors, every dynamic `I32` index local and canonical length, the final Copy result
type, and exact trap provenance. Non-Copy intermediate aggregates MUST NOT become independently
owned locals. Whole moves, calls, returns, and drops SHALL continue to use ordinary operations over
complete logical values.

#### Scenario: Lower a dynamic index

- **WHEN** HIR indexes an array with a parameter
- **THEN** MIR contains one checked place read that either produces the final Copy value or traps at the index span

### Requirement: MIR verifies array consistency deterministically

Verification SHALL reject array operations whose element count, operand type, index type, canonical
length, selector path, layout entry, calling shape, destination type, or whole-value ownership mode
disagrees. Text encoding SHALL include canonical array types, lengths, selectors, operations, and
provenance in stable order.

#### Scenario: Reject a malformed construction

- **WHEN** an `Array<I32, 3>` construction carries two operands
- **THEN** verification reports the exact completeness violation before evaluation or emission

### Requirement: MIR writes replace typed places explicitly

MIR SHALL represent assignment as one checked `WritePlace` carrying the root local, ordered field and
index selectors, dynamic index locals and canonical lengths, exact destination and source types,
replacement cleanup, and provenance. Place checks and right-hand evaluation SHALL precede the commit,
and the verifier SHALL reject inconsistent mutability, selectors, types, layouts, calling shapes, or
cleanup modes.

#### Scenario: Lower an array element replacement

- **WHEN** HIR assigns a complete value to `values[index]`
- **THEN** MIR checks the index and evaluates the source before one verified write commits

### Requirement: MIR loop outcomes preserve lexical cleanup

Each loop region SHALL expose canonical repeat and exit outcomes. Lowering SHALL map body fallthrough
and `continue` to repeat, `break` to exit, and `return` to the function outcome through the exact
cleanup regions selected by ownership. Cleanup sharing MAY make the representation a DAG rather than
a tree, but every owner SHALL be released at most once on any execution path.

#### Scenario: Lower continue through cleanup

- **WHEN** an iteration-local owner is live at `continue`
- **THEN** the transfer traverses its cleanup region before reaching the loop repeat outcome

### Requirement: Control DAG verification and encoding are deterministic

Verification SHALL reject missing or duplicate region identities, cyclic child/continuation edges,
invalid lexical transfer targets, incompatible loop-header locals, unreachable required outcomes,
and operation/type/layout disagreements as ordered data. Text encoding SHALL traverse regions in one
canonical topological order and encode structured children, outcomes, selectors, cleanup, and
provenance identically across fresh processes.

#### Scenario: Repeat loop encoding

- **WHEN** one nested mutable-loop program is lowered repeatedly in fresh processes
- **THEN** its region identities, topological order, operations, outcomes, and textual bytes are identical

### Requirement: MIR carries canonical logical union types

MIR SHALL represent a union as its normalized ordered nominal member set while referencing the
compiler-selected layout and calling shape for physical facts. Locals, contracts, struct fields,
arrays, writes, calls, returns, and drops SHALL preserve that logical type. MIR MUST NOT contain
source spelling order, aliases, backend types, numeric tags chosen outside the layout plan, or
backend-local control labels.

#### Scenario: Lower an aggregate-contained union

- **WHEN** HIR constructs and transports a struct whose field is `Token | End`
- **THEN** MIR retains one canonical logical union type and the program's matching layout-plan entry

### Requirement: MIR union conversion carries a total member mapping

MIR SHALL lower nominal injection and union widening to an explicit verified conversion operation
containing source and destination locals, exact source and target logical types, a total canonical
source-member to target-member mapping, layout/calling-shape references, access mode, and provenance.
The verifier SHALL reject unsorted or duplicate members, non-containing targets, incomplete or
incorrect mappings, inconsistent locals or layouts, and conversions that would narrow.

#### Scenario: Lower nominal injection

- **WHEN** HIR injects `Token` into `Token | End`
- **THEN** MIR contains one conversion mapping `Token` to its compiler-planned target member

#### Scenario: Lower union widening

- **WHEN** HIR widens `Token | End` to `Token | End | Fault`
- **THEN** MIR maps every source member exactly once while preserving the structured control DAG

#### Scenario: Reject an incomplete widening map

- **WHEN** hand-built MIR omits the `End` mapping from a two-member source union
- **THEN** verification reports the exact missing member before evaluation or emission

### Requirement: Union MIR encoding is deterministic

Text encoding SHALL include canonical union types, members, conversion mappings, layout references,
active-member cleanup plans, and provenance in stable order. Equivalent programs SHALL produce
byte-identical MIR across fresh processes without materializing mutable graph identity.

#### Scenario: Repeat a widening encoding

- **WHEN** one program injects, stores, widens, and drops a union in repeated fresh compilations
- **THEN** its MIR type keys, mappings, regions, cleanup, and textual bytes are identical

### Requirement: MIR carries verified logical match regions

MIR SHALL represent a match as one evaluated scrutinee local, exact logical type and access mode,
canonical member cases in source decision order, optional guard regions, pattern-bound locals,
per-arm result and cleanup regions, and one typed join outcome. Member cases SHALL reference the
compiler layout plan while omitting source aliases, public numeric tags, backend types, target
blocks, branch depths, and arbitrary cyclic edges.

#### Scenario: Lower a complete two-member match

- **WHEN** HIR exhaustively matches `Token | End` with two unguarded arms
- **THEN** MIR contains one structured acyclic selection whose cases produce one verified joined result local

#### Scenario: Keep guarded member order

- **WHEN** two guarded arms and one unguarded fallback arm target the same nominal member
- **THEN** MIR preserves their source decision order and guard fallthrough without duplicating the scrutinee payload

### Requirement: MIR verifies match coverage bindings and cleanup

Verification SHALL reject a match whose scrutinee or result local disagrees with its logical type or
layout, whose member cases are invalid or non-exhaustive, whose source decision order contradicts
the semantic coverage facts, whose pattern field or binding types disagree, whose guard is not
`Bool`, whose access mode violates ownership metadata, or whose arm result and cleanup outcomes do
not reach the declared join consistently. Violations SHALL be deterministic data produced before
evaluation or backend emission.

#### Scenario: Reject a missing member case

- **WHEN** hand-built MIR omits one required unguarded member and has no universal case
- **THEN** verification identifies the exact uncovered canonical member

#### Scenario: Reject an escaping borrow local

- **WHEN** a match-local shared or exclusive binding is referenced outside its arm region
- **THEN** verification reports its arm boundary and no backend receives the program

### Requirement: Match MIR encoding is deterministic

Text encoding SHALL include scrutinee and result types, access mode, source-ordered decisions,
canonical members, pattern paths, guards, bound locals, cleanup, arm outcomes, join relationships,
and provenance in stable topological order. Equivalent exhaustive matches SHALL encode
byte-identically across fresh processes.

#### Scenario: Repeat guarded match lowering

- **WHEN** one guarded and destructuring match is lowered repeatedly
- **THEN** its member decisions, regions, bindings, cleanup, join, and encoded bytes are identical

### Requirement: MIR contains only monomorphic generic instances

MIR lowering SHALL consume verified concrete instance keys and substitute every parameterized
logical type and operation before constructing the structured control DAG. Each function SHALL
retain provenance naming its generic declaration and concrete arguments, while the verifier MUST
reject open type parameters or missing concrete layout entries.

#### Scenario: Lower a concrete identity
- **WHEN** discovery supplies `identity<Token>`
- **THEN** MIR contains one concrete Token-typed function whose provenance names the generic declaration and `Token` argument

#### Scenario: Reject open generic MIR
- **WHEN** a malformed MIR function still contains type parameter `T`
- **THEN** verification rejects it before evaluation or backend emission

### Requirement: MIR represents Effect and owned allocation in the structured DAG

MIR SHALL represent effect entry, capture access, retry, typed outcomes, provider acquisition,
validated allocation, raw-buffer slot operations, initialization commit or rollback, explicit drop,
and automatic Drop as ordered regions and operations in the existing acyclic structured control DAG.
It MUST NOT encode source named scopes, dynamic cleanup registries, or backend control structures.

MIR SHALL carry the compiler-selected hidden Effect instance and capture-environment plan separately
from its success/failure outcome. Running a statically known instance SHALL call its generated runner
without universal runtime Effect dispatch.

#### Scenario: Encode a failed append attempt

- **WHEN** Vector growth fails with OutOfMemory inside a retried Effect
- **THEN** MIR orders failed acquisition, rollback of attempt-local owners, failure propagation, and retry without a leaked allocation or cyclic MIR edge

### Requirement: MIR represents typed outcomes in the structured DAG

MIR SHALL represent flow calls, success returns, nominal failure returns, propagation, and catch
dispatch as explicit target-aware structured operations and outcomes. Verification SHALL reject row,
tag, payload, calling-shape, ownership, cleanup, or target inconsistencies before execution. The
compiler representation SHALL remain a DAG.

#### Scenario: Verify propagation cleanup

- **WHEN** a flow call may fail after earlier locals became live
- **THEN** MIR contains distinct success and failure continuations with cleanup before forwarding the failure

#### Scenario: Reject a forged failure tag

- **WHEN** malformed MIR associates a payload with another nominal member's tag
- **THEN** verification rejects it before evaluator or backend execution
### Requirement: MIR represents slice loans in the structured control DAG

Concrete monomorphic slice types SHALL remain logical shared or exclusive slice types in MIR. Slice
formation SHALL identify one stable backing place, loan identity, access mode, element type, and
lexical region; loan endings SHALL be explicit ordered facts on every structured exit. These
operations SHALL remain inside the existing acyclic operation and region structure, including when
loops later repeat through lexical outcomes, and MUST NOT expose a source-level raw pointer.

#### Scenario: Lower a call-scoped borrow

- **WHEN** HIR passes one whole-array borrow to an ordinary function
- **THEN** MIR orders slice formation before the call and the matching loan end after the call in the same structured region

#### Scenario: End an iteration-local loan before repetition

- **WHEN** a loop body forms a call-scoped slice and reaches `continue`
- **THEN** the loan ends before the loop's lexical repeat outcome without introducing a cyclic MIR edge

### Requirement: MIR slice places derive bounds from one slice value

Length, check, read, projection, and write operations for a slice SHALL derive the backing address,
runtime length, access mode, and element type from the same verified slice local. Runtime indexing
MUST use unsigned `I32` comparison semantics so negative values and values at or above length trap.
An exclusive write SHALL validate its destination before evaluating the replacement and SHALL
commit only after displaced-value cleanup.

#### Scenario: Verify one runtime-bounded place

- **WHEN** MIR reads `slice[index].field`
- **THEN** verification proves that the check and place projection use the same slice local and canonical element-field path

#### Scenario: Reject mismatched slice bounds

- **WHEN** malformed MIR attempts to check against one slice but address through another slice or a fixed constant
- **THEN** MIR verification reports the inconsistency before evaluation or backend emission

### Requirement: MIR verifies loan conflicts and cleanup order

MIR verification SHALL reject an owner move, direct access, write, or drop that conflicts with a
live loan; duplicate exclusive access; a missing or duplicate loan end; and cleanup scheduled before
the last applicable loan end. Shared and exclusive slices SHALL have the same runtime shape even
though their verified access permissions differ.

#### Scenario: Reject owner cleanup during a loan

- **WHEN** malformed MIR drops an array root before ending its live slice loan
- **THEN** verification identifies the owner, loan, and invalid operation without delegating borrow safety to a backend

### Requirement: MIR represents target-selected Usize values in the DAG

MIR SHALL represent `Usize` literals and operations with the selected compiler-owned unsigned word
lane. Verification SHALL reject out-of-range literals, mismatched operand widths or types, signed
comparison/division semantics, and arithmetic results lacking the required overflow or underflow
trap behavior. The structured control representation SHALL remain a DAG.

#### Scenario: Reject a mismatched word lane

- **WHEN** malformed native MIR assigns a 32-bit lane to `Usize`
- **THEN** verification rejects it before evaluation or backend emission

### Requirement: MIR represents callable environments in the structured DAG

MIR SHALL represent monomorphic callable construction, ordered captures, shared, exclusive, or
consuming environment access, direct or indirect application, and cleanup as typed operations and
regions in the existing backend-neutral acyclic control DAG. Verification SHALL reject open generic
callables, mismatched callable signatures, invalid invocation modes, duplicate capture transfers,
and cleanup that can occur before a retained dependency is released.

#### Scenario: Lower a reusable arithmetic section

- **WHEN** a stored `I32.add(2)` section reaches runtime
- **THEN** MIR contains one concrete callable environment and typed unary application without a surface pipeline operation

#### Scenario: Verify a consuming environment

- **WHEN** malformed MIR invokes a take-once environment twice
- **THEN** verification rejects the second application before evaluation or backend emission

### Requirement: MIR run order follows the elaborated operand

Lowering SHALL place `run` around the complete elaborated Effect operand chosen by syntax and HIR,
including every ungrouped pipeline combinator. Grouped run results SHALL remain ordinary values that
may feed later callable applications.

#### Scenario: Retry before run

- **WHEN** source spells `run attempt |> Effect.retry(2)`
- **THEN** MIR constructs the retry composition before entering its run region

### Requirement: MIR verifies allocation and cleanup as a structured DAG

MIR SHALL contain compiler-planned operations and regions for checked layout formation, general
allocator witness dispatch, typed allocation outcomes, self-contained reclaim authority, raw typed
buffer construction, lexical Slot projection and value operations, initialization commit or
rollback, restricted Drop calls, explicit drop, and automatic field cleanup. Verification SHALL
reject layout/type/provenance mismatch, slot escape, conflicting live loans, use after consumption,
invalid hook contracts, missing cleanup on a structured exit, duplicate release, and allocator-kind
or named-scope operations. Runtime initializedness inside an unsafe buffer remains an unsafe program
invariant rather than a verifier claim.

#### Scenario: Encode an exhausted construction attempt

- **WHEN** allocation fails before a construction guard receives storage
- **THEN** MIR carries the `OutOfMemory` branch with cleanup for earlier live owners and no allocation release operation for the rejected request

#### Scenario: Encode partial rollback

- **WHEN** a later typed failure exits after a guard initialized a prefix
- **THEN** the DAG orders the guard hook, initialized-element destruction, allocation release, and unchanged failure propagation without a control back-edge

#### Scenario: Reject forged reclaim authority

- **WHEN** malformed MIR attaches a release operation to a different allocation identity or inactive ticket
- **THEN** verification rejects the program before evaluation or backend emission
