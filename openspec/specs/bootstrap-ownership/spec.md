# bootstrap-ownership Specification

## Purpose
The ownership and scope phase over typed HIR: per-declaration ownership facts (bindings,
ownership categories, live ranges, verdicts) and the target-neutral cleanup plan that MIR
lowering will consume to insert drops — established as a producer phase with its fact table and
artifact before any lowering exists to need them.
## Requirements
### Requirement: Ownership facts are produced once per declaration

The ownership phase SHALL run once per declaration over typed HIR and SHALL publish one immutable
ownership fact per function: its bindings with their ownership category and live range over
source spans, and a closed verdict. Bindings SHALL cover parameters and `let` statements alike:
a parameter is live from its declaration through the function body, and a `let` binding is live
from its statement through its last use — its consuming move where one exists, otherwise the end
of the function body. A function whose HIR body or contract is unavailable SHALL carry an
explicitly unavailable verdict retaining the originating diagnostic identity where one exists,
and MUST NOT report a satisfied check it could not perform.

#### Scenario: Check a copyable parameter

- **WHEN** `pub fn identity(value: I32) -> I32 { return value }` is checked
- **THEN** its ownership fact lists one copyable binding for `value` live from its declaration span through the function body, with a satisfied verdict

#### Scenario: Keep unavailable bodies explicit

- **WHEN** a function's HIR body is unavailable after recovery or an unresolved reference
- **THEN** its ownership verdict is explicitly unavailable, carrying the originating diagnostic's identity where one exists

#### Scenario: Range a let binding's liveness

- **WHEN** a body binds `let value = 42` and returns `value`
- **THEN** the ownership fact lists the binding live from its statement span through the end of the function body, with a satisfied verdict

#### Scenario: End liveness at a consuming move

- **WHEN** a body binds a value, moves it into a call argument, and returns the call's result
- **THEN** the binding's live range ends at the move's span rather than the end of the body

### Requirement: The cleanup plan is a target-neutral artifact

The phase SHALL produce one cleanup plan per function: every structured exit path with its
ordered releases in last-acquired, first-released order. A release SHALL record the end of one
binding's ownership at that exit; bindings already consumed by a move before the exit MUST NOT
be released again. The plan SHALL be target-neutral — it MUST NOT insert target-specific drops —
and it SHALL record releases uniformly whether or not the released type carries cleanup
behavior, so lowering and later cleanup-bearing types consume one shape. The plan SHALL expose a
deterministic textual encoding gated by committed golden files.

#### Scenario: Plan the single return exit

- **WHEN** a frozen-slice function with parameters is checked
- **THEN** its cleanup plan contains one return exit at the returned expression's span with an empty release list

#### Scenario: Match the cleanup golden encoding

- **WHEN** a committed fixture is checked and its plan encoded
- **THEN** the encoding equals the committed golden text byte-for-byte, naming every binding, exit, and release order

#### Scenario: Release bindings in reverse binding order

- **WHEN** a body declares `let first = 1` then `let second = 2` and returns a literal
- **THEN** the return exit releases `second` before `first`

#### Scenario: Skip a moved binding at the exit

- **WHEN** a body moves its only binding before the return
- **THEN** the return exit's release list omits that binding

### Requirement: Ownership output is deterministic

Checking the same elaborated module repeatedly in fresh processes SHALL produce identical
ownership facts, cleanup plans, and encodings.

#### Scenario: Repeat the ownership phase

- **WHEN** equivalent modules are checked repeatedly in fresh processes
- **THEN** the ownership facts, plans, and encoded texts are identical

### Requirement: Moves consume bindings

The ownership phase SHALL treat each move expression as the consuming use of its resolved
binding, including bindings of copyable types: after a move, the binding is no longer live. Any
later use — read or move — of a consumed binding SHALL produce one `OWN0001` ownership
diagnostic at the later use's span carrying the consuming move's span as a related span, and the
function's verdict SHALL be an explicit violation retaining that diagnostic's identity. A
violated function's facts SHALL remain published so inspection can present the timeline that
produced the violation.

#### Scenario: Diagnose a use after move

- **WHEN** a body moves a binding and then reads it
- **THEN** one `OWN0001` diagnostic marks the later read with the move's span related, and the function's verdict is a violation carrying that diagnostic's identity

#### Scenario: Diagnose a double move

- **WHEN** a body moves the same binding twice
- **THEN** the second move carries the `OWN0001` diagnostic and the first move's span as its related span

#### Scenario: Accept an ordinary read before a move

- **WHEN** a body reads a copyable binding and moves it afterwards
- **THEN** the verdict is satisfied because reads before the consuming move copy rather than consume

### Requirement: Arms scope their bindings and every return is an exit

A binding declared inside a conditional arm SHALL be live from its statement to the end of its
arm and SHALL be released at that arm's boundary — its arm's return exit where one exists,
otherwise the arm's end — never at an exit outside its arm. Every return statement SHALL be its
own exit in the cleanup plan, releasing the bindings live and unconsumed on paths reaching it in
last-acquired, first-released order. A move inside any arm SHALL conservatively count as
consuming for every use after the conditional, keeping the affine check sound without
path-sensitive analysis.

#### Scenario: Release an arm binding inside its arm

- **WHEN** an arm declares `let inner = 1` and returns it while the body declares `let outer = 2`
- **THEN** the arm's return exit releases `inner` then `outer`, and the trailing return exit releases only `outer`

#### Scenario: Treat a conditional move conservatively

- **WHEN** one arm moves a body binding and the trailing return reads it
- **THEN** the later read is an `OWN0001` violation even though the move was conditional



### Requirement: Nominal struct bindings are move-only owners

Ownership checking SHALL classify every user-defined struct as move-only in this slice and SHALL
track whole-value ownership independently on each structured control-flow path. An explicit move of
a whole parameter or local SHALL transfer its cleanup obligation and end the source's liveness on
that path. A later use SHALL retain the existing use-after-move diagnostic behavior.

#### Scenario: Move one aggregate binding

- **WHEN** `let next = move current` transfers a struct value
- **THEN** `next` owns the value, `current` is dead after the move, and only `next` appears in later cleanup

#### Scenario: Preserve ownership across branch paths

- **WHEN** a struct is moved in one returning branch and remains live in another branch
- **THEN** each exit records the correct path-local owner without globally consuming the other path

### Requirement: Partial struct moves are rejected

Ownership checking MUST reject a consuming access whose subject is a field projection, because this
slice has neither complete destructuring nor a replacement operation that could restore a valid
whole value. Non-consuming reads of Copy scalar fields SHALL leave the enclosing owner live.

#### Scenario: Read then move the whole struct

- **WHEN** code reads a scalar field and later moves the complete struct
- **THEN** the field read leaves ownership unchanged and the later whole move succeeds

#### Scenario: Refuse a field move

- **WHEN** code evaluates `move value.field`
- **THEN** ownership produces one partial-move violation at that access and retains the whole owner's state

### Requirement: Aggregate cleanup is recursive and exact

The target-neutral cleanup plan SHALL represent one whole-value release for each live struct owner
and SHALL retain the canonical declaration-defined field cleanup order recursively. Lowering SHALL
materialize that plan exactly once on every return and arm exit; moved sources and Copy-only field
reads MUST NOT cause duplicate or omitted cleanup.

#### Scenario: Plan cleanup for a nested struct

- **WHEN** a nested aggregate remains live at return
- **THEN** the cleanup facts identify the outer owner and its recursive declaration-defined field order exactly once

#### Scenario: Omit a moved source from cleanup

- **WHEN** a parameter is moved into the returned aggregate
- **THEN** the parameter source has no exit release and the returned owner carries the obligation across the call boundary

### Requirement: Array ownership is recursively element-derived

Ownership checking SHALL classify an array as Copy only when its element type is Copy and otherwise
as a move-only whole owner. A whole-array move SHALL end the source liveness and transfer cleanup;
ordinary use of a Copy array SHALL leave the source live.

#### Scenario: Move a struct array

- **WHEN** `let next = move current` transfers an `Array<Token, 4>`
- **THEN** only `next` remains live and owns the complete index-ordered cleanup obligation

### Requirement: Indexed non-Copy extraction is a partial move

Ownership SHALL allow a non-consuming read of a Copy leaf through any valid index/field place chain
without consuming the root owner. It SHALL reject consuming access whose selected indexed value is
not Copy, because this slice has no replacement or complete array destructuring.

#### Scenario: Read then move the complete array

- **WHEN** code reads `tokens[index].kind` and later moves the complete `tokens` array
- **THEN** the field read leaves `tokens` live and the later whole move succeeds

### Requirement: Array cleanup is index-ordered and exact

Cleanup plans SHALL retain one whole-array release with recursive element cleanup in ascending index
order. Zero-length and Copy-only arrays SHALL still produce explicit complete cleanup facts even when
they emit no runtime release action.

#### Scenario: Plan zero-length cleanup

- **WHEN** a live `Array<Token, 0>` reaches a structured exit
- **THEN** its cleanup fact is complete and contains zero element actions

### Requirement: Writes require exclusive live ownership

Ownership checking SHALL permit a write only when its root is one live mutable owner and no conflicting
access is active. Replacement SHALL transfer the right-hand value into the place, discharge the old
non-Copy value exactly once, and leave the complete root initialized. Moving from a place and writing
it later SHALL NOT provide partial-initialization semantics during bootstrap.

#### Scenario: Replace a move-only element

- **WHEN** a mutable array element is replaced by a complete move-only value
- **THEN** the old element receives one cleanup action and the array remains one complete live owner

#### Scenario: Reject mutation through an immutable root

- **WHEN** a field or index place is structurally valid but its root binding is immutable
- **THEN** ownership rejects the write without changing root liveness or cleanup

### Requirement: Loop ownership is a deterministic fixed point

Ownership SHALL analyze a structured loop until its header state reaches a deterministic fixed point.
Every path that repeats SHALL re-enter with compatible liveness and complete initialization; every
path that exits SHALL carry the appropriate live owners. A value moved on one repeating path MUST be
reinitialized before that path continues, otherwise the loop is rejected.

#### Scenario: Reassign before continuing

- **WHEN** an iteration moves a mutable binding, assigns a complete replacement, and continues
- **THEN** the next iteration begins with the binding live and initialized

#### Scenario: Reject a conditionally missing owner

- **WHEN** one path moves a non-Copy binding and continues without replacing it while another path retains it
- **THEN** ownership reports the incompatible loop-header state rather than widening it to available

### Requirement: Loop cleanup follows lexical exits

The cleanup plan SHALL attach exact ordered releases to iteration fallthrough, `continue`, `break`,
and `return` according to the lexical regions each transfer leaves. Loop repetition MUST NOT duplicate
cleanup obligations, and a `break` MUST preserve owners declared outside the loop for subsequent use.

#### Scenario: Clean nested loop exits

- **WHEN** an inner loop breaks while its outer iteration remains active
- **THEN** only inner-loop locals are released and outer-loop owners remain live

### Requirement: Union ownership derives from every nominal member

Ownership analysis SHALL classify a union as Copy only when every nominal member is Copy and
cleanup-free. Otherwise the union SHALL be one complete move-only owner whose injection, widening,
binding, storage, assignment, call, and return obey ordinary whole-value move rules. A conversion
MUST NOT duplicate, partially move, or expose the active payload.

#### Scenario: Move a payload into a union

- **WHEN** a move-only `Token` is injected and returned as `Token | End`
- **THEN** ownership transfers the complete `Token` obligation into the returned union and marks the source consumed

#### Scenario: Widen without duplicating ownership

- **WHEN** a move-only `Token | End` value widens to `Token | End | Fault`
- **THEN** the target receives the single active payload and the source union becomes unavailable

### Requirement: Union cleanup follows the active member exactly

The cleanup plan SHALL retain one union-owner release whose member cases are ordered by canonical
identity and whose runtime execution releases exactly the active payload according to that member's
ordinary recursive cleanup. Inactive members SHALL perform no cleanup, and moves, replacement,
loop transfers, returns, and traps MUST NOT duplicate the union obligation.

#### Scenario: Clean one active aggregate member

- **WHEN** a `Token | End` owner containing `Token` leaves scope
- **THEN** cleanup releases the `Token` fields in their ordinary order and performs no `End` cleanup

#### Scenario: Replace a mutable union

- **WHEN** assignment replaces an owned union containing `Token` with one containing `End`
- **THEN** the old active `Token` is cleaned once before the new complete union commits

### Requirement: Match modes preserve affine ownership

Ownership checking SHALL classify a bare match as a Copy read, a consuming match as one whole-value
transfer, a shared match as one lexical shared borrow, and an exclusive match as one lexical
exclusive borrow requiring a mutable live root. Borrowed pattern bindings SHALL end at their arm and
MUST NOT escape or be consumed. A consuming match SHALL make the source unavailable and transfer the
active payload into exactly one selected arm.

#### Scenario: End a shared arm borrow

- **WHEN** a shared arm reads a Copy field and returns a scalar
- **THEN** the borrow ends at the arm boundary and the source owner retains its original cleanup obligation

#### Scenario: Reject an escaping pattern borrow

- **WHEN** a shared or exclusive pattern binding would become the match result or enter owned storage
- **THEN** ownership reports the escape and publishes no executable match

### Requirement: Consuming destructuring cleans exactly one selected payload

For a consuming nominal arm, bound non-Copy fields SHALL become arm-local owners and omitted fields
acknowledged by `..` SHALL remain cleanup obligations. Branch exit, early return, nested control,
guard failure, and traps SHALL release every untransferred active field exactly once in canonical
cleanup order. Inactive union members and the consumed source SHALL receive no cleanup.

#### Scenario: Clean omitted fields

- **WHEN** a consuming `Token` arm returns one moved field and omits another with `..`
- **THEN** only the omitted active field is cleaned in the arm and neither the moved field nor inactive members are released there

#### Scenario: Guard failure preserves the payload

- **WHEN** a consuming guarded arm rejects the active member and a later arm handles the same member
- **THEN** ownership transfers the payload only into the selected later arm without cleaning or duplicating it during the failed guard

### Requirement: Generic ownership is checked once and specialized exactly

Ownership SHALL classify canonical type parameters through compiler-owned Copy and cleanup
properties, check whole-value moves and cleanup once on generic HIR, and substitute that proof for
each concrete instance. A specialization MUST NOT duplicate cleanup or re-check the source body with
concrete-only behavior.

#### Scenario: Specialize move-only and Copy uses
- **WHEN** a checked generic whole-value transfer is instantiated once with `I32` and once with a move-only struct
- **THEN** each instance receives the correct concrete copy or cleanup actions from one generic ownership proof

### Requirement: Slice loans attach to stable owner roots

Every available slice borrow SHALL create a compiler-only loan identity attached to the complete
source owner root. Any number of shared loans MAY coexist, while an exclusive loan MUST conflict
with every other live loan. A shared loan SHALL prevent mutation, replacement, movement, or cleanup
of its root; an exclusive loan SHALL prevent every direct use of its root. Loan identity and access
MUST NOT become runtime fields.

#### Scenario: Permit shared aliases

- **WHEN** one call supplies two shared borrows of the same live array root
- **THEN** ownership accepts both loans for the complete invocation

#### Scenario: Reject conflicting call arguments

- **WHEN** one invocation supplies shared and exclusive borrows or two exclusive borrows of the same root
- **THEN** ownership rejects the conflict because every argument loan overlaps all later argument evaluation and the complete callee execution

#### Scenario: Reject owner use during a loan

- **WHEN** source attempts to move, replace, mutate, or clean an owner while a conflicting slice loan is live
- **THEN** ownership diagnoses the owner operation and preserves the original loan and cleanup state

### Requirement: Slice loans remain call-scoped and non-escaping

An explicit borrow argument SHALL begin before its argument value is supplied and end only after the
ordinary callee returns. A function slice parameter SHALL remain borrowed for the complete function
body. Slice types MUST be rejected recursively from return types, struct or union fields, fixed
arrays, owned generic wrappers, lazy flow environments, and other escaping captures. Standalone
slice local bindings and borrows of temporaries or subplaces MUST be rejected in this bootstrap
capability.

#### Scenario: End a temporary loan after an ordinary call

- **WHEN** an exclusive whole-array borrow is passed to an ordinary function and that function returns
- **THEN** the call loan ends and subsequent caller access to the mutable owner is permitted

#### Scenario: Reject recursive storage of a slice

- **WHEN** a slice type appears directly or transitively inside an owned struct, union, array, or generic application
- **THEN** ownership rejects the containing type at the escaping boundary

#### Scenario: Reject a captured slice

- **WHEN** a lazy computation or callback would retain a slice after call construction
- **THEN** ownership rejects the capture rather than ending the source loan prematurely

### Requirement: Structured exits end loans before owner cleanup

Every successful return, typed failure, early return, loop `break`, loop `continue`, and lexical
fallthrough SHALL end loans belonging to exited regions before scheduling cleanup of their backing
owners. Cleanup SHALL remain exactly once and element-derived. A trap SHALL retain the existing
trap semantics and MUST NOT pretend that normal cleanup ran.

#### Scenario: Exit a loop containing a slice call

- **WHEN** `break` or `continue` leaves a region after a call-scoped borrow completes
- **THEN** the loan ends before the structured outcome and the backing owner remains valid for its eventual single cleanup

#### Scenario: Return after an exclusive write

- **WHEN** a callee replaces a move-only element through an exclusive slice and returns early
- **THEN** the displaced element and eventual backing array elements are each cleaned exactly once after the relevant loans end

### Requirement: Flow capture and failure transfer obey ordinary ownership

Flow construction SHALL retain moved and borrowed capture obligations without executing the body.
Running transfers or borrows captures according to the original call, `fail move` consumes its
payload, propagation transfers it once, and recovery gives one owned payload to the matching
handler. Cleanup SHALL occur exactly once for values in every region actually exited.

#### Scenario: Reject a second run after a taken capture

- **WHEN** a closed flow captures an affine argument by move and is run twice
- **THEN** ownership rejects the second run and identifies the consumed capture

#### Scenario: Clean before propagation

- **WHEN** a flow fails after constructing a live affine local
- **THEN** cleanup leaves the exited region before the owned failure reaches its caller

### Requirement: Ownership unifies Effect captures allocation and Drop

Ownership SHALL treat Effect environments, allocations, raw buffers, vectors, external-resource
wrappers, and failure payloads through the same affine model. It SHALL transfer cleanup on move,
end lexical provider borrows after calls, reject illegal repeat or slot escape, and schedule Drop
exactly once on every structured exit and typed failure. It MUST NOT schedule normal cleanup for a
trap.

#### Scenario: Move an allocated Vector through typed failure control

- **WHEN** a Vector is moved into a repeatable-ineligible Effect that may fail before consuming it
- **THEN** each reachable path has exactly one owner and cleanup plan, and a second run is rejected
