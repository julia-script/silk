# bootstrap-ownership Specification

## Purpose

The ownership and scope phase over typed HIR: per-declaration ownership facts (bindings,
ownership categories, live ranges, verdicts) and the target-neutral cleanup plan that MIR
lowering will consume to insert drops — established as a producer phase with its fact table and
artifact before any lowering exists to need them.

## Requirements

### Requirement: Ownership facts are produced once per residual specialization

After private residual and cleanup-call candidate closure is complete, the ownership phase SHALL run
once for each residual runtime HIR specialization and SHALL publish
one immutable ownership fact for that specialization: its runtime bindings with their ownership
category and live range over source spans, and a closed verdict. Static parameters, static local
bindings, static-function locals, inactive static arms, and static evaluator storage MUST NOT appear
as owned bindings or cleanup obligations. Runtime parameters and residual `let` statements SHALL
retain their ordinary liveness and move behavior.

A specialization whose residual HIR body or runtime contract is unavailable SHALL carry an
explicitly unavailable verdict retaining the originating diagnostic identity where one exists and
MUST NOT report a satisfied check it could not perform. A static evaluation that fails before
producing residual HIR SHALL publish its static diagnostic and no ownership fact for that failed
specialization.

Cleanup-call candidate discovery MAY use a target-neutral prepass over residual types and exits, but
that prepass MUST NOT publish ownership, liveness, borrow, or cleanup-plan facts and MUST NOT admit
executable reachability before the residual graph is closed.

#### Scenario: Check a copyable parameter

- **WHEN** a mixed function specializes to a residual body reading one runtime `i32` parameter
- **THEN** its ownership fact lists that copyable parameter through the residual body and omits every static input

#### Scenario: Keep unavailable bodies explicit

- **WHEN** selected residual HIR is unavailable after recovery or an unresolved selected reference
- **THEN** its ownership verdict is explicitly unavailable and carries the originating diagnostic identity

#### Scenario: Omit a failed static specialization

- **WHEN** `compileError` or an evaluation limit prevents a specialization from producing residual HIR
- **THEN** ownership publishes no satisfied, violated, or partial fact for that specialization

#### Scenario: Range a let binding's liveness

- **WHEN** a selected runtime arm binds `let value = 42` and returns `value`
- **THEN** the residual ownership fact lists the binding from its statement through the residual return

#### Scenario: End liveness at a consuming move

- **WHEN** a residual body moves one runtime binding into a call argument and later source does not use it
- **THEN** the binding's live range ends at that move while any unselected-arm use is absent from ownership analysis

### Requirement: The cleanup plan is a target-neutral artifact

The phase SHALL produce one cleanup plan per successful residual runtime specialization: every
structured residual exit path with its ordered releases in last-acquired, first-released order. A
release SHALL record the end of one runtime binding's ownership at that exit; bindings already
consumed by a move before the exit MUST NOT be released again. Static values and evaluator storage
MUST NOT produce releases. The plan SHALL remain target-neutral, SHALL record runtime releases
uniformly whether or not the released type carries cleanup behavior, and SHALL expose a
deterministic textual encoding gated by committed golden files.

#### Scenario: Plan the single return exit

- **WHEN** static selection leaves one residual return path with runtime parameters
- **THEN** the cleanup plan contains that return exit and only the releases required by residual runtime bindings

#### Scenario: Match the cleanup golden encoding

- **WHEN** a committed mixed-function fixture is specialized repeatedly with the same inputs
- **THEN** its cleanup encoding is byte-for-byte identical and names no static binding or inactive-arm release

#### Scenario: Release bindings in reverse binding order

- **WHEN** a selected runtime arm declares `let first = 1` then `let second = 2` and returns a literal
- **THEN** the residual exit releases `second` before `first`

#### Scenario: Skip a moved binding at the exit

- **WHEN** a residual body moves its only runtime binding before return
- **THEN** the exit release list omits that binding while static values remain outside the plan

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

### Requirement: Copy is one sealed validated property

A type SHALL be Copy only through the compiler's single sealed Copy property. A user MAY declare
`impl Copy` without operations when every stored field is Copy and no cleanup obligation exists.
The compiler SHALL reject operation bodies, non-Copy fields, `Drop`, allocation ownership, cycles,
unavailable proofs, and conflicting evidence. A nominal without an admitted `impl Copy` SHALL
remain affine even when all of its fields are Copy.

#### Scenario: Opt a plain struct into Copy

- **WHEN** a struct containing only Copy fields declares an empty `impl Copy`
- **THEN** reads may duplicate its value and arrays, unions, and generic bounds derive that same property

#### Scenario: Reject Copy over allocated storage

- **WHEN** a struct owns allocated memory or has a Drop hook and declares `impl Copy`
- **THEN** conformance validation rejects the declaration before ownership analysis uses it

#### Scenario: Move one affine aggregate binding

- **WHEN** `let next = move current` transfers a struct without an admitted `impl Copy`
- **THEN** `next` owns the value, `current` is dead after the move, and only `next` appears in later cleanup

#### Scenario: Preserve ownership across branch paths

- **WHEN** an affine struct is moved in one returning branch and remains live in another branch
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

### Requirement: Stored executable values obey ordinary aggregate ownership

Represented callable and Effect values SHALL derive Copy, moves, partial-move rejection, cleanup,
and storage behavior from their realized fields. The compiler SHALL retain access-specific capture
restrictions but SHALL NOT classify every executable-bearing nominal as affine solely because it
contains executable representation.

#### Scenario: Store a Copy callable representation

- **WHEN** a callable representation contains only Copy captures and satisfies the sealed Copy rule
- **THEN** an aggregate containing it follows ordinary Copy behavior

#### Scenario: Reject moving one affine executable field

- **WHEN** an aggregate contains an affine captured callable and another field
- **THEN** moving the callable field reports the ordinary partial-move diagnostic and retains the complete owner for recovery

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

### Requirement: Union ownership derives from every normalized member

Ownership analysis SHALL classify a union as Copy only when every normalized member is Copy and
cleanup-free. Otherwise the union SHALL be one complete move-only owner whose injection, widening,
binding, storage, assignment, call, and return obey ordinary whole-value move rules. A conversion
MUST NOT duplicate, partially move, or expose the active payload.

#### Scenario: Move a payload into a union

- **WHEN** a move-only `Token` is injected and returned as `Token | End`
- **THEN** ownership transfers the complete `Token` obligation into the returned union and marks the source consumed

#### Scenario: Derive Copy from non-nominal members

- **WHEN** every member of `i32 | Array<i32, 2>` is Copy and cleanup-free
- **THEN** the union is Copy without requiring nominal declarations or user-written conformance

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

Ownership SHALL classify canonical type parameters through the compiler-owned sealed Copy property
and cleanup rules, check whole-value moves and cleanup once on generic HIR, and substitute that proof
for each concrete instance. A parameter SHALL be Copy only under an explicit `Copy` bound. A
specialization MUST NOT invent structural Copy evidence, duplicate cleanup, or re-check the source
body with concrete-only behavior.

#### Scenario: Propagate an explicit Copy bound

- **WHEN** a generic caller whose parameter is bounded by `Copy` supplies that parameter to another Copy-bounded declaration
- **THEN** the caller's symbolic Copy evidence satisfies the nested call without enumerating concrete types

#### Scenario: Specialize affine and Copy uses

- **WHEN** a checked generic whole-value transfer is instantiated once with `i32` and once with an affine struct
- **THEN** each instance receives the correct concrete copy or cleanup actions from one generic ownership proof

### Requirement: Mutable owned parameters provide local writable storage

An ordinary or Effect function MAY declare `mut name: T` only when `T` is an owned parameter type. The
parameter SHALL remain the same callable-contract type and SHALL require the same explicit caller
transfer as `name: T`, while its function-local root permits whole-value, field, and fixed-array
element replacement. Whole replacement MUST clean the displaced value exactly once. An explicit
move MAY empty the root and a later complete assignment MAY reinitialize it. Borrowed parameters
and service or interface contract parameters MUST reject the binding-level `mut` prefix.

#### Scenario: Mutate and return an owned parameter

- **WHEN** `fn increment(mut counter: Counter) -> Counter` updates a field and returns `move counter`
- **THEN** the parameter root is writable, the caller must supply an explicit affine move, and the returned value owns the transferred storage

#### Scenario: Replace complete parameter storage

- **WHEN** a mutable affine parameter is assigned one complete replacement value
- **THEN** cleanup destroys the displaced value once and the replacement remains live for later use or transfer

#### Scenario: Reject parameter self-overlap

- **WHEN** an assignment consumes a value from the same mutable parameter root that it replaces
- **THEN** ownership reports the ordinary overlapping-assignment diagnostic without treating `mut` as permission for self-consumption

### Requirement: Borrowed-view loans attach to stable owner roots

Every available reference or slice borrow SHALL create a compiler-only loan identity attached to the complete
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

- **WHEN** source attempts to move, replace, mutate, or clean an owner while a conflicting borrowed-view loan is live
- **THEN** ownership diagnoses the owner operation and preserves the original loan and cleanup state

### Requirement: Borrowed-view loans remain lexical and non-escaping

An explicit borrow argument SHALL begin before its argument value is supplied and end only after the
ordinary callee returns. A function borrowed-view parameter SHALL remain borrowed for the complete function
body. A returned one-source view MAY extend its call loan through a lexical local's last use.
Reference and slice types MUST be rejected recursively from struct or union fields, fixed arrays, owned generic
wrappers, and escaping captures. Direct shared and exclusive borrow bindings, lexically bounded
callable or Effect captures, materialized temporary owners, and stable projected places SHALL use
the same lexical loan and non-escape rules as call-scoped borrows.

#### Scenario: End a temporary loan after an ordinary call

- **WHEN** an exclusive whole-array borrow is passed to an ordinary function and that function returns
- **THEN** the call loan ends and subsequent caller access to the mutable owner is permitted

#### Scenario: Reject recursive storage of a slice

- **WHEN** a slice type appears directly or transitively inside an owned struct, union, array, or generic application
- **THEN** ownership rejects the containing type at the escaping boundary

#### Scenario: Reject a captured slice

- **WHEN** a lazy computation or callback would retain a borrowed view beyond its source root
- **THEN** ownership rejects the escape rather than ending the source loan prematurely

#### Scenario: Store a lexical borrow locally

- **WHEN** a local binding stores `&values` and is used only within the owner's lifetime
- **THEN** ownership ends the loan at the local view's last use and restores compatible owner access

### Requirement: Lexical borrows may name stable temporary and subplace roots

The compiler SHALL assign stable logical identities to materialized temporaries and addressable
subplaces, allow shared or exclusive borrows to be stored in local bindings, and preserve provenance
through projections and calls. A borrow SHALL remain lexical and SHALL NOT escape its owner's valid
lifetime.

#### Scenario: Borrow an array temporary for one call

- **WHEN** `read(&[1, 2])` uses the borrow only during the call
- **THEN** the compiler materializes a stable temporary root and accepts the call

#### Scenario: Mutate an indexed subplace through its original storage

- **WHEN** `edit(&mut matrix[index])` mutates the selected inner array
- **THEN** the loan retains the root and checked selector path and the caller observes the mutation in `matrix`

#### Scenario: Extend a hidden owner through a returned local view

- **WHEN** `identity(&[1, 2])` returns its one-source view into a local binding
- **THEN** the hidden owner remains live through that binding's last use and is cleaned after the loan ends

#### Scenario: Reject a returned local view

- **WHEN** a function returns a view borrowed from a local array
- **THEN** ownership reports that the view would outlive its owner

### Requirement: Returned views preserve source provenance through their live range

Ownership facts SHALL identify the single source owner of every accepted returned view and carry
that provenance through assignments and compatible reborrows. While a shared returned view is live,
the owner MUST NOT be mutated, moved, or dropped. While an exclusive returned view is live, the owner
MUST NOT be otherwise read, mutated, moved, or dropped. Conflicting access MAY resume after the
view's last use.

#### Scenario: Suspend mutation for a shared returned view

- **WHEN** a caller keeps a shared returned view live and attempts to mutate its source owner
- **THEN** ownership rejects the mutation and relates it to the live view's origin

#### Scenario: Suspend every competing access for an exclusive returned view

- **WHEN** a caller keeps an exclusive returned view live and attempts a second access to its source owner
- **THEN** ownership rejects the competing access until the exclusive view's last use

#### Scenario: Move the owner after the view ends

- **WHEN** a returned view's last use precedes a whole-owner move
- **THEN** the view no longer suspends the owner and the ordinary move succeeds

#### Scenario: Reject dropping a borrowed owner

- **WHEN** a structured exit would drop an owner while a returned view derived from it remains live
- **THEN** ownership rejects the exit rather than emitting cleanup that invalidates the view

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

### Requirement: Effect capture and failure transfer obey ordinary ownership

Effect construction SHALL retain moved and borrowed capture obligations without executing the body.
Running transfers or borrows captures according to the original call, `fail move` consumes its
payload, propagation transfers it once, and recovery gives one owned payload to the matching
handler. Cleanup SHALL occur exactly once for values in every region actually exited.

#### Scenario: Reject a second run after a taken capture

- **WHEN** a closed Effect captures an affine argument by move and is run twice
- **THEN** ownership rejects the second run and identifies the consumed capture

#### Scenario: Clean before propagation

- **WHEN** an Effect fails after constructing a live affine local
- **THEN** cleanup leaves the exited region before the owned failure reaches its caller

### Requirement: Failure payloads obey ordinary detached ownership

Every value admitted as an Effect failure type SHALL use its ordinary Copy, move, Drop, union-tag,
and cleanup behavior. `fail`, propagation, selective recovery, whole-channel recovery, and re-fail
SHALL transfer one ordinary payload without a row wrapper. A failure payload SHALL be detached and
owned; a lexical or provider borrow that could escape SHALL be rejected by ordinary ownership.

#### Scenario: Propagate an affine ordinary failure once

- **WHEN** an affine failure payload crosses nested Effect calls before recovery
- **THEN** ownership transfers one payload and schedules exactly one cleanup if it remains unconsumed

#### Scenario: Reject an escaping borrowed failure

- **WHEN** `fail` attempts to publish a lexical borrow as the Effect failure value
- **THEN** the ordinary borrow-escape diagnostic rejects it before executable lowering

#### Scenario: Recover a structural failure union

- **WHEN** a handler receives one selected alternative from an ordinary failure union
- **THEN** its pattern narrowing, moves, and cleanup use the same ownership rules as that union in any other value position

### Requirement: Ownership unifies Effect captures allocation and Drop

Ownership SHALL treat Effect environments, allocations, raw buffers, vectors, external-resource
wrappers, and failure payloads through the same affine model. It SHALL transfer cleanup on move,
end lexical provider borrows after calls, reject illegal repeat or slot escape, and schedule Drop
exactly once on every structured exit and typed failure. It MUST NOT schedule normal cleanup for a
trap.

#### Scenario: Move an allocated Vector through typed failure control

- **WHEN** a Vector is moved into a repeatable-ineligible Effect that may fail before consuming it
- **THEN** each reachable path has exactly one owner and cleanup plan, and a second run is rejected

### Requirement: Callable environments obey ordinary affine ownership

Ownership SHALL derive callable invocation mode from how its environment is accessed: read-only
captures permit shared reusable calls, mutated or exclusively borrowed captures require exclusive
reusable calls, and an invocation that consumes any captured owner is take-once. Callable moves,
borrows, aggregate storage, returns, and drops SHALL use the same ownership and dependency rules as
other values. A provider or owner retained by a callable MUST remain immovable and live as required
until the callable releases that dependency.

#### Scenario: Reject a second taking call

- **WHEN** one invocation consumes an owned capture and the same callable is invoked again
- **THEN** ownership rejects the second invocation and identifies the consumed environment slot

#### Scenario: Release captures on callable drop

- **WHEN** a callable with owned and borrowed captures is dropped without invocation
- **THEN** owned captures clean exactly once and every capture loan ends at that drop

#### Scenario: Reject provider movement while retained

- **WHEN** a callable retains a borrow from a provider and code attempts to move or drop that provider
- **THEN** ownership rejects the provider operation while permitting valid shared capability use

### Requirement: Callable sections admit every non-empty trailing suffix

For an `N`-parameter callable, supplying `K` arguments where `0 < K < N` SHALL bind those arguments
to the callable's ordered trailing suffix and produce a callable awaiting the remaining ordered
leading parameters. Sections MAY be applied in stages and SHALL move or borrow supplied arguments
exactly once according to their parameter contracts. A section SHALL NOT bind holes or reorder
parameters.

#### Scenario: Partially apply a binary function

- **WHEN** `add` has two parameters and source evaluates `add(2)`
- **THEN** the result is a callable accepting the remaining parameter and eventually computing `add(value, 2)`

#### Scenario: Stage a multi-parameter section

- **WHEN** `combine(a, b, c)` is applied as `combine(3)(2)(1)`
- **THEN** each application binds the next trailing parameter exactly once and the final invocation computes `combine(1, 2, 3)`

#### Scenario: End a reusable capture loan at last invocation

- **WHEN** a reusable callable's last statically known invocation occurs before its lexical binding ends
- **THEN** a non-escaping capture loan may end after that invocation while escaping, stored, or later-used callables retain the loan

### Requirement: Callable capture loans follow the last safe use

Reusable callable capture loans SHALL end after the last statically known invocation or explicit
drop only when the callable is not subsequently copied, stored, returned, captured, or otherwise
escaped. Effect runs and callable invocations SHALL use the same conservative last-use policy.

#### Scenario: Retain a stored callable loan

- **WHEN** a callable is invoked and then copied or stored for later use
- **THEN** its capture loan remains active rather than ending at the earlier invocation

### Requirement: Pipeline application preserves ownership order

Ownership SHALL analyze the pipeline left value before constructing or accessing the right callable,
then transfer or borrow the left value according to the callable's leading parameter. Failures in
callable construction or invocation MUST NOT duplicate the left value or any capture.

#### Scenario: Pipe an affine value once

- **WHEN** an affine value is piped into a callable whose leading parameter consumes it
- **THEN** the source binding becomes moved exactly once before the callable result is available

### Requirement: Allocation cleanup is one affine obligation

Ownership SHALL treat every successful `Allocation`, `RawBuffer<T>`, construction guard, and owner
containing them as affine. Moves SHALL transfer one cleanup obligation; explicit drop and automatic
Drop SHALL consume it; a failed request SHALL create none. A live lexical Slot loan SHALL prevent
moving, dropping, or reborrowing its backing buffer incompatibly. Restricted hooks SHALL run before
field cleanup, and cleanup plans SHALL cover every structured exit while preserving the original
typed failure. Traps SHALL add no unwind promise.

#### Scenario: End a slot loan before buffer transfer

- **WHEN** a lexical slot projection ends before its backing RawBuffer moves into another owner
- **THEN** ownership ends the exclusive loan first and transfers one live cleanup obligation with the buffer

#### Scenario: Reject duplicate early drop

- **WHEN** source drops one allocation and later reads, moves, or drops the consumed binding
- **THEN** ownership emits one stable use-after-move diagnostic and publishes no conflicting cleanup plan

#### Scenario: Clean a typed failure path

- **WHEN** an allocating Effect fails after acquiring several affine locals
- **THEN** ownership schedules restricted hooks and field cleanup in the specified order before propagating the unchanged failure payload

### Requirement: Suspension transfers one ownership obligation per live value

After concrete specialization and suspendability-aware MIR normalization, ownership SHALL derive
the exact MIR-local live set needed after a deferred child transfers and later completes. The set
SHALL include compiler-generated temporaries as well as locals corresponding to source bindings.
Copy values MAY be copied; affine values SHALL occupy one field in exactly one state of the
invocation's reusable coroutine frame; and shared or exclusive borrows and provider references
SHALL retain their exact root, access, and loan dependencies until resumption or exit. A referent
that remains borrowed across suspension SHALL retain a stable logical location for the borrow's
lifetime regardless of private frame placement or relocation. A value MUST NOT remain independently
owned by both the running state and suspended state.

#### Scenario: Hold one owner per recursive level

- **WHEN** every level of a suspended recursive Effect creates one affine owner used after its child completes
- **THEN** ownership places each owner in exactly one active invocation frame state and rejects any duplicate use from another state

#### Scenario: Retain an exclusive provider dependency

- **WHEN** source code intentionally holds an ordinary exclusive provider reference across its deferred child
- **THEN** ownership keeps that provider immovable and exclusively borrowed until the parent resumes and ends the loan

#### Scenario: Preserve a borrow across private frame growth

- **WHEN** a valid source borrow remains live while the private execution stack grows, segments, or relocates implementation storage
- **THEN** the borrow continues to identify the same referent with unchanged access and lifetime

### Requirement: Continuation cleanup preserves structured-exit semantics

On successful resumption, ordinary return, fallthrough, explicit structured exit, or typed failure,
each coroutine-frame state SHALL move or clean every live source value exactly once in the existing
lexical order, then release its completed private frame through the execution owner without
replacing the original typed outcome. A state transition SHALL finish moving every live obligation
before the driver starts the deferred child, and no obligation may appear in two simultaneously
owned states. A source trap or target defect that cannot return to the runner SHALL retain the
existing no-unwind guarantee: it MUST NOT report that source `Drop` ran or duplicate an obligation.

#### Scenario: Complete one frame-state transition

- **WHEN** a parent invocation suspends while affine values remain live after its child
- **THEN** every retained value belongs to the completed parent frame state before the child begins and no prior state retains a duplicate owner

#### Scenario: Clean deep success in order

- **WHEN** suspended recursion succeeds while every level retains one owner
- **THEN** owners release exactly once in the same inner-to-outer order as the equivalent unsuspended execution

#### Scenario: Clean deep typed failure in order

- **WHEN** an inner suspended level fails with a typed payload while outer levels retain owners
- **THEN** every exited level releases its owner exactly once before the unchanged failure reaches its handler

#### Scenario: Preserve trap semantics

- **WHEN** a resumed suspended computation reaches a source trap or exhausts private execution-stack storage
- **THEN** the runner exposes no typed failure or successful Drop trace and makes no claim that normal source cleanup ran

### Requirement: Statement patterns preserve whole-value ownership

Every local or conditional pattern SHALL receive one recursive ownership plan derived from its
initializer access. An unconditional pattern SHALL be irrefutable. A consuming conditional SHALL
consume before testing and clean the active payload exactly once on both outcomes; a borrowed
conditional SHALL retain the owner and scope its loans to the conditional body. Post-statement
ownership SHALL join deterministically.

#### Scenario: Consume on conditional mismatch

- **WHEN** `if let Token token = move value` does not select `Token`
- **THEN** the unmatched active payload is cleaned and `value` remains consumed after the statement

#### Scenario: End a conditional borrow

- **WHEN** `if let Token token = &value` completes either body
- **THEN** the pattern loan ends and the owner is available after the conditional

#### Scenario: Retain irrefutable borrowed bindings

- **WHEN** `let Point { x, .. } = &point` succeeds
- **THEN** `x` retains its scoped shared view while unrelated shared reads of `point` remain valid

### Requirement: Short-circuit branches join ownership path-locally

Ownership SHALL analyze the right operand of `&&` and `||` as a conditional branch. Moves, loans,
mutation, and cleanup obligations inside that operand SHALL apply only on the executed path, and a
value used after the expression SHALL be valid only when it remains live on every reaching path.

#### Scenario: Clean a skipped affine operand

- **WHEN** a short-circuit condition skips a right operand that would consume an affine value
- **THEN** the skipped path retains and cleans that value exactly once

#### Scenario: Reject a use after a conditional move

- **WHEN** one reaching path moves a value in the right operand and source uses it afterward
- **THEN** ownership reports the ordinary use-after-move diagnostic with the move provenance

#### Scenario: End a conditional loan on its path

- **WHEN** a right operand creates a lexical borrow and then completes
- **THEN** the borrow ends before the branch joins and does not remain active on the skipped path

### Requirement: Every local shared core handle is one affine obligation

For every available `Intrinsic.SharedCore<T>` specialization, ownership SHALL classify the handle as
affine regardless of whether `T` is Copy. A whole-handle move SHALL transfer exactly one live
`LocalSharedStrong` obligation and end the source; ordinary reads or structural derivation MUST NOT
duplicate the handle. The contained `T` SHALL keep its ordinary ownership category and MUST NOT be
copied, moved, or cleaned merely because a handle moves. Ownership facts SHALL retain the
`LocalExecution` affinity established by semantic analysis. An unavailable element specialization
SHALL retain its causal diagnostic and unavailable ownership verdict rather than fabricate a Copy,
unrestricted, or satisfied result. Aggregate ownership SHALL retain one distinct obligation for
each structurally live core handle; a structural union SHALL retain only the obligations of its
active member.

#### Scenario: Move one core handle

- **WHEN** a local shared core handle moves from one binding to another in the same local execution
- **THEN** the source becomes dead, the destination owns the same single `LocalSharedStrong` obligation, and no operation on `T` is planned

#### Scenario: Reject a non-consuming handle read in ownership

- **WHEN** source attempts a non-consuming read that would duplicate a local shared core handle
- **THEN** ownership publishes an `OWN0003` violation at the attempted read, retains the affine handle fact, and publishes no duplicated obligation

#### Scenario: Reject Copy conformance before ownership

- **WHEN** source declares `impl Copy` for a nominal containing a local shared core handle
- **THEN** conformance validation publishes `SEM0083` at the implementation declaration, admits no Copy evidence, and ownership continues to classify available values of that nominal as affine

#### Scenario: Keep a Copy element behind an affine handle

- **WHEN** the core element type is `i32`
- **THEN** the core retains one `LocalSharedStrong` obligation and moving it does not copy the stored integer

#### Scenario: Specialize generic ownership independently of the element

- **WHEN** one generic wrapper over `Intrinsic.SharedCore<T>` is specialized with a Copy `T` and with an affine `T`
- **THEN** each available specialization owns exactly one affine `LocalSharedStrong` obligation and neither specialization owns or duplicates `T` through the handle

#### Scenario: Retain a handle inside a local executable

- **WHEN** a handle moves into an ordinary callable or Effect that remains within one local execution
- **THEN** ownership transfers exactly one obligation into the environment, ends the source, and preserves `LocalExecution` affinity

#### Scenario: Retain every handle obligation in aggregate storage

- **WHEN** a nominal, fixed array, callable, or Effect stores two independently live local shared core handles
- **THEN** ownership retains two distinct `LocalSharedStrong` obligations, while a structural union containing such values retains only the obligations of its active member

#### Scenario: Retain a handle across local suspension and resumption

- **WHEN** a handle moves through suspension, parking, resumption, or between independently resumable frames in one same-thread local execution domain
- **THEN** the source frame ends when moved, the destination frame retains `LocalExecution` affinity and exactly one live `LocalSharedStrong` obligation, and no park or resume creates or discharges an obligation

#### Scenario: Preserve unavailable element ownership

- **WHEN** ownership receives `Intrinsic.SharedCore<Missing>` with causal element-resolution diagnostics
- **THEN** it retains an unavailable verdict and those causes without publishing a Copy category, unrestricted affinity, satisfied verdict, or live handle obligation

### Requirement: Strong-handle transitions preserve one dynamic cleanup authority

Each successful clone SHALL add exactly one affine strong-handle obligation without copying, moving,
or cleaning `T`. A non-last explicit or structured drop SHALL discharge one obligation and perform
no payload cleanup. The drop that changes the count from one to zero SHALL exclusively clean `T`
exactly once and then release the retained allocation exactly once. Strong-count state SHALL remain
independent of access state, so clone and non-last drop MAY occur during active access without
creating another reference or changing the active access owner. A strong cycle SHALL remain live and
leak. Every acyclic graph whose handles are all discharged through structured execution SHALL reach
exact last cleanup. A fatal trap SHALL retain the language's no-unwind rule and MUST NOT claim that
live handles, payloads, or allocations were cleaned.

#### Scenario: Drop two handles in order

- **WHEN** one handle is cloned and the original is dropped before the clone
- **THEN** the first drop changes count two to one without cleaning `T`, and the second cleans `T` once before one allocation release

#### Scenario: Clean across typed-failure frames

- **WHEN** a deeper typed-failure frame drops its clone and the propagating caller later drops the original
- **THEN** the first cleanup only decrements and the caller's final cleanup destroys `T` and releases storage without replacing the failure payload

#### Scenario: Clone during access

- **WHEN** an active access callback clones its borrowed receiver through another live alias
- **THEN** the count increments while access remains active and no additional reference to `T` is created

#### Scenario: Leak a strong cycle

- **WHEN** external handles to an otherwise unreachable cycle of local shared cores are dropped
- **THEN** no count reaches zero and the cycle receives no payload cleanup or allocation release

### Requirement: Local shared access borrows are callback-scoped and non-escaping

Successful local shared access SHALL create one exclusive position-restricted borrow rooted in the
control block for exactly the ordinary callback invocation. Every competing reentrant access,
including shared-over-shared public wrappers derived from that exclusive operation, SHALL select the
conflict path before another reference is formed. The borrow SHALL end before access is restored and
before the result returns. It MUST NOT escape directly or through a generic result, aggregate,
failure value, Effect capture, callable capture, or suspended computation. Diagnostics SHALL retain
the access boundary and the attempted escape or suspension provenance.
Every direct, narrowed, generic, aggregate, failure, Effect, callable, or suspension rejection SHALL
use one stable local-shared-access diagnostic identity and retain the access-boundary span plus the
specific escape or suspension span.

#### Scenario: Return an ordinary value

- **WHEN** the access callback reads or mutates `T` and returns an owned result containing no restricted borrow
- **THEN** ownership ends the borrow, restores access, and permits later access through any live alias

#### Scenario: Reject a direct returned borrow

- **WHEN** the callback returns its `&mut T` parameter or a narrowed borrow derived from it
- **THEN** ownership rejects the result and relates the escape to the local shared access boundary

#### Scenario: Reject generic and executable escape

- **WHEN** the callback hides the borrow in a generic result, Effect, stored callable, aggregate, or failure payload
- **THEN** recursive ownership checking rejects the capture before executable lowering

#### Scenario: Reject suspension with active access

- **WHEN** a path attempts to suspend while the callback-scoped borrow remains live
- **THEN** ownership rejects the suspension and no suspended state owns the access loan

#### Scenario: Conflict every nested access combination

- **WHEN** public shared or exclusive access is nested under public shared or exclusive access through any alias
- **THEN** the nested call selects conflict and ownership never admits overlapping references

### Requirement: Independent executions retain exact affine ownership and loan boundaries

Ownership SHALL treat every available `Intrinsic.Execution<A>` as one affine, non-Copy,
non-thread-transferable obligation independent of `A`. Moving it SHALL end the source and transfer
the same obligation; completion or ordinary drop SHALL discharge it exactly once. Construction
MUST reject an executable or fixed endpoint that retains an external lexical or provider loan.
Loans created after activation MAY cross parking only when their referents are owned inside the same
Execution and retain stable logical locations; cleanup SHALL end each loan before its referent.
Completion MUST reject an `A` that borrows body, frame, endpoint, or package storage that completion
will clean.

#### Scenario: Move one execution owner

- **WHEN** an Execution moves from a source binding into owner storage
- **THEN** the source ends and the destination retains exactly one non-Copy execution obligation

#### Scenario: Reject duplicate execution use

- **WHEN** source attempts to drive or drop an Execution after it was moved into a prior drive
- **THEN** ordinary ownership reports use-after-move and publishes no second activation obligation

#### Scenario: Retain an internal loan across parking

- **WHEN** an activated execution creates a loan into a value it owns, parks, and later resumes
- **THEN** the loan keeps a stable logical referent and dormant cleanup ends it before cleaning the owned referent

#### Scenario: Retain an owned Shared handle across parking

- **WHEN** a Running Execution owns a `Shared<T>` handle and parks without an active Shared access borrow
- **THEN** the Dormant Execution retains the same strong handle obligation and later resume preserves it without granting thread transfer

#### Scenario: Reject parking with active Shared access

- **WHEN** direct or transitively reached external park occurs while a `Shared.with` or `Shared.withMut` access borrow is live
- **THEN** ownership reports the canonical local-shared-access diagnostic and creates no suspended frame or dormant execution state

#### Scenario: Reject an external loan at construction

- **WHEN** an Effect or endpoint passed toward Execution construction retains a caller lexical or provider loan
- **THEN** ownership preserves the loan cause and the Detached obligation is unsatisfied before erasure

#### Scenario: Reject a completion result borrowing package state

- **WHEN** `A` would contain a loan into the body environment, continuation frames, endpoint, or combined package
- **THEN** ownership rejects the escaping result before construction or drive can erase it

#### Scenario: Reject thread transfer

- **WHEN** a future or unsafe-adjacent operation attempts to transfer an Execution across local execution domains without a parallel-memory contract
- **THEN** the canonical local-affinity fact prevents the transfer and no atomic semantics are implied

### Requirement: Execution construction and drive have consuming cleanup matrices

Ownership SHALL model `executionFromAllocation` as one all-or-nothing consuming transition over the
Allocation, body, endpoint state, and endpoint callback. It SHALL model `drive` as consuming the
Execution, affine branch state, and both take-once callbacks, with exactly one callback receiving
the branch state. Completion SHALL transfer `A`, clean the unused suspension callback and remaining
package values, and discharge the Execution obligation. Suspension SHALL clean the unused
completion callback and transfer the same Execution obligation through `onSuspend`. Dropping an
Initial or returned Execution SHALL clean all live values exactly once in dependency-safe order.

#### Scenario: Consume initializer inputs

- **WHEN** a valid initializer executes
- **THEN** Allocation, body, endpoint state, and endpoint callback sources end and one Initial Execution owns all corresponding obligations

#### Scenario: Complete through one branch

- **WHEN** drive completes
- **THEN** the completion callback receives the sole branch state and `A`, the suspension callback is cleaned once, and no Execution obligation remains

#### Scenario: Suspend through one branch

- **WHEN** drive externally parks
- **THEN** the suspension callback receives the sole branch state and same Execution obligation, while the completion callback is cleaned once

#### Scenario: Clean a never-driven body

- **WHEN** an Initial Execution owning affine captures is dropped
- **THEN** every capture and endpoint value is cleaned once before the package Allocation and no callback obligation is invoked as control flow

#### Scenario: Preserve no-unwind trap semantics

- **WHEN** execution-stack growth or an illegal intrinsic state traps
- **THEN** ownership publishes no promised cleanup or recoverable failure beyond Silk's language-wide no-unwind contract

### Requirement: Parking and wake retain exact generation obligations

Ownership SHALL create one affine Wake obligation per park generation and transfer it to the
registration callback. It SHALL retain returned `G`, endpoint state `O`, reusable endpoint callback
`R`, and live frame values in the Dormant Execution. Wake consumption or drop SHALL discharge the
Wake obligation once. Resumption SHALL clean `G` immediately before source continuation; dormant
destruction SHALL cancel first and then clean `G`, endpoint, body, and frames in dependency order.
Notification SHALL borrow `O` and `R` under a transient retain; reentrant destruction MUST defer
their cleanup until that borrow ends. An internal loan SHALL end before its owned referent on every
destroy path.

#### Scenario: Transfer Wake into registration state

- **WHEN** park invokes registration and the callback stores Wake in source state
- **THEN** the callback source ends, source state owns the sole Wake obligation, and the Execution retains exactly one `G`

#### Scenario: Consume Wake once

- **WHEN** source calls `Intrinsic.wake(move wake)`
- **THEN** the binding ends and no second live Wake obligation exists for that generation

#### Scenario: Resume cleanup order

- **WHEN** an Eligible execution is driven through the saved park continuation
- **THEN** ownership cleans `G` once, ends any loans held by `G`, and only then continues source after park

#### Scenario: Dormant cleanup order

- **WHEN** a Dormant execution is dropped while Wake remains external
- **THEN** cancellation precedes endpoint/frame cleanup, internal loans end before referents, and only inert Allocation authority remains with Wake

### Requirement: Scalar enums are compiler-proved Copy values

Ownership analysis SHALL classify every valid scalar enum as sealed `Copy` and cleanup-free without
requiring a source conformance. Scalar enums SHALL NOT admit user `Copy` or `Drop` implementations or
conformances. Enum bindings SHALL follow the existing Copy read and explicit-move
rules, and cleanup plans SHALL contain no enum-specific release or drop operation.

#### Scenario: Plan a function holding enum values

- **WHEN** a function binds, copies, compares, and returns scalar enum values
- **THEN** ownership facts remain satisfied and the cleanup plan contains no release for those values

#### Scenario: Protect endpoint borrows during reentrant destruction

- **WHEN** endpoint invocation borrows `O` and reentrant source destroys the Execution
- **THEN** ownership records deferred cleanup and does not end the endpoint borrow or clean `O` or `R` until invocation returns

### Requirement: Loan live-ranges account for uses nested in place and effect expressions

Loan-end analysis SHALL treat identifier and callable occurrences nested inside place-replace, effect-result, and requirement-binding expressions as uses at that occurrence: they SHALL extend the enclosing loan's live range and SHALL invalidate any earlier record that treated the callable's last invocation as its final use.

#### Scenario: View used inside a place replace keeps its loan live

- **WHEN** a shared view's last use sits inside a place-replace expression's value operand and the borrowed owner is mutated between the view's direct uses and that nested use
- **THEN** ownership analysis reports owner access during the loan — the view loan's live range extends through the place-replace use rather than ending at the last direct use

### Requirement: Nominal union ownership follows nominal struct rules

A union value SHALL be affine by default. `Copy` and `Drop` implementations, generic Copy bounds,
moves, borrows, writes, partial-move rejection, and implementation admissibility SHALL follow the
same rules as nominal structs across every variant payload. The compiler MUST NOT infer Copy merely
because all currently reachable payload fields are Copy.

#### Scenario: Require an explicit Copy implementation

- **WHEN** every field of every variant is Copy but the union declares no valid `impl Copy`
- **THEN** reading the union as a whole consumes it under ordinary affine ownership

#### Scenario: Validate Copy across every variant

- **WHEN** a union requests `Copy` and one variant contains an affine field under the declared bounds
- **THEN** conformance is rejected at that field even when another variant is unit

### Requirement: Cleanup follows exactly one active variant

Owned union cleanup SHALL run the union's admitted nominal cleanup behavior and recursively clean
exactly the initialized fields of the active variant once. Variant selection, structural-union
injection, moves, borrows, typed-failure transfer, ordinary scope exits, and generic specialization
MUST preserve that single active obligation. Fatal traps SHALL retain the existing no-unwind rule.

#### Scenario: Clean one selected payload

- **WHEN** a union holding a droppable field in one variant leaves scope
- **THEN** every engine runs the union-level and active-field cleanup prescribed by ordinary struct ordering without touching inactive variant storage

#### Scenario: Consume one field variant through matching

- **WHEN** a moved match extracts one payload field and omits another with `..`
- **THEN** ownership transfers the extracted field once and cleans only the selected variant's omitted fields

### Requirement: Ownership analyzes only generated runtime projections and values

Static type descriptors, field descriptors, static sequences, parsed template values, static loop
bindings, and inactive iterations SHALL create no runtime binding, move, borrow, loan, liveness,
cleanup, or destructor fact. After residualization, every generated field access SHALL obey the
ordinary ownership mode of its concrete operation. Template formatting SHALL use a shared borrow of
the argument pack and shared field projections, so formatting MUST NOT consume or mutate the pack or
its fields.

#### Scenario: Borrow an anonymous record temporary for formatting

- **WHEN** `&.{ name: "Julia", age: 32 }` is passed directly to template formatting
- **THEN** ownership creates one hidden temporary owner, keeps it live through all generated field displays, and cleans it after the complete formatting call

#### Scenario: Keep static plans outside cleanup

- **WHEN** template parsing creates and replaces immutable static sequences
- **THEN** no sequence allocation, replacement, or value appears in the runtime cleanup plan

### Requirement: Tuple-backed and anonymous aggregates obey ordinary struct ownership

Named tuples and anonymous aggregate values SHALL use the existing whole-value nominal struct
ownership rules. Reads, moves, borrows, partial-move rejection, Copy evidence, mutation, and
structured-exit cleanup MUST NOT depend on whether the nominal declaration was written in source or
synthesized from a literal. Anonymous aggregate creation MUST NOT synthesize `Copy` evidence merely
because every current member is Copy.

Tuple cleanup order SHALL follow ordinal declaration order, and anonymous record cleanup order SHALL
follow the canonical source field order recorded by its synthesized declaration. A move of the
whole aggregate SHALL transfer exactly one recursive cleanup obligation; separate fields or
positions MUST NOT become independently movable unless ordinary struct ownership later admits that
operation for all nominal structs.

#### Scenario: Move one anonymous record as a whole

- **WHEN** a local anonymous record is moved into an owning generic call
- **THEN** the source binding becomes dead and the callee receives its one declaration-ordered cleanup obligation

#### Scenario: Refuse a positional partial move

- **WHEN** source requests a consuming move from one position of an affine named tuple
- **THEN** ownership rejects the partial move and retains the complete tuple owner's state

#### Scenario: Avoid implicit Copy for anonymous aggregates

- **WHEN** every field of an anonymous record is Copy but no nominal Copy evidence can be declared for its generated type
- **THEN** the record remains affine while non-consuming reads of its Copy fields follow ordinary struct rules

### Requirement: Referent places preserve borrowed ownership

A bare referent projection SHALL read only when the target has sealed `Copy` conformance and SHALL
leave the backing owner available. It SHALL NOT move an affine target through borrowed storage.
Shared referents SHALL permit only shared reborrows and reads, while exclusive referents SHALL also
permit exclusive reborrows and replacement with ordinary cleanup.

#### Scenario: Read a Copy scalar through a shared reference

- **WHEN** `value.*` reads `u32` from `value: &u32`
- **THEN** the result is copied and the backing owner remains available

#### Scenario: Reject an affine referent read

- **WHEN** a bare projection attempts to read a non-Copy target
- **THEN** ownership analysis rejects the borrowed move

#### Scenario: Reject mutation through shared access

- **WHEN** source assigns through a shared referent or requests `&mut value.*`
- **THEN** ownership analysis rejects the access strengthening

#### Scenario: Replace through exclusive access

- **WHEN** source assigns a compatible value through an exclusive referent
- **THEN** the previous referent is cleaned up exactly once
- **AND** the exclusive owner is restored after the access ends

### Requirement: Raw pointers are Copy and loan-free

A raw pointer type SHALL be Copy through the sealed Copy property, SHALL contribute no cleanup
obligation to any aggregate, array, or union containing it, and SHALL be storable in any position
that admits a Copy value. Forming a pointer from a borrow SHALL be an ordinary read of that borrow
and SHALL create no loan on the root.

#### Scenario: Store a pointer in a struct

- **WHEN** a struct declares a field `handle: *mut Opaque` and no other non-Copy field, and declares `impl Copy`
- **THEN** conformance accepts the struct as Copy and its cleanup plan is empty

#### Scenario: Forming a pointer leaves the root movable

- **WHEN** code forms a pointer from `&mut value` and then moves `value`
- **THEN** ownership records no loan conflict
