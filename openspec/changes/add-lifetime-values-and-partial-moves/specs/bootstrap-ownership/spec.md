## RENAMED Requirements

- FROM: `### Requirement: Partial struct moves are rejected`
- TO: `### Requirement: Owned fields support verified partial moves`

## MODIFIED Requirements

### Requirement: Owned fields support verified partial moves

Ownership SHALL admit explicit consuming moves of definitely initialized visible owned fields and nested fields when no projection crosses a dereference or an ancestor with user-defined whole-value Drop, and no overlapping live loan forbids the move. The selected subtree SHALL become uninitialized, disjoint initialized siblings SHALL remain usable, and the transferred value SHALL retain its own cleanup and loan obligations. Moving a complete Drop-bearing field from an outer owner without a hook SHALL remain valid. Explicit moves SHALL consume even Copy fields; ordinary Copy reads SHALL preserve initialization.

#### Scenario: Read then move the whole struct

- **WHEN** code reads a scalar field and later moves the complete struct
- **THEN** the field read leaves ownership unchanged and the later whole move succeeds

#### Scenario: Move an independently owned field

- **WHEN** code evaluates `move value.field`
- **THEN** ownership transfers the initialized eligible field, records the hole, and retains initialized sibling state

#### Scenario: Preserve a whole-value Drop boundary

- **WHEN** a nested field move crosses an enclosing user Drop hook, even if source promises restoration
- **THEN** ownership rejects the move because an intervening structured exit must retain a complete Drop receiver

#### Scenario: Refuse a field move

- **WHEN** move value.field crosses a reference, an enclosing user Drop hook, or an overlapping live loan
- **THEN** ownership rejects the move and preserves valid initialization for recovery

### Requirement: Moves consume bindings

The ownership phase SHALL treat each move expression as the consuming use of its resolved initialized place, including places of copyable types: after a move, that place is uninitialized. Whole-binding moves end binding liveness; field moves preserve disjoint initialized siblings. Any
later use — read or move — of a consumed place SHALL produce one `OWN0001` ownership
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
last-acquired, first-released order. Reaching branches SHALL join initialization independently for each tracked place; unreachable returning branches SHALL NOT consume values on surviving paths. A place SHALL be usable only when definitely initialized on every reachable incoming path.

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

#### Scenario: Copy a shared borrowed holder

- **WHEN** a nominal with only shared borrowed Copy fields and no cleanup declares an admitted Copy implementation
- **THEN** copies retain the referenced data lifetimes and all live dependents continue to restrict their referents; affine-by-default and exclusive non-Copy rules remain unchanged

### Requirement: Stored executable values obey ordinary aggregate ownership

Represented callable and Effect values SHALL derive Copy, moves, partial moves, cleanup,
and storage behavior from their realized fields. The compiler SHALL retain access-specific capture
restrictions but SHALL NOT classify every executable-bearing nominal as affine solely because it
contains executable representation.

#### Scenario: Store a Copy callable representation

- **WHEN** a callable representation contains only Copy captures and satisfies the sealed Copy rule
- **THEN** an aggregate containing it follows ordinary Copy behavior

#### Scenario: Move one affine executable field

- **WHEN** an aggregate contains an affine captured callable and another field
- **THEN** moving the initialized callable field from an eligible owner transfers its environment and loans and leaves its siblings initialized

#### Scenario: Reject moving one affine executable field

- **WHEN** an affine captured callable field is moved through an aggregate reference
- **THEN** ownership rejects leaving a hole in borrowed storage

### Requirement: Aggregate cleanup is recursive and exact

The target-neutral cleanup plan SHALL represent one complete or partial release for each live struct owner
and SHALL retain the canonical declaration-defined field cleanup order recursively. Missing fields SHALL be skipped and maybe-initialized fields SHALL be conditionally cleaned. Lowering SHALL
materialize that plan exactly once on every structured exit; moved sources and Copy-only field
reads MUST NOT cause duplicate or omitted cleanup.

#### Scenario: Plan cleanup for a nested struct

- **WHEN** a nested aggregate remains live at return
- **THEN** the cleanup facts identify the outer owner and its recursive declaration-defined field order exactly once

#### Scenario: Omit a moved source from cleanup

- **WHEN** a parameter is moved into the returned aggregate
- **THEN** the parameter source has no exit release and the returned owner carries the obligation across the call boundary

#### Scenario: Clean a partial remainder

- **WHEN** a function returns an extracted field while its eligible source retains other initialized fields
- **THEN** cleanup releases only the remaining source fields in their established relative order and the returned field carries its own obligation

### Requirement: Array ownership is recursively element-derived

Ownership checking SHALL classify an array as Copy only when its element type is Copy and otherwise
as an affine owner admitting sparse constant-index partial moves under the ordinary place rules. A whole-array move SHALL end the source liveness and transfer cleanup;
ordinary use of a Copy array SHALL leave the source live.

#### Scenario: Move a struct array

- **WHEN** `let next = move current` transfers an `Array<Token, 4>`
- **THEN** only `next` remains live and owns the complete index-ordered cleanup obligation

### Requirement: Indexed non-Copy extraction is a partial move

Ownership SHALL allow a non-consuming read of a Copy leaf through any valid index/field place chain without consuming the root owner. An explicit move of a definitely initialized element at a statically known in-bounds fixed-array index SHALL empty that element and preserve disjoint initialized indices when no dereference or whole-value Drop boundary is crossed. Dynamic-index and opaque collection extraction SHALL be rejected unless an ordinary owning operation preserves the collection invariant. A dynamic access overlapping a possible hole and a whole-array borrow spanning a hole SHALL be rejected.

#### Scenario: Read then move the complete array

- **WHEN** code reads `tokens[index].kind` and later moves the complete `tokens` array
- **THEN** the field read leaves `tokens` live and the later whole move succeeds

#### Scenario: Extract a constant-index element

- **WHEN** source moves values[2] from an owned complete fixed array and subsequently reads a Copy leaf of values[1]
- **THEN** the move and disjoint read are accepted while using values as a complete array is rejected until index 2 is restored

### Requirement: Array cleanup is index-ordered and exact

Cleanup plans SHALL retain one complete or partial array release with recursive cleanup of initialized elements in ascending index
order and conditional cleanup for maybe-initialized elements. Zero-length and Copy-only arrays SHALL still produce explicit complete cleanup facts even when
they emit no runtime release action.

#### Scenario: Plan zero-length cleanup

- **WHEN** a live `Array<Token, 0>` reaches a structured exit
- **THEN** its cleanup fact is complete and contains zero element actions

### Requirement: Writes require exclusive live ownership

Ownership SHALL permit a write only with mutable access to the destination and no conflicting loan or reservation. The incoming complete value SHALL satisfy the destination's unchanged semantic type and lifetime arguments before assignment cleanup or installation. A definitely empty destination SHALL initialize without displaced cleanup; an initialized destination SHALL replace with exactly one displaced cleanup; a maybe-initialized destination SHALL conditionally clean its old value. Restoring every component SHALL reestablish whole-value use. A missing subtree SHALL be restored completely before any deeper projection. Whole replacement of an eligible partial owner SHALL clean its live remainder and install a complete value. Incoming evaluation SHALL NOT be transactional: prior permitted moves or writes remain committed if evaluation fails, while assignment itself performs no cleanup or installation before successful incoming evaluation and validation.

#### Scenario: Replace a move-only element

- **WHEN** a mutable array element is replaced by a complete move-only value
- **THEN** the old element receives one cleanup action and the array remains one complete live owner

#### Scenario: Reject mutation through an immutable root

- **WHEN** a field or index place is structurally valid but its root binding is immutable
- **THEN** ownership rejects the write without changing root liveness or cleanup

#### Scenario: Restore one missing field

- **WHEN** packet.stream was moved and a complete value of its declared type is assigned through mutable access
- **THEN** the stream subtree becomes initialized without displaced-field cleanup and whole packet use resumes only if every other required component is definitely initialized

#### Scenario: Fail during incoming evaluation

- **WHEN** incoming evaluation commits a permitted move from another place and then fails before installing the destination
- **THEN** the move stays committed and exit cleanup follows the resulting place states without installing or prematurely cleaning the destination

### Requirement: Loop ownership is a deterministic fixed point

Ownership SHALL analyze a structured loop until its header state reaches a deterministic fixed point.
Every path that repeats SHALL re-enter with finite per-place initialized, uninitialized, or maybe-initialized facts and independent reachability; every
path that exits SHALL carry the appropriate live owners. Any place read or moved by the next iteration SHALL be definitely initialized on every reaching backedge. Unrelated Boolean histories SHALL NOT recover availability.

#### Scenario: Reassign before continuing

- **WHEN** an iteration moves a mutable binding, assigns a complete replacement, and continues
- **THEN** the next iteration begins with the binding live and initialized

#### Scenario: Reject a conditionally missing owner

- **WHEN** one path moves a non-Copy binding and continues without replacing it while another path retains it
- **THEN** ownership reports the incompatible loop-header state rather than widening it to available

#### Scenario: Repeat a field move

- **WHEN** one iteration moves a field and a reaching backedge does not restore it before the next iteration moves it again
- **THEN** the later move is rejected without enumerating branch histories

### Requirement: Union ownership derives from every normalized member

Ownership analysis SHALL classify a union as Copy only when every normalized member is Copy and
cleanup-free. Otherwise the union SHALL be an affine owner whose complete-value injection, widening,
binding, storage, assignment, call, and return obey ordinary whole-value move rules. A conversion
MUST NOT duplicate or expose an inactive payload. Discriminant-only owned-place refinement SHALL permit eligible partial moves from its known active payload; complete-value conversion still requires full initialization.

#### Scenario: Move a payload into a union

- **WHEN** a move-only `Token` is injected and returned as `Token | End`
- **THEN** ownership transfers the complete `Token` obligation into the returned union and marks the source consumed

#### Scenario: Derive Copy from non-nominal members

- **WHEN** every member of `i32 | Array<i32, 2>` is Copy and cleanup-free
- **THEN** the union is Copy without requiring nominal declarations or user-written conformance

#### Scenario: Widen without duplicating ownership

- **WHEN** a move-only `Token | End` value widens to `Token | End | Fault`
- **THEN** the target receives the single active payload and the source union becomes unavailable

### Requirement: Match modes preserve affine ownership

Ownership SHALL classify a bare match as a Copy read, a consuming match as one complete-value transfer, a shared match as a shared borrow, an exclusive match as an exclusive borrow requiring mutable access, and `match place value` as discriminant-only refinement of an owned place. Place refinement SHALL require a stable initialized discriminant and SHALL NOT implicitly borrow or consume the entire payload. Its pattern projections SHALL designate active owned places, allowing eligible payload-field moves only after arm selection. Guards SHALL NOT commit moves before selection. After joins, payload access SHALL require renewed variant knowledge; partial unions SHALL NOT be matched as complete values. Borrowed patterns SHALL preserve their semantic lifetimes and loans, and SHALL NOT permit holes through references. A consuming match SHALL require a complete eligible scrutinee and transfer the active payload into exactly one selected arm without bypassing user Drop.

#### Scenario: End a shared arm borrow

- **WHEN** a shared arm reads a Copy field and returns a scalar
- **THEN** the borrow ends at the arm boundary and the source owner retains its original cleanup obligation

#### Scenario: Reject a pattern borrow beyond owner validity

- **WHEN** a pattern-derived borrow would remain needed after its referent becomes invalid
- **THEN** ownership reports the invalid escape and publishes no executable match

#### Scenario: Refine without consuming a payload

- **WHEN** match place value selects an active variant and its selected arm moves one eligible field
- **THEN** only that field becomes missing, initialized siblings remain usable, and the variant discriminant remains available for later place refinement

#### Scenario: Reject an escaping pattern borrow

- **WHEN** a pattern-derived reference would be used after its backing owner is invalidated
- **THEN** ownership rejects the escaping value while retaining valid lifetime-bearing stored values

### Requirement: Consuming destructuring cleans exactly one selected payload

For a consuming nominal arm, bound non-Copy fields SHALL become arm-local owners and omitted fields
acknowledged by `..` SHALL remain cleanup obligations. Branch exit, early return, nested control,
and guard failure SHALL release every untransferred active field exactly once in canonical
cleanup order. Inactive union members and the consumed source SHALL receive no cleanup. Fatal traps SHALL preserve the no-unwind contract and MUST NOT promise source cleanup.

#### Scenario: Clean omitted fields

- **WHEN** a consuming `Token` arm returns one moved field and omits another with `..`
- **THEN** only the omitted active field is cleaned in the arm and neither the moved field nor inactive members are released there

#### Scenario: Guard failure preserves the payload

- **WHEN** a consuming guarded arm rejects the active member and a later arm handles the same member
- **THEN** ownership transfers the payload only into the selected later arm without cleaning or duplicating it during the failed guard

### Requirement: Generic ownership is checked once and specialized exactly

Ownership SHALL classify canonical type parameters through the compiler-owned sealed Copy property
and cleanup rules, check lifetime obligations, place moves, initialization, and cleanup once on generic HIR, and substitute that proof
for each concrete instance. A parameter SHALL be Copy only under an explicit `Copy` bound. A
specialization MUST NOT invent structural Copy evidence, duplicate cleanup, or re-check the source
body with concrete-only behavior.

#### Scenario: Propagate an explicit Copy bound

- **WHEN** a generic caller whose parameter is bounded by `Copy` supplies that parameter to another Copy-bounded declaration
- **THEN** the caller's symbolic Copy evidence satisfies the nested call without enumerating concrete types

#### Scenario: Specialize affine and Copy uses

- **WHEN** a checked generic whole-value transfer is instantiated once with `i32` and once with an affine struct
- **THEN** each instance receives the correct concrete copy or cleanup actions from one generic ownership proof

### Requirement: Borrowed-view loans attach to stable owner roots

Every available reference or slice borrow SHALL create a compiler-only loan attached to a stable source place, including its root, projections, shared or exclusive access, ancestry, and required uses and cleanup. Shared loans SHALL coexist; exclusive loans SHALL conflict with overlapping live access. Disjoint fields and proven distinct constant indices SHALL remain independent when neither operation relocates their common owner. Whole-owner loans SHALL overlap every field. A lifetime relationship SHALL NOT grant missing access permission or reinitialize a moved place. Loan identity and access MUST NOT become runtime fields.

#### Scenario: Permit shared aliases

- **WHEN** one call supplies two shared borrows of the same live array root
- **THEN** ownership accepts both loans for the complete invocation

#### Scenario: Reject conflicting call arguments

- **WHEN** one invocation supplies shared and exclusive borrows or two exclusive borrows of overlapping places
- **THEN** ownership rejects the conflict because every argument loan overlaps all later argument evaluation and the complete callee execution

#### Scenario: Reject owner use during a loan

- **WHEN** source attempts to move, replace, mutate, or clean an owner while a conflicting borrowed-view loan is live
- **THEN** ownership diagnoses the owner operation and preserves the original loan and cleanup state

### Requirement: Borrowed-view loans remain lexical and non-escaping

Borrow requirements SHALL follow actual uses, transfer, copies, capture, and cleanup within a finite local control-flow domain. A returned view SHALL retain its source loans beyond its originating call whenever needed. Shared references and slices SHALL be admitted in ordinary structs, unions, fixed arrays, generic wrappers, named tuples, and synthesized aggregates while preserving every nested semantic lifetime. Moving a holder SHALL transfer obligations and Copy SHALL duplicate dependents without detachment. Exclusive stored references, dependent user Drop, borrowed Effect outcomes, and suspension with partial owners SHALL remain rejected until their respective storage, cleanup, outcome, and frame proofs are admitted. Lexically valid callable and Effect captures SHALL retain environment bounds immediately. No borrow SHALL outlive its referent or lose reborrow ancestry through abstraction.

#### Scenario: End a temporary loan after an ordinary call

- **WHEN** an exclusive whole-array borrow is passed to an ordinary function which returns without retaining a child dependent
- **THEN** the call loan ends and subsequent caller access to the mutable owner is permitted

#### Scenario: Preserve recursive storage of a shared slice

- **WHEN** a shared slice type appears directly or transitively inside an owned struct, union, array, or generic application
- **THEN** ownership retains the nested lifetime and accepts uses fitting source validity; an escape beyond that validity is rejected

#### Scenario: Reject a captured slice

- **WHEN** a lazy computation or callback would retain a borrowed view beyond its source root
- **THEN** ownership rejects the escape rather than ending the source loan prematurely

#### Scenario: Store a lexical borrow locally

- **WHEN** a local binding stores `&values` and is used only within the owner's lifetime
- **THEN** ownership ends the loan at the local view's last use and restores compatible owner access

#### Scenario: Reject recursive storage of a slice

- **WHEN** a shared slice is stored in a nested aggregate which escapes beyond its backing source validity
- **THEN** ownership rejects that escape and reports the nested retaining path

### Requirement: Returned views preserve source provenance through their live range

Ownership facts SHALL identify all actual source places required by every accepted returned view and its declared lifetime relationship and carry
that provenance through assignments and compatible reborrows. While a shared returned view is live,
every overlapping source owner MUST NOT be mutated, moved, or dropped. While an exclusive returned view is live, every overlapping source owner
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

#### Scenario: Retain a shared child of exclusive access

- **WHEN** an exclusive loan is reborrowed shared and that child is copied into another live dependent
- **THEN** the parent remains restricted until both the child and every retained dependent end

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

#### Scenario: Reject an unverified partial suspension

- **WHEN** a partial owner would remain live across a potentially suspending child call
- **THEN** analysis rejects the suspension until frame initialization flags and remainder cleanup are supported; lifetime-bearing complete values retain their existing stable-location requirements

### Requirement: Nominal union ownership follows nominal struct rules

A union value SHALL be affine by default. `Copy` and `Drop` implementations, generic Copy bounds,
moves, borrows, writes, refined active-payload partial moves, and implementation admissibility SHALL follow the
same rules as nominal structs across every variant payload. The compiler MUST NOT infer Copy merely
because all currently reachable payload fields are Copy.

#### Scenario: Require an explicit Copy implementation

- **WHEN** every field of every variant is Copy but the union declares no valid `impl Copy`
- **THEN** reading the union as a whole consumes it under ordinary affine ownership

#### Scenario: Validate Copy across every variant

- **WHEN** a union requests `Copy` and one variant contains an affine field under the declared bounds
- **THEN** conformance is rejected at that field even when another variant is unit

### Requirement: Tuple-backed and anonymous aggregates obey ordinary struct ownership

Named tuples and anonymous aggregate values SHALL use the ordinary nominal struct
ownership rules. Reads, moves, borrows, partial moves, Copy evidence, mutation, and
structured-exit cleanup MUST NOT depend on whether the nominal declaration was written in source or
synthesized from a literal. Anonymous aggregate creation MUST NOT synthesize `Copy` evidence merely
because every current member is Copy.

Tuple cleanup order SHALL follow ordinal declaration order, and anonymous record cleanup order SHALL
follow the canonical source field order recorded by its synthesized declaration. A move of the
whole aggregate SHALL transfer exactly one recursive cleanup obligation; separate initialized fields or positions SHALL be independently movable under the same visibility, loan, and Drop boundaries as source-authored structs.

#### Scenario: Move one anonymous record as a whole

- **WHEN** a local anonymous record is moved into an owning generic call
- **THEN** the source binding becomes dead and the callee receives its one declaration-ordered cleanup obligation

#### Scenario: Move an initialized positional field

- **WHEN** source requests a consuming move from one position of an affine named tuple
- **THEN** ownership transfers the eligible tuple field and retains initialization and cleanup for its disjoint siblings

#### Scenario: Avoid implicit Copy for anonymous aggregates

- **WHEN** every field of an anonymous record is Copy but no nominal Copy evidence can be declared for its generated type
- **THEN** the record remains affine while non-consuming reads of its Copy fields follow ordinary struct rules

#### Scenario: Refuse a positional partial move

- **WHEN** a tuple position is moved through a reference or enclosing user Drop boundary
- **THEN** ownership rejects the hole under the same rule as an ordinary struct field

### Requirement: Ownership facts are produced once per residual specialization

Generic ownership and lifetime facts SHALL first be checked once under the declaration's abstract assumptions. After private residual and cleanup-call candidate closure is complete, separately attributable residual ownership validation SHALL consume those checked facts and resolved representation/static inputs once for each residual runtime HIR specialization and SHALL publish
one immutable ownership fact for that specialization: its runtime bindings with their ownership
category and live range over source spans, and a closed verdict. Static parameters, static local
bindings, static-function locals, inactive static arms, and `StaticEvaluation` storage MUST NOT appear
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

#### Scenario: Separate source checking from residual validation

- **WHEN** multiple runtime instances share an unchanged generic body
- **THEN** the generic lifetime and ownership query executes once in its semantic context while each necessary residual check exposes separate inputs and work

## ADDED Requirements

### Requirement: Incomplete values remain local place states

An incomplete owner SHALL NOT be passed, returned, captured, copied, borrowed as a whole, or used for an ordinary receiver call until complete. Independently initialized projections SHALL remain usable. The checker MUST NOT inspect a callee body to waive receiver completeness, introduce public Partial types, or reconstruct arbitrary Boolean histories. `drop value` SHALL perform explicit place cleanup of the live remainder, terminate ownership, and reject subsequent use or cleanup.

#### Scenario: Reject a complete receiver call

- **WHEN** a partial packet calls an operation taking &Self even when that operation reads only an initialized code field
- **THEN** ownership rejects the whole-receiver borrow while allowing an explicit borrow of packet.code

#### Scenario: Clean a partial place explicitly

- **WHEN** drop packet executes after one eligible field has been extracted
- **THEN** only packet's initialized remainder is cleaned once and a second drop packet is rejected
