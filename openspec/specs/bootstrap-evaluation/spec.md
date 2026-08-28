# Bootstrap Evaluation Specification

## Purpose

Evaluate the first closed, semantically checked Silk program to an exact `i32` result while retaining a deterministic explanation of every call and value binding.
## Requirements
### Requirement: First bootstrap entry point

Bootstrap evaluation SHALL execute the snapshot's lowered MIR program from the entry that instance
discovery resolved. An ordinary `main() -> ()` SHALL complete with status zero, an ordinary
`main() -> i32` SHALL retain its exact completed value, and an effectful
`main() -> () ! E` SHALL be constructed and run once by the lowered entry adapter, producing
either completed status `0` or deterministic structured unhandled-failure termination data
retaining status one, canonical identity, provenance, logical path, and causal history. When discovery reports an unavailable entry,
evaluation SHALL return a closed `Blocked` outcome carrying that explicit entry reason rather than
throw, fail, or choose a declaration.

#### Scenario: Select one ordinary main

- **WHEN** discovery resolves exactly one zero-parameter ordinary `main` declaring `i32`
- **THEN** evaluation enters that instance's lowered function and preserves its exact result

#### Scenario: Select one effectful main

- **WHEN** discovery resolves exactly one zero-parameter effectful `main` succeeding with `()`
- **THEN** evaluation enters the generated adapter and runs the entry Effect exactly once

#### Scenario: Retain an unhandled entry failure

- **WHEN** effectful `main` fails with a concrete detached owned failure
- **THEN** evaluation returns deterministic termination data naming status one, its private tag, canonical identity, provenance, logical path, and causal history

#### Scenario: Block a missing main

- **WHEN** no top-level declaration is named `main`
- **THEN** evaluation is blocked with the missing-entry reason and does not select another function

#### Scenario: Block ambiguous main declarations

- **WHEN** multiple top-level declarations are named `main`
- **THEN** evaluation is blocked with the ambiguous-entry reason and does not choose the first

#### Scenario: Block a parameterized main

- **WHEN** the unique `main` declaration has one or more parameters
- **THEN** evaluation is blocked with the parameterized-entry reason

### Requirement: Evaluate the closed bootstrap expression slice

Evaluation SHALL interpret the lowered MIR control-flow graph: literals define locals, moves copy
them, calls evaluate their already-computed argument locals bound positionally to the target's
parameter locals, drops execute as explicit no-release events for the copyable slice, and each
function returns its terminator's value, publishing the exact `i32` result on completion.
Unavailable facts and ownership violations reach evaluation as the explicit generated traps
lowering inserted; executing a trap SHALL produce a top-level `Trap` outcome carrying its
classification, reason, provenance, and available logical path rather than a guessed value.
Functions absent from the
lowered program MUST NOT affect the outcome.

#### Scenario: Evaluate a literal main

- **WHEN** `main` returns the available literal `42`
- **THEN** evaluation completes with exact `i32` result `42`

#### Scenario: Evaluate through one parameter

- **WHEN** `identity(value: i32)` returns `value` and `main` returns the compatible call `identity(42)`
- **THEN** argument `42` binds to `identity`'s parameter local and evaluation completes with `42`

#### Scenario: Evaluate two positional bindings

- **WHEN** `main` makes a compatible call to a two-parameter function whose return references its second parameter
- **THEN** the second argument binds to parameter one and becomes the completed result

#### Scenario: Evaluate one nested call

- **WHEN** `main` returns the compatible expression `identity(identity(42))`
- **THEN** the inner call completes with `42`, that value binds to the outer parameter, and evaluation completes with `42`

#### Scenario: Evaluate nested siblings left to right

- **WHEN** a compatible call contains two nested call arguments
- **THEN** the first nested expression completes before the second begins and both completed values are then bound to their matching outer parameters

#### Scenario: Trap at a lowered trap

- **WHEN** a reachable function's body was unavailable and lowered to a generated trap
- **THEN** evaluation returns a fatal-trap outcome with its function identity, reason, and causative span

#### Scenario: Block at an inner unavailable fact

- **WHEN** a body contains an unavailable fact anywhere in its returned expression
- **THEN** lowering has already turned that body into a generated trap and evaluation terminates there before executing any enclosing target

#### Scenario: Block wrong call arity

- **WHEN** a call contract at any nesting depth is an arity mismatch
- **THEN** the enclosing body's HIR is unavailable, its lowered function traps, and evaluation returns that trap's provenance

#### Scenario: Ignore an unreachable broken function

- **WHEN** the program contains a valid reachable path from `main` and a different unreachable function has unavailable semantic facts
- **THEN** evaluation completes from the reachable path without executing the unrelated function

#### Scenario: Evaluate a binding program

- **WHEN** `main` binds `let value = identity(42)` and returns `value`
- **THEN** the call completes into the binding's local, its drop executes at the exit, and evaluation completes with `42`

#### Scenario: Trap a use-after-move program at its trap

- **WHEN** a reachable function's ownership verdict is a violation
- **THEN** its lowered function is a generated trap and evaluation returns fatal termination with the violation's provenance

### Requirement: Recursive calls execute in distinct activation frames

Evaluation SHALL permit direct and mutual recursive calls by creating one independent activation
frame per invocation. Parameters, locals, borrows, cleanup, returns, and trace events MUST belong to
the correct frame even when several active frames share one canonical function identity.

#### Scenario: Complete terminating recursion

- **WHEN** a recursive function reduces its input to a base case and returns through several active invocations
- **THEN** evaluation completes with the same result and caller-visible mutations as native and WebAssembly execution

#### Scenario: Keep recursive mutable slices distinct

- **WHEN** quicksort recursively passes one mutable slice with different low and high bounds
- **THEN** every activation observes its own scalar bounds while mutations remain visible through the shared slice

### Requirement: Evaluation limits are deterministic blocked data

Evaluation SHALL accept positive maximum-step and maximum-call-depth limits with stable defaults.
Each executed MIR operation consumes one step and each active source-logical invocation consumes one
depth unit, including an invocation retained as a suspended heap continuation. Compiler-generated
driver, resume, and storage helpers MUST NOT consume additional source-logical depth units.
Exhaustion SHALL produce an `EvaluationLimit` blocked outcome naming `Steps` or `CallDepth`, the
configured limit, the active function, the source span that attempted further work, and the complete
active source-logical call identities. Evaluation MUST NOT depend on JavaScript stack overflow or
wall-clock time.

#### Scenario: Bound direct non-termination

- **WHEN** a function recursively calls itself without reaching a base case
- **THEN** evaluation blocks at the configured call-depth limit with a deterministic active call path

#### Scenario: Bound suspended logical recursion

- **WHEN** an Effect crosses suspension boundaries until another source-logical invocation would exceed `maxCallDepth`
- **THEN** evaluation blocks with `CallDepth` at that exact boundary and reports every retained suspended invocation in the active call path

#### Scenario: Bound an infinite loop

- **WHEN** a reachable loop executes beyond the configured step limit
- **THEN** evaluation blocks at the next operation with the `Steps` limit and exact operation span

#### Scenario: Repeat a limited evaluation

- **WHEN** an equivalent program is evaluated repeatedly under equal limits
- **THEN** its blocked reason, active frames, source provenance, and trace are identical

### Requirement: Evaluation trace is deterministic data

Every evaluation outcome SHALL retain an ordered trace of the reachable work performed before
completion or blockage. Trace events SHALL identify entry, function calls, positional bindings,
and function returns with canonical MIR function identities, argument and parameter ordinals,
exact values, and the provenance of the lowered operations they replay. A call event SHALL be
recorded when its call operation executes, after its argument locals were computed in
left-to-right evaluation order, and binding events SHALL carry whether their argument value came
from a nested call. Equivalent programs SHALL produce equivalent outcomes and traces without
depending on object identity, wall-clock time, random state, I/O, or process-global state.

#### Scenario: Trace the identity program

- **WHEN** `main` evaluates `identity(42)` successfully
- **THEN** the trace records entering `main`, calling `identity`, binding `42` to parameter zero, returning `42` from `identity`, and returning from `main` in order

#### Scenario: Trace one nested identity program

- **WHEN** `main` evaluates `identity(identity(42))` successfully
- **THEN** the trace records the inner call and its return before the outer call's binding, with distinct call-site provenance and the outer binding marked as coming from a nested call

#### Scenario: Trace nested siblings deterministically

- **WHEN** two nested argument expressions both complete
- **THEN** all events for the first argument precede all events for the second and both precede the enclosing call's binding events

#### Scenario: Retain a partial blocked trace

- **WHEN** evaluation reaches a trap after completing earlier nested calls
- **THEN** the trap outcome retains the ordered successful events preceding the trap without a binding or return that did not occur

#### Scenario: Repeat successful evaluation

- **WHEN** equivalent valid programs are analyzed and evaluated repeatedly in fresh processes
- **THEN** their exact result, event kinds, identities, values, provenance, and ordering are identical

### Requirement: Arithmetic evaluates exactly and traps on the pinned conditions

The evaluator SHALL execute every admitted integer width and mode without host-number precision loss. Checked overflow, invalid division/remainder, and invalid shift counts SHALL block with operation identity, reason, and provenance; wrapping, saturating, bitwise, comparison, and conversion behavior SHALL match both backends.

#### Scenario: Evaluate wide arithmetic

- **WHEN** `u64` uses values above JavaScript's exact integer range
- **THEN** evaluation returns the exact result or pinned trap without rounding

#### Scenario: Evaluate checked recovery

- **WHEN** a recoverable checked operation overflows
- **THEN** evaluation constructs `None` rather than trapping

### Requirement: Conditionals evaluate exactly one arm

The interpreter SHALL execute user-authored branches: a true condition takes the taken block, a
false condition the otherwise path, and exactly one arm's operations execute per traversal.
Boolean values SHALL be exact (`false` is zero, `true` is one) and comparison operations SHALL
produce them without trapping. Interpreter and native execution SHALL agree arm by arm across
the corpus, including programs whose two arms produce different results.

#### Scenario: Take the true arm

- **WHEN** `main` returns `1` under `if i32.equals(1, 1)` and `0` otherwise
- **THEN** evaluation completes with `1` and the trace shows only the taken path's work

#### Scenario: Take the otherwise path

- **WHEN** the condition compares unequal values
- **THEN** evaluation completes with the fall-through result and the taken arm's operations never execute

### Requirement: Evaluation consumes the MIR layout plan

Evaluation SHALL accept the target-aware MIR program produced by the snapshot and SHALL treat its
canonical target and verified layout table as program facts. The interpreter MUST NOT derive,
default, or accept a second representation plan. Structured scalar values remain logical interpreter
values; the layout table does not require simulation of raw bytes when no operation observes them.

#### Scenario: Evaluate with the shared scalar plan

- **WHEN** a branching program is evaluated from a snapshot using the canonical `i32` and `bool` entries
- **THEN** evaluation uses that MIR program and completes with its logical result without creating an interpreter-specific layout

#### Scenario: Block malformed target-aware MIR before execution

- **WHEN** a MIR program omits the layout of a runtime type used by an operation
- **THEN** MIR verification reports the inconsistency and the interpreter does not execute the malformed program

### Requirement: Evaluation carries immutable nominal values

The MIR evaluator SHALL represent a nominal struct value by its canonical type and complete
declaration-ordered field values. Construction SHALL evaluate field operands exactly once in MIR
operand order and create no partial value. Whole-value moves, parameter binding, calls, and returns
SHALL preserve canonical nominal identity and value contents.

#### Scenario: Evaluate a factory result

- **WHEN** a factory constructs and returns `Pair { left: 1, right: 2 }`
- **THEN** evaluation produces one canonical `Pair` value containing declaration-ordered field values `1` and `2`

#### Scenario: Pass a nested aggregate through a call

- **WHEN** a complete nested struct is moved through an internal function and returned
- **THEN** evaluation preserves every nested nominal identity and field value without aliasing a partial source

### Requirement: Evaluation projects exact field values

An aggregate projection SHALL read the canonical field identified by MIR and return its stored value
with the declared result type. Chained projections SHALL evaluate left-to-right. A structurally
invalid projection SHALL be rejected by MIR verification rather than guessed by the evaluator.

#### Scenario: Evaluate a chained scalar projection

- **WHEN** `main` returns `token.span.start`
- **THEN** evaluation follows both canonical fields and returns the exact stored scalar

### Requirement: Aggregate traces are deterministic and bounded

Evaluation traces SHALL identify aggregate construction, whole-value movement across calls and
returns, field projection, and cleanup using canonical types, field identities, source provenance,
and compact deterministic value summaries. Trace ordering MUST NOT depend on object identity,
backend representation, physical address, or hash iteration.

#### Scenario: Repeat an aggregate trace

- **WHEN** the same construction-call-projection program is evaluated repeatedly
- **THEN** its event order, canonical identities, field summaries, values, and encoded trace are identical

### Requirement: Evaluation consumes aggregate calling shapes

Before executing a nominal call or return, evaluation SHALL verify that the logical aggregate value
matches the target plan's compiler-selected calling shape. It MUST NOT invent a flattening or
continue with a missing lane. The completed program result remains the fixed scalar bootstrap entry
result.

#### Scenario: Evaluate a flattened internal result

- **WHEN** an internal function returns a struct whose calling shape has multiple scalar lanes
- **THEN** evaluation transfers all lanes according to the selected field paths and the caller observes the original logical nominal value

### Requirement: Evaluation carries immutable complete array values

The evaluator SHALL represent an array by its canonical type and immutable ascending-index element
values. Construction SHALL evaluate each operand exactly once in MIR order; whole moves, parameter
binding, calls, returns, and cleanup SHALL preserve the complete logical value without exposing lane
realization.

#### Scenario: Evaluate a nested array call

- **WHEN** a complete nested array passes through an internal function and returns
- **THEN** every canonical length and element value is preserved without aliasing a partial source

### Requirement: Evaluation checks every dynamic index

Evaluation SHALL compare a dynamic `i32` index against zero and the canonical length before reading
the selected element or continuing a place chain. Failure SHALL produce a deterministic trap with the
index, length, function identity, and exact projection provenance.

#### Scenario: Trap a negative index

- **WHEN** execution indexes an array with `-1`
- **THEN** evaluation blocks at that index operation without reading an element

#### Scenario: Trace a successful indexed field read

- **WHEN** `pairs[index].left` completes
- **THEN** the trace identifies the canonical array, selected index, canonical field, resulting value, and source order

### Requirement: Evaluation executes writes as complete replacement

Evaluation SHALL execute `WritePlace` by checking selectors, evaluating the right-hand value, applying
replacement cleanup, and committing one complete logical root value in the specified order. Logical
struct and array values SHALL remain immutable snapshots; a successful write SHALL publish a new root
value rather than expose backend storage or aliases.

#### Scenario: Evaluate an indexed increment

- **WHEN** a loop assigns `values[index] = values[index] + 1`
- **THEN** evaluation changes only the selected logical element and later reads observe the new complete array

### Requirement: Evaluation executes the structured control DAG directly

Evaluation SHALL traverse ordered DAG regions and implement loop repetition from the explicit loop
region's condition, repeat, and exit outcomes. It MUST NOT first flatten the program into a cyclic CFG.
Condition, transfer, write, cleanup, and trap events SHALL remain compact deterministic data with
canonical region and source provenance.

#### Scenario: Evaluate continue and break

- **WHEN** a loop continues for early elements and breaks on a later element
- **THEN** evaluation follows the canonical repeat and exit outcomes and reports the exact iteration order

#### Scenario: Trap before an out-of-bounds write

- **WHEN** a loop attempts a dynamic array write outside its canonical length
- **THEN** evaluation traps at that selector before evaluating or committing the right-hand replacement

### Requirement: Evaluation carries immutable tagged union values

Evaluation SHALL represent a union as one immutable logical value containing its canonical union
type, active ordinary member identity, and complete member payload. Injection SHALL install the
source member, widening SHALL remap that member into the target union without changing the payload,
and calls, returns, aggregate storage, moves, and writes SHALL preserve the same active identity.

#### Scenario: Evaluate injection and widening

- **WHEN** an `i32` is injected into `i32 | Token` and widened to `i32 | Token | Fault`
- **THEN** evaluation retains the complete scalar payload under the canonical wider type

#### Scenario: Evaluate represented executable members

- **WHEN** an exact callable or opaque Effect value is injected, stored, projected, and invoked or run
- **THEN** evaluation preserves its exact finite representation and produces the same result as the unwrapped value

#### Scenario: Evaluate a union inside an array

- **WHEN** a fixed array stores values contextually injected into one ordinary union element type
- **THEN** each element retains its own active member and complete immutable payload

### Requirement: Evaluation cleans only the active union payload

Evaluation SHALL execute union cleanup from the canonical active member and ownership plan, releasing
that payload exactly once and performing no inactive-member cleanup. Trace events for injection,
widening, transport, replacement, and cleanup SHALL use canonical type/member identities and exact
source provenance without exposing numeric tags as source values.

#### Scenario: Trace replacement cleanup

- **WHEN** a mutable union containing `Token` is replaced by one containing `End`
- **THEN** the trace records one `Token` cleanup before one committed replacement and no `End` cleanup for the old value

### Requirement: Evaluation dispatches matches by logical active member

Evaluation SHALL execute the match scrutinee exactly once, select arms in source order from the
logical active nominal member, evaluate matching guards in order, bind fields under the verified
access mode, and evaluate exactly one selected result. It MUST NOT inspect physical storage or
derive a different member mapping from backend tags.

#### Scenario: Fall through a rejected guard

- **WHEN** the active member matches a guarded arm whose guard is false and a later unguarded arm for that member exists
- **THEN** evaluation records the failed guard and evaluates only the later arm result

#### Scenario: Select a universal fallback

- **WHEN** no preceding nominal arm accepts the active member and `_` remains
- **THEN** evaluation selects the universal arm without changing the logical payload identity

### Requirement: Match traces preserve bindings and cleanup deterministically

Evaluation traces SHALL identify match entry, active canonical member, each attempted arm, guard
outcome, selected arm, pattern bindings, result, borrow end or ownership transfer, and active-field
cleanup with exact source provenance. Inactive members and unreachable arms SHALL produce no binding
or cleanup events.

#### Scenario: Trace consuming omitted-field cleanup

- **WHEN** a consuming arm moves one bound field and omits another
- **THEN** the trace records one payload transfer, the selected binding, exact omitted-field cleanup, and joined result in execution order

#### Scenario: Repeat a borrowed match trace

- **WHEN** the same shared match is evaluated repeatedly
- **THEN** its arm attempts, guard results, bindings, borrow end, result, and provenance are identical

### Requirement: Evaluation executes concrete specializations only

The evaluator SHALL execute generic-origin functions and nominal values solely through their
concrete MIR types, layouts, and instance identities. It MUST NOT introduce interpreter-owned type
arguments, runtime dictionaries, or alternate generic layout decisions.

#### Scenario: Evaluate two identity instances
- **WHEN** one program calls concrete i32 and nominal-struct specializations
- **THEN** evaluation preserves each concrete value and traces the two canonical instance identities

### Requirement: Evaluation preserves borrowed backing identity

Logical evaluation SHALL realize a slice as a view of one stable caller-owned storage place with a
base position and runtime length, not as a copied array value. Shared reads and exclusive writes
SHALL therefore observe the same backing state across nested ordinary calls while access mode and
loan identity remain compiler facts rather than runtime payload. Logical slices SHALL retain the
complete selector path from their backing cell to a nested fixed array. Hidden temporary cells
SHALL remain live until their final derived loan ends.

#### Scenario: Observe exclusive mutation in the caller

- **WHEN** a caller passes `&mut values` to a helper that replaces an indexed element and then reads `values` after return
- **THEN** logical evaluation observes the helper's replacement in the original caller-owned array

#### Scenario: Read two source lengths through one callee

- **WHEN** one evaluated slice function receives arrays of two different lengths
- **THEN** each invocation traverses exactly its runtime logical length without copying or specializing the callee by length

#### Scenario: Mutate a runtime selected inner array

- **WHEN** evaluation runs `edit(&mut matrix[index])`
- **THEN** the checked inner array in `matrix` changes and no copied temporary receives the write

### Requirement: Slice evaluation preserves checked-place ordering

Evaluation SHALL check a slice index against its own runtime length before reading, projecting, or
evaluating an assignment replacement. Valid exclusive replacement SHALL update the authoritative
backing place and clean the displaced value exactly once.

#### Scenario: Trap without evaluating a replacement

- **WHEN** an exclusive slice write is out of bounds and its replacement would otherwise produce an observable trace event
- **THEN** evaluation traps at the bounds check and records no replacement event or write

### Requirement: Evaluation preserves exact usize semantics

The evaluator SHALL represent every integer with one canonical tagged value containing its scalar
spelling and an exact `bigint` payload. Entry-point `i32` requirements, raw-buffer `usize` indexes,
and other width-specific contracts SHALL be validated through that scalar spelling rather than
through distinct runtime variants. Target-selected `usize` SHALL use canonical unsigned decimal
encoding.

#### Scenario: Integer values retain their scalar identity
- **WHEN** one evaluated program binds both `i32` and `usize` integer values
- **THEN** both values use the same integer discriminator
- **AND** each value records its own canonical scalar spelling and exact `bigint` payload

#### Scenario: Evaluate native maximum

- **WHEN** native `usize` evaluates its maximum value
- **THEN** evaluation returns `18446744073709551615` exactly

### Requirement: Evaluation is the flow and failure oracle

Evaluation SHALL distinguish construction from execution, represent success and owned nominal
failure explicitly, run one layer, recover exact members, propagate unmatched members, and record
deterministic ordered flow/failure/cleanup events. Traps SHALL remain separate fatal outcomes.

#### Scenario: Compare lazy success and recovery

- **WHEN** one fixture first succeeds and then recovers its declared failure
- **THEN** traces show no body event before run and both executions produce the specified result with exact event order

### Requirement: Evaluation is the Effect and allocation oracle

The evaluator SHALL model lazy construction, capture persistence, one-shot rejection, retry attempts,
provider acquisition, allocation identity, initialized slots, Vector state, explicit drop, Drop order,
and deterministic allocation failure without relying on JavaScript garbage collection or object
identity.

#### Scenario: Sweep every allocation failure

- **WHEN** a deterministic allocator fails each allocation ordinal of a Vector-building Effect in turn
- **THEN** evaluation preserves the original vector or drops every committed owner exactly once and can run a fresh program afterward

### Requirement: Evaluation executes callable values exactly

The evaluator SHALL construct monomorphic callable environments, preserve capture identity and
ownership, enforce shared, exclusive, and consuming invocation modes, invoke direct and stored
callables, and drop unconsumed environments exactly as specified by MIR. Callable trace events
SHALL be deterministic, bounded, and independent of JavaScript closure identity or garbage
collection. Evaluation SHALL preserve successive section capture order independently of original
parameter order and SHALL end non-escaping reusable capture loans after their last statically known
invocation.

#### Scenario: Reuse an exclusive callable sequentially

- **WHEN** a `mut fn` callback updates captured state across two legal invocations
- **THEN** evaluation returns both results in order and retains the mutation between calls

#### Scenario: Reject an already consumed callable

- **WHEN** a take-once callable is invoked after its owned capture was consumed
- **THEN** evaluation exposes the phase-owned rejection rather than duplicating or fabricating the capture

#### Scenario: Evaluate a staged section

- **WHEN** evaluation runs `combine(3)(2)(1)`
- **THEN** it invokes `combine(1, 2, 3)` after evaluating each supplied value once

### Requirement: Evaluation distinguishes run grouping

The evaluator SHALL execute an ungrouped pipeline inside the operand of `run` and a pipeline outside
a grouped run over the resulting success value. Both forms SHALL preserve their distinct trace
order and one-layer execution behavior.

#### Scenario: Compare grouped and ungrouped run

- **WHEN** one program spells `run effect |> Effect.map(transform)` and another spells `(run effect) |> transform`
- **THEN** evaluation shows composition-before-execution for the first and value-transformation-after-execution for the second

### Requirement: Evaluation is the deterministic allocation oracle

The evaluator SHALL execute compiler-planned allocation, logical addresses, reclaim tickets,
RawBuffer storage, Slot operations, shared bounds-checked recursively Copy reads including
structural unions, initialization events, restricted hooks, explicit drop, and automatic cleanup
without relying on JavaScript object identity or garbage collection. A shared union read SHALL
return the same canonical active member and complete payload without mutating the buffer, owner,
initializedness, or cleanup state. Evaluation SHALL support deterministic failure at each requested
allocation ordinal, create no owner for a rejected request, preserve self-contained owners after
provider access ends, and expose bounded deterministic events for acquisition, initialization,
copy, destruction, and release.

#### Scenario: Sweep allocation exhaustion

- **WHEN** the same construction program fails each allocation ordinal in turn
- **THEN** every run returns `OutOfMemoryError`, releases each successfully acquired owner exactly once, and permits a subsequent successful run in the same evaluator

#### Scenario: Drop after provision ends

- **WHEN** evaluation ends the exclusive allocator provider access before dropping the returned Allocation
- **THEN** release succeeds through the allocation's active ticket without looking up the current provider

#### Scenario: Observe hook-before-release order

- **WHEN** a guard owns initialized move-only elements and its backing allocation
- **THEN** the trace records element destruction by the hook before recursive field cleanup releases the bytes

#### Scenario: Evaluate a structural-union shared read without mutation

- **WHEN** evaluation reads the same initialized all-Copy union through two shared raw-buffer aliases
- **THEN** both results retain the stored active member and payload and the later destruction and release trace is identical to a run with no reads

### Requirement: Evaluation is bit-aware for floats

Evaluation SHALL store explicit float width and IEEE bits, round after every `f32` operation, preserve signed zero, canonicalize arithmetic NaNs where payload is unspecified, and implement classification, total order, reinterpretation, and conversions deterministically.

#### Scenario: Round an f32 operation

- **WHEN** an `f32` arithmetic result needs binary32 rounding
- **THEN** evaluation rounds once at that operation and matches both backends

#### Scenario: Preserve fromBits-toBits

- **WHEN** a float is created from same-width integer bits and reinterpreted back
- **THEN** evaluation returns the original bits exactly

### Requirement: Evaluation exposes exact immutable static bytes

Evaluation SHALL model static bytes and views without allocation, mutation, or host-string identity and SHALL encode their events deterministically.

#### Scenario: Read a UTF-8 view

- **WHEN** evaluation observes a non-ASCII text literal's byte view
- **THEN** it returns the exact UTF-8 bytes and `usize` length

### Requirement: Evaluation records standard-stream writes

With an explicit provider, evaluation SHALL record complete ordered byte events and typed failures without ambient host streams. Repeated evaluation with the same provider behavior SHALL be deterministic.

#### Scenario: Capture several writes

- **WHEN** a program writes a heading and two rows
- **THEN** evaluation records exactly those three byte events in order

### Requirement: Evaluation indexes immutable static bytes

Evaluation SHALL resolve an indexed static view against its compiler-owned bytes, check the runtime
index against the view length, and return the selected `u8` without converting the view into an
array, allocation, or host string.

#### Scenario: Evaluate an indexed byte literal

- **WHEN** a program reads every byte of a static byte literal inside a loop
- **THEN** evaluation returns the exact decoded values and records no allocation event

#### Scenario: Trap an out-of-bounds static read

- **WHEN** the runtime index equals or exceeds the static view length
- **THEN** evaluation returns the ordinary indexed-read fatal trap and the indexing span

### Requirement: Evaluation applies the canonical transcendental contract

Evaluation SHALL compute sine and cosine from explicit input width and bits, apply the canonical
range reduction and approximation, round to the operation width, and publish the specified result
bits. It MUST NOT delegate observable semantics to ambient JavaScript `Math` behavior.

#### Scenario: Round f32 sine once

- **WHEN** an `f32.sin` result lies between adjacent binary32 values
- **THEN** evaluation applies the canonical operation sequence and publishes the specified binary32 bits

#### Scenario: Repeat f64 cosine

- **WHEN** an equivalent `f64.cos` program is evaluated repeatedly
- **THEN** its result bits and evaluation trace are identical

### Requirement: Evaluation receives OS behavior through an injected adapter

The evaluator SHALL access operating-system file and directory operations only through an explicit
host adapter supplied with the evaluation request. The adapter SHALL implement the normalized
handle protocol and stable reason mapping. Compiler core initialization MUST NOT import or construct
a process filesystem implementation by default.

#### Scenario: Block a missing OS adapter

- **WHEN** evaluation reaches a supported OS intrinsic without an injected host adapter
- **THEN** evaluation returns deterministic blocked data identifying the unavailable host capability rather than touching ambient process APIs

#### Scenario: Preserve normalized adapter outcomes

- **WHEN** an injected host operation reports a low-level reason and native code
- **THEN** evaluation exposes those exact protocol outputs to ordinary `OsFileSystem` source

#### Scenario: Load the evaluator in a browser-capable bundle

- **WHEN** a browser consumer imports compiler and evaluator core modules without configuring OS support
- **THEN** module loading requires no Node filesystem module or equivalent ambient host API

### Requirement: Evaluation preserves first-class string semantics

Evaluation SHALL model `string` as valid immutable text with storage provenance, byte length, and
lexical lifetime distinct from a byte slice. It SHALL agree with emitted targets on ordinary
references to string values, explicit byte viewing, `char` traversal, exact equality, safe
validation results, owned-string views, checked scalar conversion, and loan endings without using
host-string identity as observable semantics.

#### Scenario: Compare exact strings

- **WHEN** evaluation compares equal UTF-8 sequences and then compares canonically equivalent but byte-distinct sequences
- **THEN** it reports equality only for the exact sequence pair without normalization

#### Scenario: Validate a runtime byte view

- **WHEN** stdlib validation receives valid and invalid runtime byte views
- **THEN** evaluation returns the borrowing `string` for the valid input and the typed invalid-UTF-8 value for the invalid input

#### Scenario: Traverse a non-ASCII scalar

- **WHEN** evaluation traverses a valid multi-byte UTF-8 sequence
- **THEN** it produces the exact `char` and next byte offset through checked scalar conversion

#### Scenario: Reject an invalid scalar conversion

- **WHEN** evaluation checks a surrogate or a value above `0x10ffff`
- **THEN** it returns `None` without a trap or truncated character

#### Scenario: Keep invalid unchecked construction outside safe guarantees

- **WHEN** unsafe source forms `string` from malformed UTF-8
- **THEN** the program has violated the unsafe operation contract and evaluation does not publish a recoverable validation result

### Requirement: Standard input is an injected evaluator host

Evaluation SHALL accept an explicit standard-input provider, separate from the OS filesystem host,
exposing one capacity-bounded read that returns committed bytes or a host failure. The evaluator
MUST NOT import an ambient process input implementation into browser-capable compiler cores, and
MUST NOT commit more bytes than the caller's buffer holds.

#### Scenario: Evaluate against a scripted provider

- **WHEN** a program reads with an injected provider holding scripted bytes
- **THEN** each read commits the provider's chosen prefix, leaves later buffer bytes unchanged, and reports the exact count

#### Scenario: Block a reachable read without a host

- **WHEN** evaluation reaches a standard-input read and no provider was injected
- **THEN** it reports a blocked outcome naming the missing host rather than inventing empty input

### Requirement: Child-process execution is an injected evaluator host

Evaluation SHALL accept an explicit child-process provider, separate from the OS filesystem and
standard-input hosts, taking one structured request of program, ordered arguments, environment
entries, and an optional working directory, and returning an exit, a signal, or a host failure. The
evaluator SHALL split the low-level NUL-terminated entry blocks into entries before calling the
provider, and MUST NOT import an ambient process implementation into browser-capable compiler cores
or run a real program.

#### Scenario: Evaluate against a scripted provider

- **WHEN** a program executes with an injected provider holding a scripted outcome
- **THEN** the evaluation observes that outcome's termination and captured bytes, and the provider observes the request's exact program, argument, and environment bytes

#### Scenario: Block a reachable execution without a host

- **WHEN** evaluation reaches a child-process execution and no provider was injected
- **THEN** it reports a blocked outcome naming the missing host rather than inventing an outcome

#### Scenario: Reject a malformed low-level request

- **WHEN** an entry block is not NUL-terminated or a program path is empty or contains NUL
- **THEN** evaluation reports the low-level invalid-path reason without calling the provider

### Requirement: Host input is an injected evaluator host

Evaluation SHALL accept an explicit host-input provider, separate from the OS filesystem and
standard-input hosts, exposing an argument count, an argument lookup by index, an environment lookup
by raw byte name, and a working-directory lookup, each returning a value, absence, or a host failure.
The evaluator MUST NOT import an ambient process command line, environment, or working directory into
browser-capable compiler cores, and MUST NOT commit more bytes than the caller's buffer holds.

#### Scenario: Evaluate against a scripted command line

- **WHEN** a program reads host input with an injected provider holding a scripted command line, environment, and working directory
- **THEN** each lookup answers from the script, commits only the prefix that fits, and reports the complete byte length

#### Scenario: Block a reachable lookup without a host

- **WHEN** evaluation reaches a host-input lookup and no provider was injected
- **THEN** it reports a blocked outcome naming the missing host rather than inventing an empty command line

### Requirement: Evaluation executes suspension through logical activations

Evaluation SHALL execute explicit suspension by retaining the parent logical invocation as one
reusable coroutine frame in its heap activation machine, evaluating the deferred child, and
resuming the parent with the child's exact typed outcome. It SHALL model the same frame states,
ownership transfers, stable loans, provider access, cleanup, and release as native and Wasm without
requesting a source allocator, emitting continuation-allocation events, or producing typed storage
failure. A suspended parent SHALL remain one active source-logical invocation for `CallDepth`;
compiler-generated driver and resume work SHALL add no source-logical depth. Evaluation MUST NOT
recurse on the JavaScript stack or expose a pending value as a source result.

#### Scenario: Complete deep suspension under raised limits

- **WHEN** a terminating suspended Effect recursion is evaluated with step and call-depth limits above its logical work
- **THEN** evaluation completes with the same result, frame-state transitions, ownership, and cleanup trace as native and Wasm without depending on the JavaScript call stack

#### Scenario: Bound suspended logical recursion

- **WHEN** a suspended recursive Effect would add a source-logical invocation beyond `maxCallDepth`
- **THEN** evaluation returns the deterministic `CallDepth` blocked outcome at that exact source boundary rather than simulating allocation failure

#### Scenario: Preserve channels during evaluation

- **WHEN** evaluation runs `Effect.suspend` over `Effect<A ! E ? R>`
- **THEN** its source-visible outcome and requirements remain exactly `A ! E ? R` with no allocator request or `OutOfMemoryError` branch

#### Scenario: Exhaust private evaluator activation storage

- **WHEN** the evaluator cannot retain the private activation state required to continue suspended execution
- **THEN** evaluation terminates with a fatal host defect or trap-equivalent outcome outside the program's typed failure channel and does not synthesize `OutOfMemoryError`

### Requirement: Evaluation executes finite Effect composites exactly

Evaluation SHALL construct only the selected member of a finite Effect composite, preserve its
laziness, run exactly that member when requested, and retain its exact success or typed-failure
identity under the normalized joined channels. Dropping or completing the value SHALL clean only
the active member and SHALL introduce no allocation event.

#### Scenario: Run the selected success member

- **WHEN** a branch selects one of two compatible lazy Effects and the result is run
- **THEN** evaluation enters only the selected body and returns its exact success value

#### Scenario: Preserve selected failure identity

- **WHEN** the selected member fails with one member of the joined failure union
- **THEN** evaluation retains that exact failure member and payload while closing an unhandled entry failure with status one

#### Scenario: Drop a selected affine capture

- **WHEN** a composite holding one affine capture is dropped without running
- **THEN** evaluation records exactly one cleanup for that selected capture and none for inactive alternatives

### Requirement: Evaluation executes statement patterns from logical members

Evaluation SHALL execute MIR statement selections using the scrutinee's canonical logical active
member. It SHALL preserve nested payload values, create only the selected bindings, execute the
correct conditional body, and apply the MIR ownership and cleanup plan without decoding backend
storage or choosing numeric tags independently.

#### Scenario: Evaluate nested local destructuring

- **WHEN** an irrefutable pattern binds fields nested inside two nominal values
- **THEN** evaluation exposes the exact nested payloads to subsequent statements

#### Scenario: Evaluate conditional mismatch

- **WHEN** the active union member does not equal an if-let selector
- **THEN** evaluation creates no taken-body bindings and executes only the mismatch body

### Requirement: Evaluation is the deterministic local shared ownership oracle

The evaluator SHALL execute verified local-shared layout, initialization, clone, callback access,
conflict, and drop operations using logical block identity rather than JavaScript object identity or
garbage collection. It SHALL retain the concrete target layout, bounded strong count, independent
available-or-active access state, initialized `T`, and private active reclaim authority. Clone SHALL
check before mutation and SHALL fatally trap at the selected target maximum without wrapping,
saturating, storing a count, or returning a partial handle; conflict SHALL leave active access unchanged; normal access SHALL end its
borrow before restoring availability; last drop SHALL clean `T` before releasing storage.

Evaluation SHALL expose bounded deterministic events sufficient to distinguish initialization,
clone, access acquisition, conflict, access restoration, non-last decrement, payload cleanup, and
allocation release. Strong cycles SHALL remain retained without synthesized collection. Fatal traps
SHALL preserve the language's no-unwind contract.

#### Scenario: Evaluate sequential shared mutation

- **WHEN** two handles sequentially inspect, mutate, and inspect one local shared counter
- **THEN** evaluation returns the expected values and records one access acquisition and restoration per successful callback

#### Scenario: Observe conflict without releasing access

- **WHEN** an active callback attempts reentrant access through an alias and then continues using its original borrow
- **THEN** evaluation selects conflict, records no access-state transition for it, and keeps the outer borrow valid until its normal return

#### Scenario: Clone and drop during access

- **WHEN** a callback clones another live alias and drops a non-last handle
- **THEN** evaluation changes only the strong count while access remains active and performs no payload cleanup

#### Scenario: Trap clone before overflow mutation

- **WHEN** evaluation observes the selected target's maximum strong count
- **THEN** it returns the fatal no-unwind trap before count mutation and publishes no new or partial handle

#### Scenario: Clean the last handle in order

- **WHEN** the final handle to an affine payload is dropped
- **THEN** evaluation records payload cleanup exactly once before one allocation release

#### Scenario: Retain a strong cycle

- **WHEN** every external handle to a two-block strong cycle is dropped
- **THEN** evaluation records the external decrements but no payload cleanup or release for either retained block

### Requirement: Evaluation is the independent-execution state oracle

Evaluation SHALL model each explicit Execution with deterministic logical package identity,
execution-local stack root, suspension frames, endpoint generation, Execution state, wake-cell
state, and reclamation authorities independent of JavaScript promises, stack identity, garbage
collection, or object finalization. It SHALL execute owner-selected drives synchronously until
completion or external relinquishment and SHALL model wake as readiness only. Bounded traces SHALL
record initialization, drive, park, latch, suspend, notify, eligible, resume, complete, cancel,
DestroyPending, cleanup, and release in canonical order.

#### Scenario: Defer body evaluation

- **WHEN** evaluation constructs and stores an Initial Execution
- **THEN** no body event occurs until an owner drive transition

#### Scenario: Preserve two logical roots

- **WHEN** evaluation alternates two executions through several parks
- **THEN** each trace retains a distinct stable root and its own CallDepth sequence

#### Scenario: Sweep every ordering branch

- **WHEN** tests execute wake-during-register, wake-after-dormant, destroy-before-wake, eligible-drop, and reentrant-destroy cases
- **THEN** evaluation emits the selected transition and cleanup sequence for each without depending on host scheduling

#### Scenario: Repeat the oracle

- **WHEN** the same pressure program and scripted readiness events are evaluated repeatedly
- **THEN** results, logical identities, and bounded traces are byte-identical

### Requirement: Evaluation executes scalar enums by declared member identity

Evaluation SHALL construct and copy enum members, compare equal enum values, expose discriminants
through `value`, and dispatch matches using the verified canonical member identity. It SHALL consume
the MIR representation plan and SHALL NOT admit arbitrary integers as enum values or independently
reinterpret structural-union tags.

#### Scenario: Evaluate enum construction and matching

- **WHEN** a program constructs one enum member, copies it, checks equality, reads `value`, and matches it
- **THEN** evaluation completes with the results implied by that member and its declared discriminant

### Requirement: Evaluation injects independent native clock hosts

Bootstrap evaluation SHALL accept optional system-clock and monotonic-clock host providers as
separate configuration from each other and from filesystem, input, process, and stream hosts. The
system host SHALL supply canonical Unix-epoch reads and resolution. The monotonic host SHALL supply
canonical non-decreasing reads, resolution, and observable absolute waits. Evaluation SHALL
preserve exact `i64` and `u64` values without consulting JavaScript wall time unless the caller
explicitly chooses a real-time host implementation. Host validation SHALL require seconds in
`[-2^63, 2^63 - 1]`, fractions in `[0, 999_999_999]`, and resolutions in `[1, 2^64 - 1]`; it MUST
NOT truncate or wrap a `bigint` into those ranges.

#### Scenario: Evaluate with scripted clocks

- **WHEN** evaluation receives a fixed system host and a scripted monotonic host
- **THEN** clock operations return the exact injected values and waits advance or record only the
  scripted monotonic timeline

#### Scenario: Keep clock hosts independent

- **WHEN** evaluation receives only a system-clock host
- **THEN** a system read can complete while a reachable OS monotonic operation remains blocked for
  its own missing host

#### Scenario: Preserve wide clock values

- **WHEN** an injected host returns a valid clock component or resolution above JavaScript's exact
  integer range
- **THEN** evaluation retains the exact integer value without Number rounding

#### Scenario: Reject values just outside scalar ranges

- **WHEN** a host or scripted constructor supplies seconds outside `i64`, a fraction outside the
  canonical range, or a resolution outside positive `u64`
- **THEN** it returns explicit failure and commits no wrapped or partially initialized scalar output

#### Scenario: Reject an invalid scripted timeline

- **WHEN** a scripted-host constructor receives a malformed fraction, non-positive resolution, or
  decreasing monotonic sequence
- **THEN** it returns explicit host-construction failure data rather than accepting the script or
  throwing a JavaScript exception

#### Scenario: Record a past virtual wait

- **WHEN** a scripted monotonic host receives a deadline at or before its current mark
- **THEN** it records the completed wait without moving its timeline backwards

### Requirement: Missing evaluator clock hosts are explicit blocked data

A reachable native system-clock operation with no system host SHALL produce
`MissingSystemClock`; a reachable native monotonic operation with no monotonic host SHALL produce
`MissingMonotonicClock`. The blocked outcome and inspector presentation SHALL name the missing
capability and retain the trace preceding it. Evaluation MUST NOT fabricate epoch zero, reuse one
host for the other clock, read ambient process time, or throw a JavaScript exception for absence.

#### Scenario: Block a missing system clock

- **WHEN** evaluation reaches `OsSystemClock.now` with no injected system-clock host
- **THEN** it returns `Blocked(MissingSystemClock)` with the preceding deterministic trace

#### Scenario: Block a missing monotonic clock

- **WHEN** evaluation reaches an `OsMonotonicClock` read or wait with no injected monotonic host
- **THEN** it returns `Blocked(MissingMonotonicClock)` without consulting the system-clock host

### Requirement: Evaluation receives random bytes only from an injected host

Bootstrap evaluation SHALL accept an optional random-byte host independently from every other OS
host. The evaluator MUST NOT consult `Math.random`, Web Crypto, Node cryptography, clocks, process
state, or another ambient source. A reachable OS-random call without the injected host SHALL return
an explicit missing-random blocked reason. A supplied host SHALL be asked for the exact requested
length and its output SHALL be validated as complete bytes before evaluation commits the caller's
buffer.

#### Scenario: Evaluate with a scripted random host

- **WHEN** evaluation receives a scripted host and reaches one OS-random fill
- **THEN** it writes exactly the scripted bytes in order and completes deterministically

#### Scenario: Block a missing random host

- **WHEN** evaluation reaches an OS-random fill without an injected random host
- **THEN** it returns the explicit missing-random blocked outcome without fabricating bytes

#### Scenario: Reject malformed host output

- **WHEN** an injected host returns the wrong length or a value outside the byte range
- **THEN** the low-level call reports failure and no successful fill is committed

### Requirement: Random traces reveal behavior but not generated bytes or raw failures

An evaluator OS-call trace for random filling SHALL record operation identity, requested length,
completion or failure, call-site provenance, and a closed normalized failure category when present.
It MUST NOT record generated byte contents, arbitrary host messages, thrown values, or raw failure
payloads. Equivalent scripted executions SHALL retain deterministic event order without turning
production random data or provider secrets into trace output.

#### Scenario: Trace one random fill

- **WHEN** a scripted OS-random call succeeds during evaluation
- **THEN** the trace records its length and successful outcome but omits the byte values

#### Scenario: Redact a hostile failure payload

- **WHEN** a host failure or thrown value contains a canary copied from generated bytes
- **THEN** raw traces, inspector presentation, flow models, and serialized snapshots expose only the normalized category and contain no canary or arbitrary message
