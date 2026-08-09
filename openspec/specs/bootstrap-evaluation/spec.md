# Bootstrap Evaluation Specification

## Purpose

Evaluate the first closed, semantically checked Silk program to an exact `i32` result while retaining a deterministic explanation of every call and value binding.
## Requirements
### Requirement: First bootstrap entry point

Bootstrap evaluation SHALL execute the snapshot's lowered MIR program from the entry that instance
discovery resolved. An ordinary `main() -> i32` SHALL retain its exact completed value. An effectful
`main() -> () ! E` SHALL be constructed and run once by the lowered entry adapter, producing
either completed status `0` or deterministic unhandled-failure termination data retaining the
normalized failure tag and canonical identity. When discovery reports an unavailable entry,
evaluation SHALL return a closed `Blocked` outcome carrying that explicit entry reason rather than
throw, fail, or choose a declaration.

#### Scenario: Select one ordinary main

- **WHEN** discovery resolves exactly one zero-parameter ordinary `main` declaring `i32`
- **THEN** evaluation enters that instance's lowered function and preserves its exact result

#### Scenario: Select one effectful main

- **WHEN** discovery resolves exactly one zero-parameter effectful `main` succeeding with `()`
- **THEN** evaluation enters the generated adapter and runs the entry Effect exactly once

#### Scenario: Retain an unhandled entry failure

- **WHEN** effectful `main` fails with a reportable failure
- **THEN** evaluation returns deterministic termination data naming its normalized tag and canonical identity

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
lowering inserted; executing a trap SHALL produce a `Blocked` outcome carrying the trap's
function identity, reason, and provenance rather than a guessed value. Functions absent from the
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

#### Scenario: Block at a lowered trap

- **WHEN** a reachable function's body was unavailable and lowered to a generated trap
- **THEN** evaluation is blocked at that trap with its function identity, reason, and causative span

#### Scenario: Block at an inner unavailable fact

- **WHEN** a body contains an unavailable fact anywhere in its returned expression
- **THEN** lowering has already turned that body into a generated trap and evaluation blocks there before executing any enclosing target

#### Scenario: Block wrong call arity

- **WHEN** a call contract at any nesting depth is an arity mismatch
- **THEN** the enclosing body's HIR is unavailable, its lowered function traps, and evaluation blocks with that trap's provenance

#### Scenario: Ignore an unreachable broken function

- **WHEN** the program contains a valid reachable path from `main` and a different unreachable function has unavailable semantic facts
- **THEN** evaluation completes from the reachable path without executing the unrelated function

#### Scenario: Evaluate a binding program

- **WHEN** `main` binds `let value = identity(42)` and returns `value`
- **THEN** the call completes into the binding's local, its drop executes at the exit, and evaluation completes with `42`

#### Scenario: Block a use-after-move program at its trap

- **WHEN** a reachable function's ownership verdict is a violation
- **THEN** its lowered function is a generated trap and evaluation blocks there with the violation's provenance

### Requirement: Recursive call cycles are bounded data

Evaluation SHALL track the active call path by canonical function identity. Re-entering an active
function SHALL stop evaluation with a recursive-cycle `Blocked` outcome containing the complete
cycle in call order and the closing call's provenance. Evaluation MUST NOT recurse indefinitely,
overflow the host stack, or define a language-wide recursion policy beyond this bootstrap
interpreter.

#### Scenario: Block direct self recursion

- **WHEN** `main` calls its own unique declaration
- **THEN** evaluation stops with the cycle `main → main` and the self-call span

#### Scenario: Block mutual recursion

- **WHEN** `main` calls `other` and `other` calls `main`
- **THEN** evaluation stops with the ordered cycle `main → other → main`

#### Scenario: Block a cycle reached inside an argument

- **WHEN** evaluating a nested argument call would re-enter an active function
- **THEN** evaluation stops at that inner call with the complete active cycle and its exact call-site span

#### Scenario: Do not enter an enclosing target before its arguments

- **WHEN** an enclosing call's nested argument blocks before producing a value
- **THEN** the enclosing target is absent from the active path unless it was already active for another reason

#### Scenario: Repeat recursive evaluation

- **WHEN** an equivalent recursive program is evaluated repeatedly
- **THEN** its blocked reason, cycle identities, call-site provenance, and trace are identical

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
- **THEN** the blocked outcome retains the ordered successful events preceding the trap without a binding or return that did not occur

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
type, active nominal member identity, and complete member payload. Injection SHALL install the
source member, widening SHALL remap that member into the target union without changing the payload,
and calls, returns, aggregate storage, moves, and writes SHALL preserve the same active identity.

#### Scenario: Evaluate injection and widening

- **WHEN** a `Token` is injected into `Token | End` and widened to `Token | End | Fault`
- **THEN** evaluation retains the complete `Token` payload under the canonical wider type

#### Scenario: Evaluate a union inside an array

- **WHEN** a fixed array stores values contextually injected into one union element type
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
loan identity remain compiler facts rather than runtime payload.

#### Scenario: Observe exclusive mutation in the caller

- **WHEN** a caller passes `&mut values` to a helper that replaces an indexed element and then reads `values` after return
- **THEN** logical evaluation observes the helper's replacement in the original caller-owned array

#### Scenario: Read two source lengths through one callee

- **WHEN** one evaluated slice function receives arrays of two different lengths
- **THEN** each invocation traverses exactly its runtime logical length without copying or specializing the callee by length

### Requirement: Slice evaluation preserves checked-place ordering

Evaluation SHALL check a slice index against its own runtime length before reading, projecting, or
evaluating an assignment replacement. Valid exclusive replacement SHALL update the authoritative
backing place and clean the displaced value exactly once.

#### Scenario: Trap without evaluating a replacement

- **WHEN** an exclusive slice write is out of bounds and its replacement would otherwise produce an observable trace event
- **THEN** evaluation traps at the bounds check and records no replacement event or write

### Requirement: Evaluation preserves exact usize semantics

The evaluator SHALL represent target-selected `usize` exactly and use canonical unsigned decimal encoding.

#### Scenario: Evaluate native maximum

- **WHEN** native `usize` evaluates its maximum value
- **THEN** evaluation returns `18446744073709551615` exactly

### Requirement: Evaluation is the flow and failure oracle

Evaluation SHALL distinguish construction from execution, represent success and owned nominal
failure explicitly, run one layer, recover exact members, propagate unmatched members, and record
deterministic ordered flow/failure/cleanup events. Traps SHALL remain separate blocked outcomes.

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
collection.

#### Scenario: Reuse an exclusive callable sequentially

- **WHEN** a `mut fn` callback updates captured state across two legal invocations
- **THEN** evaluation returns both results in order and retains the mutation between calls

#### Scenario: Reject an already consumed callable

- **WHEN** a take-once callable is invoked after its owned capture was consumed
- **THEN** evaluation exposes the phase-owned rejection rather than duplicating or fabricating the capture

### Requirement: Evaluation distinguishes run grouping

The evaluator SHALL execute an ungrouped pipeline inside the operand of `run` and a pipeline outside
a grouped run over the resulting success value. Both forms SHALL preserve their distinct trace
order and one-layer execution behavior.

#### Scenario: Compare grouped and ungrouped run

- **WHEN** one program spells `run effect |> Effect.map(transform)` and another spells `(run effect) |> transform`
- **THEN** evaluation shows composition-before-execution for the first and value-transformation-after-execution for the second

### Requirement: Evaluation is the deterministic allocation oracle

The evaluator SHALL execute compiler-planned allocation, logical addresses, reclaim tickets,
RawBuffer storage, Slot operations, initialization events, restricted hooks, explicit drop, and
automatic cleanup without relying on JavaScript object identity or garbage collection. It SHALL
support deterministic failure at each requested allocation ordinal, create no owner for a rejected
request, preserve self-contained owners after provider access ends, and expose bounded deterministic
events for acquisition, initialization, destruction, and release.

#### Scenario: Sweep allocation exhaustion

- **WHEN** the same construction program fails each allocation ordinal in turn
- **THEN** every run returns `OutOfMemory`, releases each successfully acquired owner exactly once, and permits a subsequent successful run in the same evaluator

#### Scenario: Drop after provision ends

- **WHEN** evaluation ends the exclusive allocator provider access before dropping the returned Allocation
- **THEN** release succeeds through the allocation's active ticket without looking up the current provider

#### Scenario: Observe hook-before-release order

- **WHEN** a guard owns initialized move-only elements and its backing allocation
- **THEN** the trace records element destruction by the hook before recursive field cleanup releases the bytes

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
