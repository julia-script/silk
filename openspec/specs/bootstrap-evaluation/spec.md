# Bootstrap Evaluation Specification

## Purpose

Evaluate the first closed, semantically checked Silk program to an exact `I32` result while retaining a deterministic explanation of every call and value binding.
## Requirements
### Requirement: First bootstrap entry point

Bootstrap evaluation SHALL execute the snapshot's lowered MIR program from the entry that
instance discovery resolved. When discovery reports an unavailable entry — missing, ambiguous,
parameterized, or untyped — evaluation SHALL return a closed `Blocked` outcome carrying that
explicit entry reason rather than throw, fail, or choose a declaration.

#### Scenario: Select one valid main

- **WHEN** discovery resolves exactly one zero-parameter `main` declaring `I32`
- **THEN** evaluation enters that instance's lowered function

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
function returns its terminator's value, publishing the exact `I32` result on completion.
Unavailable facts and ownership violations reach evaluation as the explicit generated traps
lowering inserted; executing a trap SHALL produce a `Blocked` outcome carrying the trap's
function identity, reason, and provenance rather than a guessed value. Functions absent from the
lowered program MUST NOT affect the outcome.

#### Scenario: Evaluate a literal main

- **WHEN** `main` returns the available literal `42`
- **THEN** evaluation completes with exact `I32` result `42`

#### Scenario: Evaluate through one parameter

- **WHEN** `identity(value: I32)` returns `value` and `main` returns the compatible call `identity(42)`
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

The interpreter SHALL execute binary operations with exact signed 32-bit results. Signed
overflow, division by zero, and `-2147483648` divided or remaindered by `-1` SHALL produce a
`Blocked` trap outcome carrying the operation's function identity, an arithmetic reason, and the
operation's provenance — never a wrapped or guessed value. Division SHALL truncate toward zero
and remainder SHALL take the dividend's sign. These outcomes SHALL agree with native execution
across the corpus: matching results for completing programs and matching abnormal termination for
trapping programs.

#### Scenario: Evaluate arithmetic exactly

- **WHEN** `main` returns `I32.subtract(I32.multiply(6, 7), 0)`
- **THEN** evaluation completes with exact result `42`

#### Scenario: Trap on signed overflow

- **WHEN** `main` returns `I32.add(2147483647, 1)`
- **THEN** evaluation blocks at that operation with an arithmetic trap reason and its provenance

#### Scenario: Trap on division by zero

- **WHEN** `main` returns `I32.divide(1, 0)`
- **THEN** evaluation blocks at that operation rather than producing a value

#### Scenario: Truncate division toward zero

- **WHEN** `main` returns `I32.divide(-7, 2)`
- **THEN** evaluation completes with exact result `-3`

### Requirement: Conditionals evaluate exactly one arm

The interpreter SHALL execute user-authored branches: a true condition takes the taken block, a
false condition the otherwise path, and exactly one arm's operations execute per traversal.
Boolean values SHALL be exact (`false` is zero, `true` is one) and comparison operations SHALL
produce them without trapping. Interpreter and native execution SHALL agree arm by arm across
the corpus, including programs whose two arms produce different results.

#### Scenario: Take the true arm

- **WHEN** `main` returns `1` under `if I32.equals(1, 1)` and `0` otherwise
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

- **WHEN** a branching program is evaluated from a snapshot using the canonical `I32` and `Bool` entries
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

Evaluation SHALL compare a dynamic `I32` index against zero and the canonical length before reading
the selected element or continuing a place chain. Failure SHALL produce a deterministic trap with the
index, length, function identity, and exact projection provenance.

#### Scenario: Trap a negative index

- **WHEN** execution indexes an array with `-1`
- **THEN** evaluation blocks at that index operation without reading an element

#### Scenario: Trace a successful indexed field read

- **WHEN** `pairs[index].left` completes
- **THEN** the trace identifies the canonical array, selected index, canonical field, resulting value, and source order
