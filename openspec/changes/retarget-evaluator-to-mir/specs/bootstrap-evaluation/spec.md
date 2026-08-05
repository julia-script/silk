## MODIFIED Requirements

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
parameter locals, and each function returns its terminator's value, publishing the exact `I32`
result on completion. Unavailable facts reach evaluation as the explicit generated traps lowering
inserted; executing a trap SHALL produce a `Blocked` outcome carrying the trap's function
identity, reason, and provenance rather than a guessed value. Functions absent from the lowered
program MUST NOT affect the outcome.

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

#### Scenario: Block at a lowered trap

- **WHEN** a reachable function's body was unavailable and lowered to a generated trap
- **THEN** evaluation is blocked at that trap with its function identity, reason, and causative span

#### Scenario: Ignore an unreachable broken function

- **WHEN** the program contains a valid reachable path from `main` and a different unreachable function has unavailable semantic facts
- **THEN** evaluation completes from the reachable path without executing the unrelated function

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

#### Scenario: Retain a partial blocked trace

- **WHEN** evaluation reaches a trap after completing earlier nested calls
- **THEN** the blocked outcome retains the ordered successful events preceding the trap without a binding or return that did not occur

#### Scenario: Repeat successful evaluation

- **WHEN** equivalent valid programs are analyzed and evaluated repeatedly in fresh processes
- **THEN** their exact result, event kinds, identities, values, provenance, and ordering are identical
