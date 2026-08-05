## MODIFIED Requirements

### Requirement: Evaluate the closed bootstrap expression slice
Evaluation SHALL recursively interpret available decimal `I32` values, uniquely resolved parameter
references, and uniquely resolved function calls with compatible call contracts. Every call's
arguments SHALL be evaluated to exact values in concrete left-to-right order before those values
are bound positionally to the target function's parameters. Each function SHALL return its
evaluated expression value, and a completed entry evaluation SHALL publish the exact `I32` result.
Missing, ambiguous, incompatible, or unavailable facts anywhere on the reachable nested path SHALL
produce a `Blocked` outcome with the deepest exact reason data rather than a guessed value. Facts in
unreachable expressions MUST NOT affect the outcome.

#### Scenario: Evaluate a literal main
- **WHEN** `main` returns the available literal `42`
- **THEN** evaluation completes with exact `I32` result `42`

#### Scenario: Evaluate through one parameter
- **WHEN** `identity(value: I32)` returns `value` and `main` returns the compatible call `identity(42)`
- **THEN** argument `42` binds to `identity.value`, its reference reads `42`, and evaluation completes with `42`

#### Scenario: Evaluate two positional bindings
- **WHEN** `main` makes a compatible call to a two-parameter function whose return references its second parameter
- **THEN** the second argument binds to parameter one and becomes the completed result

#### Scenario: Evaluate one nested call
- **WHEN** `main` returns the compatible expression `identity(identity(42))`
- **THEN** the inner call completes with `42`, that value binds to the outer parameter, and evaluation completes with `42`

#### Scenario: Evaluate nested siblings left to right
- **WHEN** a compatible call contains two nested call arguments
- **THEN** the first nested expression completes before the second begins and both completed values are then bound to their matching outer parameters

#### Scenario: Block at an inner unavailable fact
- **WHEN** a reachable inner call has a missing target, incompatible contract, or unavailable value
- **THEN** evaluation is blocked with that inner reason and provenance before binding or executing the enclosing target

#### Scenario: Block wrong call arity
- **WHEN** a reachable call contract at any nesting depth is an arity mismatch
- **THEN** evaluation is blocked at that exact call with the target identity and expected and actual counts

#### Scenario: Ignore an unreachable broken function
- **WHEN** the program contains a valid reachable path from `main` and a different unreachable function has unavailable semantic facts
- **THEN** evaluation completes from the reachable path without treating the unrelated function as executed

### Requirement: Recursive call cycles are bounded data
Evaluation SHALL track the active function call path by declaration identity across both returned
calls and calls reached while evaluating nested arguments. Re-entering an active declaration SHALL
stop evaluation with a recursive-cycle `Blocked` outcome containing the complete cycle in call
order and the closing call-site provenance. A target whose arguments fail before entry SHALL not be
added to the active path. Evaluation MUST NOT recurse indefinitely, overflow the host stack, or
define a language-wide recursion policy beyond this bootstrap evaluator.

#### Scenario: Block direct self recursion
- **WHEN** `main` calls its own unique declaration
- **THEN** evaluation stops with the cycle `main → main` and the self-call span

#### Scenario: Block mutual recursion
- **WHEN** `main` calls `other` and `other` calls `main`
- **THEN** evaluation stops with the ordered cycle `main → other → main`

#### Scenario: Block a cycle reached inside an argument
- **WHEN** evaluating a nested argument call would re-enter an active function declaration
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
parameter reads, and function returns with existing semantic identities and syntax provenance. A
call event SHALL be recorded when its call expression is reached; its nested arguments SHALL then
produce their complete left-to-right events, and the enclosing positional bindings SHALL be
recorded only after every argument has produced a value. Equivalent analyzed programs SHALL
produce equivalent outcomes and traces without depending on object identity, wall-clock time,
random state, I/O, or process-global state.

#### Scenario: Trace the identity program
- **WHEN** `main` evaluates `identity(42)` successfully
- **THEN** the trace records entering `main`, calling `identity`, binding `42` to parameter zero, reading that parameter, returning from `identity`, and returning from `main` in order

#### Scenario: Trace one nested identity program
- **WHEN** `main` evaluates `identity(identity(42))` successfully
- **THEN** the trace records the outer call, the complete inner call and return, the outer binding of the inner result, the outer read and return, and the return from `main` in order with distinct call-site provenance

#### Scenario: Trace nested siblings deterministically
- **WHEN** two nested argument expressions both complete
- **THEN** all events for the first argument precede all events for the second and both precede the enclosing call's binding events

#### Scenario: Retain a partial blocked trace
- **WHEN** evaluation reaches an unavailable fact after completing earlier nested calls
- **THEN** the blocked outcome retains the ordered successful events preceding the blocked reason without an enclosing binding or return that did not occur

#### Scenario: Repeat successful evaluation
- **WHEN** equivalent valid programs are analyzed and evaluated repeatedly in fresh processes
- **THEN** their exact result, event kinds, identities, values, provenance, and ordering are identical
