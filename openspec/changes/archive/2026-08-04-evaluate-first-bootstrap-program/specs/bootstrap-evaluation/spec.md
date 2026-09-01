## Purpose

Evaluate the first closed, semantically checked Silk program to an exact `I32` result while retaining a deterministic explanation of every call and value binding.

## ADDED Requirements

### Requirement: First bootstrap entry point

Bootstrap evaluation SHALL select a top-level function named `main` only when lookup resolves to
exactly one declaration, that declaration has zero parameters, and its declared return type is the
resolved bootstrap `I32` type. Evaluation SHALL return a closed `Blocked` outcome rather than throw
or fail for a missing, ambiguous, parameterized, damaged, or incorrectly typed entry declaration.
Every blocked entry outcome SHALL retain the available lookup, declaration, and syntax provenance.

#### Scenario: Select one valid main

- **WHEN** analysis contains exactly one zero-parameter `main` declaring `I32`
- **THEN** evaluation selects that exact declaration as the entry point

#### Scenario: Block a missing main

- **WHEN** no top-level declaration is named `main`
- **THEN** evaluation is blocked with a missing-entry reason and does not select another function

#### Scenario: Block ambiguous main declarations

- **WHEN** multiple top-level declarations are named `main`
- **THEN** evaluation is blocked with every matching declaration identity and does not choose the first

#### Scenario: Block a parameterized main

- **WHEN** the unique `main` declaration has one or more parameters
- **THEN** evaluation is blocked with the exact entry identity and actual parameter count

### Requirement: Evaluate the closed bootstrap expression slice

Evaluation SHALL interpret available decimal `I32` values, uniquely resolved parameter references,
and uniquely resolved function calls with compatible call contracts. Call arguments SHALL be
evaluated in concrete left-to-right order and bound positionally to the target function's parameters.
Each function SHALL return its evaluated expression value, and a completed entry evaluation SHALL
publish the exact `I32` result. Missing, ambiguous, incompatible, or unavailable facts on the
reachable path SHALL produce a `Blocked` outcome with exact reason data rather than a guessed value.

#### Scenario: Evaluate a literal main

- **WHEN** `main` returns the available literal `42`
- **THEN** evaluation completes with exact `I32` result `42`

#### Scenario: Evaluate through one parameter

- **WHEN** `identity(value: I32)` returns `value` and `main` returns the compatible call `identity(42)`
- **THEN** argument `42` binds to `identity.value`, its reference reads `42`, and evaluation completes with `42`

#### Scenario: Evaluate two positional bindings

- **WHEN** `main` makes a compatible call to a two-parameter function whose return references its second parameter
- **THEN** the second argument binds to parameter one and becomes the completed result

#### Scenario: Block wrong call arity

- **WHEN** a reachable call contract is an arity mismatch
- **THEN** evaluation is blocked at that call with the target identity and expected and actual counts

#### Scenario: Ignore an unreachable broken function

- **WHEN** the program contains a valid reachable path from `main` and a different unreachable function has unavailable semantic facts
- **THEN** evaluation completes from the reachable path without treating the unrelated function as executed

### Requirement: Recursive call cycles are bounded data

Evaluation SHALL track the active function call path by declaration identity. Re-entering an active
declaration SHALL stop evaluation with a recursive-cycle `Blocked` outcome containing the complete
cycle in call order and the closing call-site provenance. Evaluation MUST NOT recurse indefinitely,
overflow the host stack, or define a language-wide recursion policy beyond this bootstrap evaluator.

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
parameter reads, and function returns with existing semantic identities and syntax provenance.
Equivalent analyzed programs SHALL produce equivalent outcomes and traces without depending on
object identity, wall-clock time, random state, I/O, or process-global state.

#### Scenario: Trace the identity program

- **WHEN** `main` evaluates `identity(42)` successfully
- **THEN** the trace records entering `main`, calling `identity`, binding `42` to parameter zero, reading that parameter, returning from `identity`, and returning from `main` in order

#### Scenario: Retain a partial blocked trace

- **WHEN** evaluation reaches an unavailable fact after completing earlier calls or bindings
- **THEN** the blocked outcome retains the ordered successful events preceding the blocked reason

#### Scenario: Repeat successful evaluation

- **WHEN** equivalent valid programs are analyzed and evaluated repeatedly in fresh processes
- **THEN** their exact result, event kinds, identities, values, provenance, and ordering are identical
