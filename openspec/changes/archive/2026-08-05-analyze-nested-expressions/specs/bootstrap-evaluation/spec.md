## MODIFIED Requirements

### Requirement: Evaluate the closed bootstrap expression slice

Evaluation SHALL interpret available decimal `I32` values, uniquely resolved parameter references,
and uniquely resolved function calls with compatible call contracts. Call arguments SHALL be
visited in concrete left-to-right order and flat arguments SHALL be bound positionally to the
target function's parameters. Until recursive expression evaluation is available, reaching an
analyzed call used as an argument SHALL instead produce a closed `Blocked` outcome whose reason
identifies the nested expression and exact syntax provenance. Each completed function SHALL return
its evaluated expression value, and a completed entry evaluation SHALL publish the exact `I32`
result. Missing, ambiguous, incompatible, unavailable, or temporarily unsupported facts on the
reachable path SHALL produce a `Blocked` outcome with exact reason data rather than a guessed value.
Evaluation MUST NOT treat an unreachable nested expression as executed.

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

#### Scenario: Block one reachable nested call

- **WHEN** `main` reaches the semantically compatible expression `identity(identity(42))`
- **THEN** evaluation is blocked with an unsupported-nested-expression reason identifying the inner call and retains the trace prefix that reached the outer call

#### Scenario: Ignore an unreachable broken or nested function

- **WHEN** the program contains a valid flat reachable path from `main` and a different unreachable function has unavailable facts or a nested call argument
- **THEN** evaluation completes from the reachable path without treating the unrelated function as executed

#### Scenario: Repeat the transitional blocked outcome

- **WHEN** an equivalent reachable nested expression is evaluated repeatedly
- **THEN** its blocked reason, nested provenance, and partial trace are identical
