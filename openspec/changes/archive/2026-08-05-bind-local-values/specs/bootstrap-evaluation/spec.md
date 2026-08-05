## MODIFIED Requirements

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
