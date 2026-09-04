## MODIFIED Requirements

### Requirement: Match facts retain source arms and canonical coverage

Semantic analysis SHALL publish the scrutinee type and access mode, source-ordered arms, resolved
structural roots, applied nominal parents, canonical variants, complete selection paths, source and
canonical field mappings, pattern bindings, guard outcomes, remaining path set before and after each
arm, reachability, explicit expression-or-block body kind, statement facts, normal-completion and lexical-transfer facts, result type, and complete-or-unavailable match outcome. Whole-member selection
SHALL retain the covered descendant paths, while direct variant selection SHALL retain its exact
root-parent-variant leaf without representing that leaf as a structural member. Failed lookups,
damaged patterns, incompatible guards, and unavailable results SHALL retain all independent facts
with exact provenance and causal diagnostics.

#### Scenario: Inspect coverage arm by arm

- **WHEN** `Token` and `End` unguarded arms cover `Token | End`
- **THEN** facts show the canonical set before each arm and the empty remaining set after the second

#### Scenario: Retain an unknown member pattern

- **WHEN** one arm names an unresolved nominal type beside an independently valid arm
- **THEN** both arm facts remain queryable and only the dependent match outcome is unavailable

#### Scenario: Inspect hierarchical coverage arm by arm

- **WHEN** direct variant arms cover every leaf of `HttpError` inside `HttpError | OutOfMemoryError`
- **THEN** facts retain each complete selection path, each subtraction step, and the unchanged normalized structural root identities

#### Scenario: Inspect an expression-nested ordinary arm

- **WHEN** a call argument contains a match with ordinary statement arms
- **THEN** facts retain arm-local statement identities and spans, selected body kind, completion paths, and enclosing transfer targets without introducing a callable or Effect construction

## ADDED Requirements

### Requirement: Ordinary match arms participate in enclosing statement analysis

Analysis SHALL analyze ordinary arm statements in the current computation wherever the match occurs, including initializers, arguments, assignment operands, and return operands. Pattern bindings and block locals SHALL have arm-local lexical scope and obey existing name-conflict rules. Outer mutable writes SHALL affect the enclosing computation eagerly. Every reachable `return` SHALL be discovered and checked against the current function, anonymous callable, or Effect body return contract, including returns nested in a larger expression. The arm closing brace SHALL NOT synthesize an enclosing return. Explicit nested callable and Effect bodies SHALL retain their own execution boundaries. Invalid programs SHALL retain independent facts and use the existing diagnostic codes and exact offending source spans without speculative duplicate diagnostics.

#### Scenario: Check a return nested in an argument

- **WHEN** a function declared to return `i32` calls another function with a match argument whose selected block executes `return true`
- **THEN** the inner return is checked against the enclosing `i32` contract and receives the existing return-mismatch diagnostic at its returned operand span; the body is not executable

#### Scenario: Discover returns in initializer and return operands

- **WHEN** a match nested in a binding initializer or another return operand executes an explicit return
- **THEN** that return belongs to the current enclosing body, contributes its return contract fact, and prevents completion of the containing expression on that path

#### Scenario: Do not infer return from the arm brace

- **WHEN** a selected block completes normally before a later enclosing statement
- **THEN** analysis retains continuation to the later statement and checks enclosing fallthrough separately

#### Scenario: Keep explicit execution boundaries

- **WHEN** an ordinary arm inside a callable or Effect returns, or an explicit callable or Effect expression occurs inside an arm
- **THEN** each return belongs to its nearest enclosing callable or Effect body; the ordinary arm adds no return boundary

#### Scenario: Keep block locals arm-local

- **WHEN** one block declares a local beside pattern bindings and another arm or subsequent statement references that local
- **THEN** the declaration is visible only within its selected arm, and the outside reference receives the existing unresolved-name diagnostic at the reference span

#### Scenario: Apply existing name-conflict rules

- **WHEN** an arm block attempts to redeclare a pattern binding or a name forbidden by the enclosing lexical conflict rules
- **THEN** analysis reports the existing declaration-conflict code at the conflicting declaration with provenance to the original binding

#### Scenario: Distinguish a guard transfer from Boolean rejection

- **WHEN** a guard evaluates a nested match containing ordinary transferring arms
- **THEN** analysis records those enclosing transfers and requires `bool` only on normally completing guard paths; a transfer-only guard needs no Boolean value, and a transfer does not make a later candidate execute

### Requirement: Ordinary match arms compose current computation Effect rows

`run` and `fail` in ordinary arm blocks SHALL obey ordinary legality, failure propagation, requirement-row composition, and lexical provider scope in the current computation. Analysis SHALL retain their success and failure facts and enclosing contracts through every expression nesting position. An arm block SHALL NOT construct an Effect, allocate a capture environment, or defer its statements.

#### Scenario: Compose eager run rows

- **WHEN** selected ordinary arms perform sequential `run` operations requiring Writer and propagating WriterError inside an enclosing effect function
- **THEN** their rows compose into the current computation contract and the enclosing provider scope supplies Writer without any arm-created Effect boundary

#### Scenario: Propagate a failure out of a nested expression

- **WHEN** an ordinary arm within a call argument executes a legal `fail`
- **THEN** the failure contributes to the current failure row and terminates that expression path; it does not become an arm value

#### Scenario: Preserve illegal run and fail diagnostics

- **WHEN** an ordinary block uses `run` or `fail` where the current computation does not permit its requirements or failure row
- **THEN** analysis reports the existing operation or propagation diagnostic code at its ordinary offending source span rather than accepting an implicit Effect boundary
