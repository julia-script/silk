## MODIFIED Requirements

### Requirement: Match results join reachable arm types canonically

The result of a match SHALL join only reachable arm body result types. Equal types SHALL remain that
precise type. Source-declared nominal and structural-union results SHALL normalize into one
canonical union, with `never` contributing no member. Distinct occurrence-generated anonymous
aggregate types SHALL NOT be implicitly joined: an uncontextualized match whose reachable arms
produce separate anonymous tuple or record occurrences SHALL have an unavailable result. Distinct
built-in scalars, arrays with different types or lengths, or another mixture that cannot form a
valid structural union SHALL likewise make the match result unavailable rather than introducing an
implicit conversion or non-nominal union member.

An ordinary arm block SHALL contribute `()` when any path completes normally and `never` when no path completes normally. Completion SHALL follow ordinary flow analysis, including nested conditionals and loops, rather than the final token. An all-`never` reachable arm set SHALL produce `never`. A block SHALL NOT yield its trailing expression or coerce its unit result to a surrounding expected result type. Unit mixed with an incompatible scalar SHALL report `SEM0049` at the match expression span; no arm result SHALL be implicitly discarded.

#### Scenario: Join two nominal results

- **WHEN** reachable arms produce precise source-declared `Token` and `End` values
- **THEN** the match result is the canonical normalized type `Token | End`

#### Scenario: Keep one scalar result

- **WHEN** every reachable arm produces `i32`
- **THEN** the match result remains precise `i32`

#### Scenario: Reject incompatible scalar results

- **WHEN** one reachable arm produces `i32` and another produces `bool`
- **THEN** the match result is unavailable with a deterministic incompatible-arm diagnostic

#### Scenario: Reject separate anonymous aggregate results

- **WHEN** reachable arms independently create same-shaped anonymous tuple or record literals
  without an enclosing named aggregate expectation
- **THEN** the match result is unavailable with a deterministic incompatible-arm diagnostic

#### Scenario: Named context resolves every aggregate arm

- **WHEN** an enclosing named aggregate expectation is independently known before analyzing a
  match and every reachable aggregate literal arm satisfies it
- **THEN** every arm constructs that named nominal type and the match result retains that type

#### Scenario: Join a block with a unit expression

- **WHEN** reachable arms contain `{}` and `()`
- **THEN** the match result is unit

#### Scenario: Join a transferring block with a scalar

- **WHEN** one reachable block always returns, fails, or transfers to an enclosing loop and another reachable expression has type `i32`
- **THEN** the transferring arm contributes `never` and the match result is `i32`

#### Scenario: Join only noncompleting arms

- **WHEN** every reachable arm body has no normal completion path
- **THEN** the match result is `never` and no result is required at a continuation

#### Scenario: Reject unit mixed with a scalar

- **WHEN** a normally completing block and an `i32` expression are reachable arms, including in an `i32` expected context
- **THEN** analysis reports `SEM0049` at the match expression span and leaves the match result unavailable

#### Scenario: Conditional return retains fallthrough

- **WHEN** a block contains a conditional return with a path that reaches the closing brace
- **THEN** the block contributes unit, so joining it with an incompatible scalar reports `SEM0049`

#### Scenario: Inner loop break allows arm completion

- **WHEN** a block breaks its own nested loop and execution reaches statements after that loop and the arm closing brace
- **THEN** the block contributes unit rather than `never`

#### Scenario: Analyze nested total transfer

- **WHEN** all branches of nested control flow transfer out of the arm and no path reaches its closing brace
- **THEN** the block contributes `never` even when no final token by itself establishes this fact

#### Scenario: Reject a trailing non-unit expression statement

- **WHEN** an arm body is `{ 42 }`
- **THEN** ordinary expression-statement checking reports `SEM0087` at the non-unit expression statement span and no block value is produced

#### Scenario: Explicit discard completes normally

- **WHEN** an arm body is `{ drop 42 }`
- **THEN** the drop statement is accepted and the arm contributes unit
