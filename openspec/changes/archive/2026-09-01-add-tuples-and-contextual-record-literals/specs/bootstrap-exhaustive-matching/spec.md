## MODIFIED Requirements

### Requirement: Match results join reachable arm types canonically

The result of a match SHALL join only reachable arm expression types. Equal types SHALL remain that
precise type. Source-declared nominal and structural-union results SHALL normalize into one
canonical union, with `never` contributing no member. Distinct occurrence-generated anonymous
aggregate types SHALL NOT be implicitly joined: an uncontextualized match whose reachable arms
produce separate anonymous tuple or record occurrences SHALL have an unavailable result. Distinct
built-in scalars, arrays with different types or lengths, or another mixture that cannot form a
valid structural union SHALL likewise make the match result unavailable rather than introducing an
implicit conversion or non-nominal union member.

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
