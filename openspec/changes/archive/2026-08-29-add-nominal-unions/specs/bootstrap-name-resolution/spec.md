## ADDED Requirements

### Requirement: Variants resolve through an instantiated nominal union

Name resolution SHALL first resolve a constructor qualifier through ordinary module scopes to one
canonical union declaration and bind any contiguous explicit parent-argument prefix. It SHALL then
resolve the variant only within that declaration; named-field inference SHALL complete the parent
application before the canonical selection becomes available. Pattern qualifiers SHALL resolve one
complete applied parent without scrutinee- or expected-type inference. A bare variant name SHALL NOT
search visible union declarations, and a same-spelled variant from another union SHALL remain a
distinct identity. Cross-module access SHALL enforce parent-union and complete-variant construction
authority under the ordinary nominal declaration rules.

#### Scenario: Resolve one applied variant

- **WHEN** `Result<i32, Problem>.Failure` is selected from a visible generic `Result<A, E>` declaration
- **THEN** resolution records the applied parent arguments and the canonical `Failure` identity owned by `Result`

#### Scenario: Complete a zero-prefix constructor qualifier

- **WHEN** `Option.Some { value: 42 }` resolves `Option<T>` and supplies no explicit parent arguments
- **THEN** resolution selects `Some` from the canonical declaration and records the applied `Option<i32>` only after field inference completes

#### Scenario: Reject a variant through the wrong parent

- **WHEN** two unions declare `Failure` and source selects the first union while requiring the second union's variant
- **THEN** analysis reports the canonical parent mismatch instead of resolving by spelling

#### Scenario: Keep unqualified variants out of ordinary lookup

- **WHEN** source refers to `Failure` without an ordinary binding or parent qualifier
- **THEN** resolution reports the ordinary unresolved-name state and does not search union variant sets

#### Scenario: Refuse pattern inference from the scrutinee

- **WHEN** a pattern spells `Option.Some { value }` against a scrutinee of type `Option<i32>`
- **THEN** resolution reports the incomplete pattern qualifier and requires `Option<i32>.Some`
