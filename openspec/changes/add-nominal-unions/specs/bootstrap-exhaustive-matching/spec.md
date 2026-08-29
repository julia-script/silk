## MODIFIED Requirements

### Requirement: Coverage uses canonical union subtraction

Match arms SHALL be considered in source order over canonical selection paths rooted in the
scrutinee's normalized member set. An ordinary member contributes one root path. A nominal-union
member contributes one root-parent-variant leaf for every canonical variant, including variants with
uninhabited specialized payloads. An unguarded whole-member arm SHALL remove its root and every
remaining descendant; an unguarded qualified variant arm SHALL remove exactly its leaf. A guarded
arm SHALL NOT remove any path. `_` SHALL cover every remaining path and MUST make every following arm
unreachable. A match SHALL be exhaustive only when no paths remain or an explicit universal arm
covers them. Duplicate, unreachable, guard-after-exhaustive-path, and incomplete matches SHALL be
rejected with fully qualified remaining paths and exact arm spans. Variant leaves SHALL never become
members of the structural-union type.

#### Scenario: Exhaust a two-member union

- **WHEN** a match over `Token | End` has unguarded `Token` and `End` arms
- **THEN** coverage reaches the empty set without a universal arm

#### Scenario: Guard does not prove coverage

- **WHEN** the only `Token` arm has a guard and the scrutinee is `Token | End`
- **THEN** both `Token` and `End` remain in the final uncovered-member diagnostic

#### Scenario: Reject an arm after universal coverage

- **WHEN** `_` is followed by another arm
- **THEN** the following arm is diagnosed as unreachable and contributes no binding or result fact

#### Scenario: Match variants directly through a structural union

- **WHEN** a match over `HttpError | OutOfMemoryError` has unguarded arms for every `HttpError` variant and `OutOfMemoryError {}`
- **THEN** coverage is exhaustive without requiring an intermediate whole-`HttpError` arm or nested match

#### Scenario: Cover the remaining nominal subtree

- **WHEN** one direct `HttpError.Timeout` arm is followed by `HttpError remaining`
- **THEN** the whole-parent arm binds `remaining` as `HttpError` and covers every other `HttpError` variant

#### Scenario: Reject a leaf after whole-parent coverage

- **WHEN** `HttpError remaining` is followed by `HttpError.Dns { ... }`
- **THEN** the later variant arm is unreachable because its parent subtree was already removed

#### Scenario: Keep a guarded affine variant available

- **WHEN** a guarded direct variant arm inspects an affine payload and its guard is false before a later arm can select the same path
- **THEN** coverage retains the complete path and ownership retains the tags and payload for the later arm without early movement or cleanup

#### Scenario: Diagnose a missing generic variant path

- **WHEN** a match over `Option<i32> | Option<bool>` omits only `Option<bool>.Some`
- **THEN** the incomplete-match diagnostic names that fully applied root-parent-variant path without collapsing either Option application

## ADDED Requirements

### Requirement: Variant patterns bind struct-like fields

A named-field variant pattern SHALL bind, rename, nest, borrow, move, omit with `..`, and validate
fields under the same rules as a nominal struct pattern. A unit variant SHALL bind no fields. Pattern
selection SHALL retain the applied parent type and canonical variant identity without introducing a
variant subtype.

#### Scenario: Move fields from one selected variant

- **WHEN** `Result<A, E>.Success { value }` matches a moved `Result<A, E>`
- **THEN** `value` receives the specialized `A` payload and cleanup remains restricted to that selected variant

#### Scenario: Reject an incomplete field pattern

- **WHEN** a variant pattern omits a declared field without `..`
- **THEN** analysis reports the same missing-field condition as struct destructuring and creates no executable arm
