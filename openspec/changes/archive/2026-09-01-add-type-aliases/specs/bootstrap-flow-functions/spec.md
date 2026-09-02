## MODIFIED Requirements

### Requirement: Effect failure channels contain ordinary types

For `Effect<A ! E ? R>`, `E` SHALL be an ordinary detached owned type or normalized structural
union. The `!` token SHALL label the channel only; it SHALL NOT create a distinct type kind, binder
form, or value wrapper. `never` SHALL denote the empty failure channel. A declared failure-row
member whose canonical type is a structural union SHALL contribute each union member to the row as
a separate member, so the row is identical whether the members are spelled directly, through a
parenthesized union, or through an alias. A nominal union SHALL remain one atomic member. `fail`
SHALL stop the current Effect execution with success type `never`, copying a Copy payload or
consuming an explicitly moved affine payload.

#### Scenario: Fail with a Copy problem

- **WHEN** an Effect executes `fail problem` for a Copy nominal value
- **THEN** the failure channel receives the copied value without requiring `fail move`

#### Scenario: Use the same failure union as a value

- **WHEN** `E` is `NotFoundError | OfflineError`
- **THEN** the Effect failure channel, a propagated failure value, and a handler parameter all use that same ordinary union without a value conversion

#### Scenario: A union alias flattens into the declared row

- **WHEN** `type FetchError = HttpError | JsonError` is declared and a function is declared `-> () ! FetchError`
- **THEN** its failure row has the two members `HttpError` and `JsonError`, and `Effect.catch<HttpError>` leaves the residual row `JsonError`

#### Scenario: Selecting through a union alias removes every member

- **WHEN** `Effect.catch<FetchError>` protects a row declared `! HttpError | JsonError | Timeout`
- **THEN** the residual row is `Timeout`, identical to `Effect.catch<HttpError | JsonError>`

#### Scenario: A nominal union stays atomic

- **WHEN** `union HttpError { NotFound, Timeout }` is a member of a failure row
- **THEN** `Effect.catch<HttpError>` removes the whole member and no selector can remove `NotFound` alone

#### Scenario: Reject a borrowed failure payload

- **WHEN** an Effect attempts to fail with a non-detached lexical borrow
- **THEN** analysis reports the ordinary ownership violation rather than a failure-kind diagnostic
