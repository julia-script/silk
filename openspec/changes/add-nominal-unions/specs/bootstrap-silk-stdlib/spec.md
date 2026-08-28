## MODIFIED Requirements

### Requirement: Option is ordinary canonical Silk source

The standard library SHALL define `Option<T>` as an ordinary shipped nominal union with unit
variant `None` and named-field variant `Some { pub value: T }`. The parent union SHALL be public, so
its variants are externally selectable, and the payload field SHALL be public for direct construction
and matching. Recoverable integer operations and every other optional-value consumer SHALL use this
declaration without an Option-shaped compiler collection primitive. The ordinary `some` and `none`
helper functions MAY remain as ergonomic constructors only when they construct the direct variants.
The former transparent wrapper struct, detached `Some<T>` and `None` structs, compatibility aliases,
and dual representations MUST NOT remain.

#### Scenario: Return checked success

- **WHEN** checked integer arithmetic succeeds
- **THEN** it returns the canonical `Option<T>.Some` variant containing the exact value

#### Scenario: Return checked failure

- **WHEN** checked integer arithmetic cannot represent a result
- **THEN** it returns canonical `Option<T>.None`

#### Scenario: Remove the wrapper representation

- **WHEN** standard-library source, manifests, documentation, and tests are inspected after migration
- **THEN** `Option<T>` is the direct nominal union and no detached `Some<T>`, detached `None`, wrapper `value` field, alias, or compatibility path remains

## ADDED Requirements

### Requirement: Result is one ordinary nominal union

The standard library SHALL define `Result<A, E>` as an ordinary shipped nominal union with
`Success { pub value: A }` and `Failure { pub error: E }`. The parent union SHALL be public, so its
variants are externally selectable, and both payload fields SHALL be public for direct construction
and matching. Its error argument MAY itself be an ordinary structural union and SHALL normalize
independently without changing the two Result variants. The ordinary `succeed` and `failResult`
helper functions MAY remain as ergonomic constructors only when they construct the direct variants.
The former wrapper, detached `Success<A>` and `Failure<E>` declarations, compatibility aliases, and
dual representations MUST NOT remain.

#### Scenario: Carry a structural failure set

- **WHEN** a function returns `Result<Data, HttpError | OutOfMemoryError>`
- **THEN** the result retains exactly `Success` and `Failure`, and the `Failure.error` payload retains the independently normalized structural union

#### Scenario: Migrate standard-library operations

- **WHEN** `map`, `mapError`, `flatMap`, predicates, `Effect.result`, and other Result producers or consumers are compiled
- **THEN** they construct and match direct Result variants without a wrapper field or detached member types

#### Scenario: Remove the Result wrapper representation

- **WHEN** standard-library source, manifests, callers, fixtures, documentation, and tests are inspected after migration
- **THEN** `Result<A, E>` is the direct nominal union and no detached member, wrapper `value` field, alias, compatibility path, or dual representation remains
