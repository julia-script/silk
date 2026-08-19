## ADDED Requirements

### Requirement: Effect failure channels contain ordinary types

For `Effect<A ! E ? R>`, `E` SHALL be an ordinary detached owned type or normalized structural union. The `!` token SHALL label the channel only; it SHALL NOT create a distinct type kind, binder form, or value wrapper. `never` SHALL denote the empty failure channel.

#### Scenario: Use the same failure union as a value

- **WHEN** `E` is `NotFoundError | OfflineError`
- **THEN** the Effect failure channel, a propagated failure value, and a handler parameter all use that same ordinary union without `Row<!E>` conversion

#### Scenario: Reject a borrowed failure payload

- **WHEN** an Effect attempts to fail with a non-detached lexical borrow
- **THEN** analysis reports the ordinary ownership violation rather than a failure-row-kind diagnostic

### Requirement: Selective recovery subtracts ordinary unions

`Effect.catch<S>` SHALL select one ordinary type or union `S` from `E`, pass the selected value directly to its handler, and compute the residual failure type as `Without<E, S>`. Handler success SHALL join with protected success as an ordinary finite union when needed.

#### Scenario: Recover one member with a fallback

- **WHEN** an `Effect<i32 ! NotFoundError | OfflineError>` catches `NotFoundError` with a `string` fallback
- **THEN** the result is `Effect<i32 | string ! OfflineError>`

#### Scenario: Re-fail an unhandled member

- **WHEN** a catch-all handler matches one failure member and fails again with the unmatched value
- **THEN** ordinary union narrowing preserves that unmatched member in the output failure channel
