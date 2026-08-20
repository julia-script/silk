## REMOVED Requirements

### Requirement: Effect failure rows are normalized owned contracts

**Reason**: The failure channel is not a distinct row kind. Its `E` is the same ordinary type or
union used by `fail`, propagation, pattern matching, and handler parameters.

**Migration**: Resolve failure channels through ordinary types, use `never` for the empty channel,
and delete failure-row binders and value wrappers.

### Requirement: Effect catch subtracts and composes rows

**Reason**: Selective recovery is no longer restricted to one nominal row member and whole-channel
recovery no longer reifies a special `Row<!E>` value.

**Migration**: Accept any nonempty selected ordinary type or union contained in `E`, pass ordinary
failure values directly, and compute the residual with ordinary `Without<E, S>`.

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

The selector SHALL be nonempty and wholly contained in `E`. A generic `S in E` constraint SHALL
check that ordinary type relation without creating a failure-row binder or runtime type dictionary.
Execution SHALL partition the concrete failure union completely: values in `S` invoke the handler,
and values in `Without<E, S>` propagate unchanged. `Effect.catchAll` SHALL pass the complete
ordinary `E` value directly to its handler.

#### Scenario: Recover one member with a fallback

- **WHEN** an `Effect<i32 ! NotFoundError | OfflineError>` catches `NotFoundError` with a `string` fallback
- **THEN** the result is `Effect<i32 | string ! OfflineError>`

#### Scenario: Re-fail an unhandled member

- **WHEN** a catch-all handler matches one failure member and fails again with the unmatched value
- **THEN** ordinary union narrowing preserves that unmatched member in the output failure channel

#### Scenario: Reject an invalid selected subset

- **WHEN** `S` is `never` or contains an alternative absent from protected failure type `E`
- **THEN** ordinary type constraints reject the catch before lowering

#### Scenario: Catch the whole ordinary failure value

- **WHEN** `Effect.catchAll` protects `Effect<A ! E>`
- **THEN** its handler accepts `E` directly, without `Row<!E>` or another reification wrapper
