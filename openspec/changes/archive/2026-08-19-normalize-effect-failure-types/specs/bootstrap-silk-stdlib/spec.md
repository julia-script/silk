## REMOVED Requirements

### Requirement: Effect row combinators are ordinary fixed-mode Silk source

**Reason**: Provision still operates on requirement rows, but catch no longer operates on a special
failure-row kind or reified whole-row value.

**Migration**: Keep ordinary source wrappers and fixed-mode provider constraints, while expressing
recovery over ordinary failure types and unions.

## ADDED Requirements

### Requirement: Effect channel combinators are ordinary fixed-mode Silk source

The standard library SHALL define shared `bindRequirement`, exclusive `bindRequirementMut`, owned
`bindRequirementOwned`, `provide`, `provideMut`, and acquisition-based provision as ordinary Silk
declarations over requirement rows with their existing checked provider constraints and
`Without<R, S>` results.

`Effect.catch<S>` SHALL accept one nonempty ordinary selected type or union `S`, require `S in E`,
call the sealed executable selective primitive, pass `S` directly to its handler, and return
`Effect<A | B ! Without<E, S> | F ? R | Q>`. `Effect.catchAll` SHALL pass ordinary `E` directly and
remove the entire protected failure channel. No compiler phase SHALL recognize either wrapper by
standard-library actor, name, or origin.

#### Scenario: Recover a selected failure union through ordinary source

- **WHEN** source applies `Effect.catch<FirstError | ThirdError>` to a compatible protected Effect
- **THEN** the wrapper passes that ordinary union to the handler and preserves only the unselected failure alternatives

#### Scenario: Recover the whole failure type through ordinary source

- **WHEN** source applies `Effect.catchAll` to `Effect<A ! E>`
- **THEN** the handler accepts ordinary `E` and no `Row<!E>` conversion exists

### Requirement: Shipped error types use the Error suffix

Canonical standard-library error declarations and their public contracts SHALL use descriptive
PascalCase names ending in `Error`. The migration SHALL be atomic and SHALL retain no old-name alias,
fallback, or compatibility export.

#### Scenario: Name allocation failure canonically

- **WHEN** source or tooling resolves the standard allocation failure type
- **THEN** it resolves `OutOfMemoryError` and no `OutOfMemory` declaration or alias exists

#### Scenario: Keep ordinary values eligible as failures

- **WHEN** user source declares `Effect<A ! string>` or another valid detached ordinary type without an `Error` suffix
- **THEN** the compiler accepts it because the suffix is a style rule for error-like declarations, not a type-system gate
