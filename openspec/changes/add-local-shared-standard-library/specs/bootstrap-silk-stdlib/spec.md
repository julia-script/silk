## ADDED Requirements

### Requirement: Shared is canonical ordinary Silk source

Canonical standard-library module `silk/shared` SHALL define and export `Shared<T>` as an explicitly cloned,
non-thread-transferable strong handle containing exactly one private `Intrinsic.SharedCore<T>`.
No compiler phase SHALL know `Shared` by name: it MUST NOT gain an intrinsic nominal entry, layout
branch, cleanup-plan node, semantic special case, MIR operation, evaluator case, or backend case from
the public spelling.

`Shared.make<T>(value)` SHALL return an Effect with only ordinary `OutOfMemoryError` failure and
exclusive `Allocator` requirement, request `sharedLayout<T>()`, allocate through the selected
provider, and initialize only after allocation succeeds. `Shared.clone` SHALL be synchronous and
allocation-free with exact contract
`Shared.clone<T>(self: &Shared<T>) -> Shared<T>` and no Effect, failure, or requirement channel. It
SHALL borrow rather than consume the receiver, publish exactly one new non-Copy strong obligation,
and preserve the intrinsic's fatal pre-mutation overflow trap without a partial handle. It MUST NOT
read, copy, move, or clean `T`.

`Shared.with<T, A>(self: &Shared<T>, use: once fn(&T) -> A) -> A` and
`Shared.withMut<T, A>(self: &Shared<T>, use: once fn(&mut T) -> A) -> A` SHALL accept ordinary
take-once callbacks, return only after their callback borrow ends, and add no failure, Effect, or
allocator channel. `with` SHALL delegate through `Shared.withMut` and narrow its exclusive callback
borrow. Every
reentrant access combination SHALL trap through ordinary source conflict policy.

Successful construction SHALL transfer `T` exactly once into recursively derived opaque-core
cleanup and SHALL publish no separately live source payload. `Shared<T>` SHALL declare no source
Drop hook. Dropping a non-last wrapper SHALL preserve `T`; dropping the last wrapper through a
structured path SHALL clean `T` exactly once before one allocation release.

`Shared<T>` SHALL remain affine with `LocalExecution` affinity for every `T`, recursively through
ordinary aggregates and executable captures. This slice SHALL publish that fact without adding
thread-transfer syntax or a transfer diagnostic. Its first version MUST NOT expose raw
addresses, allocation identity, Weak handles, cycle collection, thread-safe transfer, or a separate
shared-reader primitive.

#### Scenario: Construct through the selected allocator

- **WHEN** `Shared.make(Token.make())` receives one successful allocation
- **THEN** it consumes the token exactly once into one local affine handle with recursively derived core cleanup, no source Drop hook, and no allocator requirement attached to that handle

#### Scenario: Preserve the value on construction failure

- **WHEN** the allocator rejects `Shared.make` before initialization
- **THEN** the Effect reports `OutOfMemoryError`, creates no handle, and ordinary failure cleanup destroys the token exactly once

#### Scenario: Clone and access without allocation

- **WHEN** source clones an existing handle and performs sequential `with` and `withMut` calls whose callbacks allocate nothing
- **THEN** no allocation event occurs after construction and both handles observe the same stored value

#### Scenario: Trap clone before overflow mutation

- **WHEN** `Shared.clone(&handle)` observes the selected target's maximum strong count
- **THEN** ordinary source reaches the intrinsic fatal trap before count mutation and receives no new or partial wrapper

#### Scenario: Drop public handles exactly once

- **WHEN** source clones one `Shared<Token>`, drops the first wrapper, and then drops the second through structured execution
- **THEN** the first drop preserves the token and allocation, while the last drop cleans the token exactly once before one allocation release

#### Scenario: Move an affine payload through mutation

- **WHEN** `Shared.withMut` moves one affine token into ordinary state and a later `Shared.withMut` moves it back out
- **THEN** the token has one owner at every step, is never required to be Copy, and receives no compiler privilege from the wrapper

#### Scenario: Trap every nested access combination

- **WHEN** source nests `with` under `with`, `withMut` under `with`, `with` under `withMut`, and `withMut` under `withMut` through an alias of the same allocation
- **THEN** every nested source conflict callback traps before it receives a second reference

#### Scenario: Reject a returned access borrow

- **WHEN** either public access callback returns its direct or narrowed `&T` or `&mut T` parameter
- **THEN** ownership reports the stable local-shared-access diagnostic with the return and access-boundary spans

#### Scenario: Reject recursive and executable borrow escape

- **WHEN** either public access callback places its borrow in a generic result, aggregate, failure value, Effect, or stored callable
- **THEN** recursive ownership checking reports the same stable diagnostic with the escape and access-boundary spans before executable lowering

#### Scenario: Reject suspension during public access

- **WHEN** either public access callback attempts to suspend while its callback borrow remains live
- **THEN** ownership reports the same stable diagnostic with the suspension and access-boundary spans and no coroutine frame receives the loan

#### Scenario: Retain recursive local affinity

- **WHEN** semantic inspection realizes `Shared<T>`, an aggregate containing it, and an Effect capturing it
- **THEN** each available fact is affine and `LocalExecution` without adding thread-transfer syntax, a transfer verdict, or a transfer diagnostic

#### Scenario: Import the canonical module

- **WHEN** user source imports `silk/shared` without vendoring its source
- **THEN** module closure resolves the canonical `Shared` export and its declarations retain ordinary shipped-source spans

#### Scenario: Rename the safe wrapper

- **WHEN** equivalent ordinary source wraps the sealed core under another nominal and operation names
- **THEN** it receives the same semantic contracts without any compiler branch changing

#### Scenario: Keep cycles explicit

- **WHEN** ordinary source constructs a cycle using cloned strong handles
- **THEN** the public contract specifies a leak and supplies no implicit collection or weak observation
