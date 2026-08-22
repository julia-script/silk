## ADDED Requirements

### Requirement: Shared is canonical ordinary Silk source

Canonical standard-library source SHALL define `Shared<T>` as an explicitly cloned,
non-thread-transferable strong handle containing exactly one private `Intrinsic.SharedCore<T>`.
No compiler phase SHALL know `Shared` by name: it MUST NOT gain an intrinsic nominal entry, layout
branch, cleanup-plan node, semantic special case, MIR operation, evaluator case, or backend case from
the public spelling.

`Shared.make<T>(value)` SHALL return an Effect with only ordinary `OutOfMemoryError` failure and
exclusive `Allocator` requirement, request `sharedLayout<T>()`, allocate through the selected
provider, and initialize only after allocation succeeds. `Shared.clone` SHALL be synchronous and
allocation-free. `Shared.with<T, A>(self: &Shared<T>, use: once fn(&T) -> A) -> A` and
`Shared.withMut<T, A>(self: &Shared<T>, use: once fn(&mut T) -> A) -> A` SHALL accept ordinary
take-once callbacks, return only after their callback borrow ends, and add no failure, Effect, or
allocator channel. `with` SHALL be derived by narrowing the exclusive primitive borrow; every
reentrant access combination SHALL trap through ordinary source conflict policy.

`Shared<T>` SHALL remain affine and local for every `T`. Its first version MUST NOT expose raw
addresses, allocation identity, Weak handles, cycle collection, thread-safe transfer, or a separate
shared-reader primitive.

#### Scenario: Construct through the selected allocator

- **WHEN** `Shared.make(Token.make())` receives one successful allocation
- **THEN** it returns one local affine handle and no allocator requirement remains attached to that handle

#### Scenario: Preserve the value on construction failure

- **WHEN** the allocator rejects `Shared.make` before initialization
- **THEN** the Effect reports `OutOfMemoryError`, creates no handle, and ordinary failure cleanup destroys the token exactly once

#### Scenario: Clone and access without allocation

- **WHEN** source clones an existing handle and performs sequential `with` and `withMut` calls whose callbacks allocate nothing
- **THEN** no allocation event occurs after construction and both handles observe the same stored value

#### Scenario: Trap on nested inspection

- **WHEN** a `Shared.with` callback invokes `Shared.with` through an alias of the same allocation
- **THEN** the nested source conflict callback traps before it receives a second reference

#### Scenario: Rename the safe wrapper

- **WHEN** equivalent ordinary source wraps the sealed core under another nominal and operation names
- **THEN** it receives the same semantic contracts without any compiler branch changing

#### Scenario: Keep cycles explicit

- **WHEN** ordinary source constructs a cycle using cloned strong handles
- **THEN** the public contract specifies a leak and supplies no implicit collection or weak observation
