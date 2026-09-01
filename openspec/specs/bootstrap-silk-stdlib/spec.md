# bootstrap-silk-stdlib Specification

## Purpose

Define how Silk standard-library modules ship with the compiler and become reachable from user
programs without vendoring source, while compiling through the same pipeline with no privilege.

## Requirements

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

### Requirement: Standard-library modules resolve without vendoring

The compiler SHALL ship canonical standard-library `.silk` source files and resolve their module
identities when importing programs do not contain those sources. Standard-library module identities
and source roots SHALL be canonical, disjoint from user module identities, and stable across
processes, hosts, compilation orders, and supported package installation layouts.

#### Scenario: Import a library module from user source

- **WHEN** a user program imports a standard-library module that is not present in the user's source set
- **THEN** resolution succeeds through the ordinary module closure and the resolved declarations carry the library's canonical module identity

#### Scenario: User modules cannot collide with library identity

- **WHEN** a user program declares a module whose name would shadow a standard-library module
- **THEN** the compiler reports a deterministic diagnostic naming both origins instead of silently preferring either

#### Scenario: Library resolution is deterministic

- **WHEN** the same program importing standard-library modules is compiled in two fresh processes
- **THEN** every published artifact that mentions library declarations is byte-identical

#### Scenario: Package the standard library

- **WHEN** a compiler package or toolchain distribution is assembled
- **THEN** content verification finds every canonical `.silk` file required by its deterministic manifest

### Requirement: The three random capabilities are canonical ordinary source

The standard library SHALL ship canonical documented ordinary Silk source for secure `Random`,
deterministic `InsecureRandom`, immutable `InsecureSeed`, and the native `OsRandom` provider. The
portable actors, service contracts, provider implementations, derived operations, seed policy, and
xoshiro algorithm SHALL receive no compiler privilege from module or declaration spelling. Only
the explicit OS provider SHALL invoke the sealed native random intrinsic.

#### Scenario: Navigate each public random actor

- **WHEN** tooling resolves a random service, provider, seed value, or derived operation
- **THEN** go-to-definition opens its canonical `.silk` declaration rather than generated or compiler-known behavior

#### Scenario: Copy a service implementation

- **WHEN** user source defines an equivalent provider under another legal name
- **THEN** it receives ordinary service conformance and lexical provision without intrinsic registration

### Requirement: Random module identities make security explicit

The manifest SHALL assign `silk/random` exclusively to secure `Random` and
`silk/insecure_random` exclusively to non-cryptographic `InsecureRandom` and
`Xoshiro256StarStar`. The former deterministic `silk/random` surface SHALL have no compatibility
alias, forwarding module, deprecated declaration, or dual path. Generated embeddings,
documentation, navigation, resolution indexes, examples, and tests SHALL agree with the canonical
module identities.

#### Scenario: Resolve the secure module

- **WHEN** source imports `silk/random`
- **THEN** resolution exposes the secure service and derived secure operations without the seeded xoshiro provider

#### Scenario: Resolve the insecure module

- **WHEN** source imports `silk/insecure_random`
- **THEN** resolution exposes `InsecureRandom`, `Xoshiro256StarStar`, and the stable seeded operations under explicitly non-cryptographic documentation

### Requirement: Portable random actors do not depend on an OS provider

The portable `random`, `insecure_random`, and `insecure_seed` modules MUST NOT import an OS
provider, target selector, native runtime type, WASI interface, or browser API. Applications SHALL
select and provide implementations at their outer boundary. Importing portable modules on direct
WebAssembly SHALL remain valid when no reachable function calls the native intrinsic.

#### Scenario: Supply an ordinary secure provider

- **WHEN** an application provides a source-defined `Random` implementation
- **THEN** portable secure derived operations use it without loading `silk/os_random`

#### Scenario: Load portable random source on direct Wasm

- **WHEN** a direct-WebAssembly program imports only portable random modules
- **THEN** module closure contains no operating-system random import or native symbol

### Requirement: Standard-library code has no compiler privilege

Canonical standard-library files SHALL remain ordinary Silk source compiled through the same lexer,
parser, elaboration, ownership, and lowering as user code, with the same diagnostics and no phase
branching on library origin. A build MAY generate an embedded byte table from those files, but the
generated bytes MUST match the canonical files exactly and MUST NOT become an independently editable
source of truth.

#### Scenario: Library source fails like user source

- **WHEN** a standard-library module contains a semantic error
- **THEN** compilation reports the same diagnostic an identical user module would receive, attributed to the library module's canonical identity

#### Scenario: No library-origin branch in published artifacts

- **WHEN** semantic, HIR, ownership, or MIR artifacts for a library declaration are inspected
- **THEN** they use the same canonical forms as user declarations with no library-kind tag

#### Scenario: Verify generated embedding

- **WHEN** a build generates an embedded standard-library table
- **THEN** content verification proves each module is byte-identical to its canonical `.silk` file

#### Scenario: Report a library source error

- **WHEN** a standard-library file contains a semantic error
- **THEN** compilation reports the ordinary diagnostic against the canonical library source location
### Requirement: Effect combinators are canonical visible Silk source

The standard library SHALL ship canonical `.silk` declarations for success, failure, and
requirement-channel transformations and for the derived `map`, `mapError`, `mapBoth`, `flatMap`,
`tap`, `catch`, `retry`, `provide`, and `provideEffect` API. These files SHALL be the only editable
source of truth, participate in the deterministic standard-library manifest, and retain ordinary
source spans in semantic facts, diagnostics, documentation, hover, and navigation.

#### Scenario: Navigate to a standard Effect combinator

- **WHEN** editor tooling resolves a call to `Effect.mapBoth`
- **THEN** go-to-definition opens the canonical shipped Silk declaration rather than a generated TypeScript signature or embedded string

#### Scenario: Diagnose standard Effect source normally

- **WHEN** an Effect library body violates row, callable, or ownership rules
- **THEN** the compiler reports the same source diagnostic an equivalent user declaration receives

### Requirement: Effect library sources have no semantic privilege

The compiler SHALL recognize only the closed low-level Effect operations documented by the flow-
function contract. It MUST NOT branch on the `Effect` namespace, combinator declaration identity,
standard-library module identity, or source location when analyzing or lowering derived
combinators. Temporary differential implementations MAY coexist during migration but MUST be
removed before this change is complete.

#### Scenario: Rename a user-defined equivalent

- **WHEN** an equivalent generic combinator is copied into user source under another legal name
- **THEN** it receives the same available HIR, ownership facts, MIR, and execution behavior as the standard declaration

### Requirement: Public abstractions wrap the minimum intrinsic surface

The standard library SHALL ship navigable Silk source for every reusable abstraction removed from
the compiler-known catalog, including numeric interfaces and actor functions, service contracts
and implementations, layout validation, Effect wrappers, and safe storage operations. A public
standard-library declaration MUST NOT receive special semantics from its name; any required
primitive call SHALL be explicit in its source body through `Intrinsic`.

#### Scenario: Navigate from a numeric wrapper

- **WHEN** tooling selects the public generic integer addition function
- **THEN** it navigates to canonical Silk source whose implementation selects a concrete intrinsic through conformance

#### Scenario: Copy a standard-library implementation

- **WHEN** equivalent source declarations are copied under different valid names
- **THEN** they retain equivalent behavior without compiler registration

### Requirement: Logging is canonical visible Silk source

The standard library SHALL ship canonical `.silk` declarations for the scalar `LogLevel` enum,
`LogError`, Logger, `Effect.log`, `Effect.logAt`, `Effect.logTrace`, `Effect.logDebug`,
`Effect.logInfo`, `Effect.logWarning`, `Effect.logError`, the initial stdout and in-memory
providers, and provider-owned recorded observation values where needed. These declarations SHALL
participate in the deterministic standard-library manifest, retain ordinary source spans in
diagnostics and editor facts, and receive no semantic privilege from their module identity.

#### Scenario: Navigate to the Logger contract

- **WHEN** editor tooling resolves a Logger implementation, a `LogLevel` member, or an Effect logging helper
- **THEN** go-to-definition opens the canonical shipped Silk declaration rather than a generated TypeScript signature

#### Scenario: Copy the logging contract into user source

- **WHEN** equivalent enum, service, provider, and helper declarations are written in a user module
- **THEN** they receive the same parsing, conformance, ownership, Effect, and lowering behavior without intrinsic registration

### Requirement: Vector provides canonical lexical slice accessors

Canonical Silk source SHALL define `Vector.asSlice(&self) -> &[T]` and
`Vector.asMutSlice(&mut self) -> &mut [T]` as ordinary wrappers over the minimal raw-buffer view
intrinsics. The accessors MUST cover only initialized elements, MUST NOT allocate or copy, and MUST
remain subject to returned lexical borrow checking.

#### Scenario: Borrow initialized vector elements for reading

- **WHEN** source calls `Vector.asSlice` on a live vector containing initialized elements
- **THEN** it receives one shared lexical view whose length equals the vector length

#### Scenario: Borrow initialized vector elements for mutation

- **WHEN** source calls `Vector.asMutSlice` through an exclusive vector borrow
- **THEN** writes through the returned view affect the vector and competing vector access remains suspended

#### Scenario: Keep Vector ordinary source

- **WHEN** tooling navigates either slice accessor or the compiler lowers its body
- **THEN** the accessor resolves to canonical Silk source and only its raw-buffer operation resolves to `Intrinsic`

### Requirement: Canonical source exports encoding-neutral Bytes

The generated standard-library module graph SHALL export one canonical `Bytes` actor implemented in
ordinary Silk source over `Vector<u8>`. Its manifest dependencies SHALL use the ordinary `Allocator`,
`OutOfMemoryError`, returned-borrow, and Drop contracts, and MUST NOT import filesystem or String policy.

#### Scenario: Load Bytes without platform facilities

- **WHEN** a portable program imports and uses `Bytes` on direct Wasm
- **THEN** module closure includes only its ordinary storage and allocation dependencies and no operating-system imports

#### Scenario: Keep text and I/O out of Bytes

- **WHEN** the canonical source inventory is inspected
- **THEN** `Bytes` contains no UTF-8 validation, formatting rules, filesystem service dependency, or platform provider

### Requirement: Portable filesystem actors are canonical ordinary Silk source

The standard library SHALL ship canonical `.silk` declarations for `Path`, `FileInfo`,
`DirectoryInfo`, `DirectoryEntry`, `FileOperation`, `FileReason`, `FileError`, and `FileSystem` plus
the ordinary helpers `createDirectoriesRecursively`, `writeFileWithParents`, and `exists`. These
actors SHALL participate in the deterministic standard-library manifest, retain ordinary source
spans in diagnostics and editor facts, and receive no compiler privilege from names or module origin.
`Bytes` SHALL be consumed from the separate owned-bytes foundation rather than reimplemented here.

#### Scenario: Navigate to a primitive operation

- **WHEN** editor tooling resolves `FileSystem.readFile`
- **THEN** go-to-definition opens canonical shipped Silk source rather than a compiler-generated host signature

#### Scenario: Navigate to a helper

- **WHEN** editor tooling resolves `writeFileWithParents`
- **THEN** go-to-definition opens its ordinary source composition over parent creation and `writeFile`

#### Scenario: Define a user service implementation

- **WHEN** a user value satisfies the source-defined `FileSystem` contract
- **THEN** it receives ordinary service provision, ownership, and lowering behavior without intrinsic registration

### Requirement: Portable source has no platform provider dependency

Portable filesystem actors and helpers MUST NOT import an OS provider, native path or handle type,
host target selector, hosted-Wasm ABI, or built-in virtual provider. Applications SHALL select and
provide implementations at their outer boundary.

#### Scenario: Load portable source on direct Wasm

- **WHEN** a direct-Wasm application imports portable Path and FileSystem declarations
- **THEN** module closure requires no operating-system filesystem implementation or host import

#### Scenario: Supply an application-specific provider

- **WHEN** an application defines a virtual provider in ordinary Silk source
- **THEN** it can satisfy FileSystem without depending on any standard-library platform implementation

### Requirement: OsFileSystem is separate ordinary source

Canonical standard-library source SHALL define `OsFileSystem` as an ordinary provider separate from
the portable `FileSystem`, `Path`, and value actors. Its constructor SHALL copy one absolute native
root into owned `Bytes` and SHALL require `OutOfMemoryError ? &mut Allocator`. Portable service signatures
MUST NOT mention `OsHandle`, native paths, target selectors, or the provider type.

#### Scenario: Construct an owned native root

- **WHEN** an application creates `OsFileSystem` from borrowed native-root bytes
- **THEN** the provider owns an independent copy and the caller may release the original bytes

#### Scenario: Navigate provider implementation

- **WHEN** tooling resolves an `OsFileSystem` constructor or service method
- **THEN** it navigates to canonical Silk source while only the enclosed low-level calls resolve to `Intrinsic`

#### Scenario: Replace the provider lexically

- **WHEN** an application supplies another value conforming to the portable `FileSystem` service
- **THEN** Effect dispatch uses that provider without constructing `OsFileSystem` or requiring OS intrinsics

### Requirement: Shipped source provides owned String and UTF-8 policy

Canonical shipped Silk source SHALL define nominal `String`, a typed `InvalidUtf8` result member,
complete UTF-8 validation, effectful copying from `string`, allocation-free `String` viewing, and
explicit byte-length, UTF-8 byte-view, and Unicode-scalar traversal functions. The owner SHALL use
ordinary allocation and collection source, preserve valid UTF-8 after every safe operation, and
remain navigable and diagnosable like user source. No safe function SHALL publish a partial string,
hide allocation, or return a view that outlives its backing storage.

#### Scenario: Navigate to String behavior

- **WHEN** editor tooling resolves a call that copies or views a `String`
- **THEN** go-to-definition opens the canonical shipped Silk implementation rather than a compiler-generated declaration

#### Scenario: Preserve UTF-8 across mutation

- **WHEN** safe stdlib operations build or extend owned `String` from valid `string` inputs
- **THEN** every subsequently borrowed view remains valid UTF-8 with the exact concatenated scalar sequence

#### Scenario: Report allocation failure honestly

- **WHEN** copying a non-empty `string` cannot obtain owned storage
- **THEN** the existing typed allocation failure is returned and no incomplete `String` escapes

### Requirement: Unicode policy remains explicit stdlib behavior

Scalar decoding, normalization, grapheme segmentation, case mapping, and locale-sensitive
comparison SHALL be ordinary, explicitly invoked stdlib behavior. The initial string surface MUST
NOT claim implicit normalization or a generic character unit, and later Unicode data versions MUST
be independently testable without changing compiler type identity or target ABI.

#### Scenario: Request scalar traversal

- **WHEN** source traverses a `string` containing one-byte and multi-byte scalars
- **THEN** the stdlib yields the exact Unicode scalar sequence without exposing continuation bytes as characters

#### Scenario: Normalize explicitly

- **WHEN** source requests a named normalization form for canonically equivalent strings
- **THEN** normalization follows the stdlib's declared Unicode policy rather than changing ordinary equality

### Requirement: Semantic text boundaries use string

Shipped standard-library APIs SHALL use `string` for complete logging messages, normalized path
construction and resolution, path text accessors, and native filesystem roots. This SHALL include
`Effect.log`, `Effect.logAt`, every level-specific Effect logging helper, and `Logger.log`.
Implementations SHALL request UTF-8 byte views explicitly where text reaches byte storage, standard
streams, or raw OS operations. APIs whose domain is arbitrary bytes, including `Bytes`, whole-file
contents, and standard streams, SHALL remain byte-oriented.

#### Scenario: Log semantic text

- **WHEN** source submits a complete message through any Effect logging helper or `Logger.log`
- **THEN** the API accepts `string` and a provider converts it to bytes only if its output boundary requires an encoding

#### Scenario: Construct and inspect paths as text

- **WHEN** source constructs, joins, resolves, or inspects a normalized `Path`
- **THEN** the textual inputs and borrowed textual outputs use `string` without exposing the path's owned byte storage

#### Scenario: Preserve binary boundaries

- **WHEN** source reads file contents, writes standard streams, or manipulates arbitrary byte collections
- **THEN** those APIs continue to use byte-oriented values rather than reclassifying binary data as text

### Requirement: StandardInput and its native provider are separate canonical modules

Canonical standard-library source SHALL define the portable `StandardInput` service, its
`ReadOutcome` members, and its typed read failure in one module, and the native `OsStandardInput`
provider in another, mirroring the portable `FileSystem` and native `OsFileSystem` split. The
portable signature MUST NOT mention the provider type, native descriptors, or target selectors.

#### Scenario: Implement the service without a platform intrinsic

- **WHEN** an application supplies its own value conforming to `StandardInput`
- **THEN** Effect dispatch uses that provider without constructing `OsStandardInput` or requiring an OS intrinsic

#### Scenario: Navigate provider implementation

- **WHEN** tooling resolves an `OsStandardInput` read
- **THEN** it navigates to canonical Silk source while only the enclosed low-level call resolves to `Intrinsic`

### Requirement: ChildProcess and its native provider are separate canonical modules

Canonical standard-library source SHALL define the portable `ChildProcess` service, its
`ProcessRequest` builder, its `ProcessOutcome` members, and its typed process failure in one module,
and the native `OsChildProcess` provider in another, mirroring the portable `FileSystem` and native
`OsFileSystem` split. The portable signature MUST NOT mention the provider type, native descriptors,
or target selectors.

#### Scenario: Implement the service without a platform intrinsic

- **WHEN** an application supplies its own value conforming to `ChildProcess`
- **THEN** Effect dispatch uses that provider without constructing `OsChildProcess` or requiring an OS intrinsic

#### Scenario: Navigate provider implementation

- **WHEN** tooling resolves an `OsChildProcess` execution
- **THEN** it navigates to canonical Silk source while only the enclosed low-level calls resolve to `Intrinsic`

### Requirement: HostInput and its native provider are separate canonical modules

Canonical standard-library source SHALL define the portable `HostInput` service, its typed failure,
and its byte and checked-text helpers in one module, and the native `OsHostInput` provider in
another, mirroring the portable `FileSystem` and native `OsFileSystem` split. The portable signature
MUST NOT mention the provider type, native storage, or target selectors.

#### Scenario: Implement the service without a platform intrinsic

- **WHEN** an application supplies its own value conforming to `HostInput`
- **THEN** Effect dispatch uses that provider without constructing `OsHostInput` or requiring an OS intrinsic

#### Scenario: Navigate provider implementation

- **WHEN** tooling resolves an `OsHostInput` lookup
- **THEN** it navigates to canonical Silk source while only the enclosed low-level call resolves to `Intrinsic`

### Requirement: Effect channel combinators are ordinary fixed-mode Silk source

The standard library SHALL define shared `bindRequirement`, exclusive `bindRequirementMut`, owned
`bindRequirementOwned`, `provide`, `provideMut`, and acquisition-based provision as ordinary Silk
declarations using whole input row `R`, selected row `S`, checked fixed-mode provider constraints,
and `Without<R, S>`. Public wrappers SHALL place `S` first and discharge the same intrinsic wanted
from a definitionally equivalent declared given.

`Effect.catch<S>` SHALL accept one nonempty ordinary selected type or union `S`, require `S in E`,
call the sealed executable selective primitive, pass `S` directly to its handler, and return
`Effect<A | B ! Without<E, S> | F ? R | Q>`. `Effect.catchAll` SHALL pass ordinary `E` directly and
remove the entire protected failure channel. No compiler phase SHALL recognize either wrapper by
standard-library actor, name, or origin.

#### Scenario: Preserve Clock while providing Logger

- **WHEN** `Effect.provideMut` receives an Effect requiring `&mut Clock | &mut Logger` and an exclusive `StdoutLogger` conforming only to `Logger`
- **THEN** ordinary constraint solving selects and removes exactly `&mut Logger`, leaving `&mut Clock`

#### Scenario: Bind all three provider modes ordinarily

- **WHEN** shared, exclusive, and owned wrappers are analyzed
- **THEN** their bodies type-check from declared givens and ordinary capture semantics determine borrow, Copy snapshot, or affine take-once behavior

#### Scenario: Recover a selected failure union through ordinary source

- **WHEN** source applies `Effect.catch<FirstError | ThirdError>` to a compatible protected Effect
- **THEN** the wrapper passes that ordinary union to the handler and preserves only the unselected failure alternatives

#### Scenario: Recover the whole failure type through ordinary source

- **WHEN** source applies `Effect.catchAll` to `Effect<A ! E>`
- **THEN** the handler accepts ordinary `E` and no failure-value conversion exists

### Requirement: Shipped error types use the Error suffix

Canonical standard-library error declarations and their public contracts SHALL use descriptive
PascalCase names ending in `Error`. The migration SHALL be atomic and SHALL retain no old-name alias,
fallback, or compatibility export.

#### Scenario: Name allocation failure canonically

- **WHEN** source or tooling resolves the standard allocation failure type
- **THEN** it resolves `OutOfMemoryError` and no old-name declaration or alias exists

#### Scenario: Keep ordinary values eligible as failures

- **WHEN** user source declares `Effect<A ! string>` or another valid detached ordinary type without an `Error` suffix
- **THEN** the compiler accepts it because the suffix is a style rule for error-like declarations, not a type-system gate

### Requirement: Standard provision helpers use canonical key selectors

The canonical Effect source library SHALL expose `provide`, `provideMut`, and `provideEffect` as
ordinary Silk declarations whose explicit selector is `Service` or `Service at Role`. The selector
SHALL NOT contain `&` or `&mut`; each helper's provider parameter SHALL determine available access.
`provideEffect` SHALL acquire a fresh provider for each execution and compose the acquisition
Effect's failure and requirement channels. The superseded `provideWith` name SHALL not resolve as
an alias.

#### Scenario: Resolve only the canonical effectful helper

- **WHEN** tooling inspects the Effect standard-library actor
- **THEN** it exposes `provideEffect` with source spans and does not expose `provideWith`

#### Scenario: Select a non-default role

- **WHEN** a caller supplies `Clock at Primary` to any provision helper
- **THEN** the helper discharges that key and validates provider access separately

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

### Requirement: Canonical source exports separate portable clock actors

The deterministic standard-library manifest SHALL export `silk/system_clock` with `Instant` and
`SystemClock`, and `silk/monotonic_clock` with `MonotonicClock` using the shared `Instant` type.
Both modules SHALL contain their public service contracts, ordinary module-level service wrappers,
complete public documentation, and no target-provider dependency or compiler-recognized
declaration. Their service requirements SHALL be exclusive so ordinary scripted providers can
advance a timeline or record calls while preserving lexical replacement and deterministic behavior.

#### Scenario: Navigate portable clocks

- **WHEN** tooling resolves `Instant`, either service, or any public clock wrapper
- **THEN** go-to-definition opens canonical shipped Silk source rather than a generated signature
  or compiler catalog entry

#### Scenario: Use a pure clock provider on direct Wasm

- **WHEN** a direct-Wasm application implements and provides both clock services in ordinary source
- **THEN** it uses the complete portable API without importing either OS-provider module or a host
  clock ABI

### Requirement: Canonical source exports separate native clock providers

The manifest SHALL export `silk/os_system_clock` and `silk/os_monotonic_clock` as separate native
provider actors. Each module SHALL define one stateless provider, an infallible constructor, the
ordinary source operations needed for its matching service conformance, and documented fatal and
target limitations. Portable service signatures MUST NOT mention either provider, a platform clock
identifier, runtime symbol, target selector, or native status protocol.

#### Scenario: Construct providers without reading time

- **WHEN** an application constructs either OS clock provider and does not invoke a clock operation
- **THEN** construction completes without consulting the host and contributes no reachable clock
  runtime symbol

#### Scenario: Keep provider modules independent

- **WHEN** an application imports and provides only `OsSystemClock`
- **THEN** its source closure does not require `OsMonotonicClock` and its executable closure gains
  no monotonic wait support

### Requirement: Clock documentation is generated and verified from source

All four clock modules SHALL participate in generated standard-library embedding, reference
generation, documentation policy checks, and doctest verification. Documentation SHALL teach the
units, epoch or unspecified origin, canonical negative-time representation, non-decreasing rather
than strictly increasing behavior, same-provider mark limitation, blocking wait semantics, fatal
host boundary, explicit provision, and direct-Wasm exclusion where each applies.

#### Scenario: Generate clock reference pages

- **WHEN** standard-library source and documentation are regenerated
- **THEN** the index lists all four modules and their pages retain the documented service
  operations, conceptual Instant components, public constructor and accessors, provider
  constructors, examples, and portability limitations

#### Scenario: Reject stale clock documentation

- **WHEN** a clock signature or documented example changes without regenerating its reference
- **THEN** normal repository verification fails with the authored or generated source location

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

- **WHEN** `map`, `mapError`, `flatMap`, `unwrapOr`, `Effect.result`, and other Result producers or consumers are compiled
- **THEN** they construct and match direct Result variants without a wrapper field or detached member types

#### Scenario: Remove the Result wrapper representation

- **WHEN** standard-library source, manifests, callers, fixtures, documentation, and tests are inspected after migration
- **THEN** `Result<A, E>` is the direct nominal union and no detached member, wrapper `value` field, alias, compatibility path, or dual representation remains

### Requirement: Static reflection and sequences are canonical ordinary source actors

The standard library SHALL ship canonical documented ordinary source actors for static type and
field reflection and immutable static sequences. Public operations SHALL wrap the minimum sealed
intrinsic seam, retain ordinary source identities and spans, and receive no compiler privilege from
their module, actor, type, or operation spelling. Equivalent user source over the same intrinsics
SHALL receive equivalent behavior.

#### Scenario: Navigate a reflection operation

- **WHEN** tooling resolves a public field-reflection or static-sequence operation
- **THEN** go-to-definition opens its canonical `.silk` declaration and only the irreducible primitive call resolves to `Intrinsic`

### Requirement: Template formatting extends the canonical Format actor

The canonical `silk.format` source module SHALL define the static-template formatting operation by
composing static text inspection, static sequences, reflection, `Display`, `Formatter`, and `Writer`.
The placeholder grammar, validation policy, traversal, and Writer composition MUST remain visible
ordinary source and MUST NOT be implemented by a compiler-known Format declaration or a monolithic
format intrinsic.

The module SHALL also provide ordinary-source `Display<string>` by forwarding the borrowed string's
UTF-8 bytes through the existing Writer surface. It MUST preserve Writer prefix/failure behavior and
MUST NOT allocate an intermediate String or introduce a second text-writing path.

#### Scenario: Copy template formatting into user source

- **WHEN** equivalent template parsing and reflection composition is written under another legal module and operation name
- **THEN** it validates and residualizes through the same public and intrinsic contracts without compiler registration

#### Scenario: Navigate string display

- **WHEN** tooling resolves the canonical `Display<string>` implementation
- **THEN** go-to-definition opens its ordinary `silk.format` source declaration and no compiler-known formatting operation

#### Scenario: Package the new source actors

- **WHEN** the compiler package or toolchain distribution is assembled
- **THEN** manifest verification includes the canonical reflection, static-sequence, and updated format source files byte-for-byte

### Requirement: Formatting streams through Writer under explicit options

Canonical `silk.format` source SHALL define `FormatOptions`, `Formatter`, and the static `Display`
interface. `Display.display` SHALL receive the displayed value by shared borrow and one mutable
Formatter session, return unit, fail only with `WriterError`, and require exclusive access to the
ordinary `Writer` service. Formatter SHALL carry width, alignment, fill, sign, alternate-form,
zero-padding, precision, and color-permission options and SHALL expose ordinary source helpers for
writing content and padding. A Display implementation SHALL be able to call those helpers repeatedly
through compatible call-scoped reborrows of its mutable Formatter parameter. Formatter MUST NOT
own, capture, select, or replace the Writer provider.

`Display` SHALL mean the default human-readable presentation. Radix-specific or diagnostic
presentations MUST NOT silently reinterpret `Display`; they require separately named presentation
contracts or operations.

#### Scenario: Display through a supplied Writer

- **WHEN** a generic function displays a conforming value with one Formatter session
- **THEN** the emitted bytes reach the lexically supplied mutable Writer and any Writer failure remains typed

#### Scenario: Format with defaults

- **WHEN** a caller displays a value without overriding options
- **THEN** Formatter uses the canonical default alignment, fill, sign, padding, precision, alternate-form, and color policy

#### Scenario: Keep Formatter independent from provider selection

- **WHEN** the same Formatter options are used with two different Writer providers
- **THEN** formatting emits the same requested byte sequence while each provider retains its own effects and failures

#### Scenario: Reborrow one Formatter session

- **WHEN** a Display implementation performs several option reads and Writer-backed helper calls through one `&mut Formatter` parameter
- **THEN** each nested call receives a compatible temporary reborrow and the parent Formatter access resumes afterward

### Requirement: Formatting options have deterministic streaming semantics

Width SHALL be a minimum visible Unicode-scalar count, fill SHALL contribute one visible scalar per
repetition, and styling control bytes permitted by the color option SHALL not contribute to width.
Alignment SHALL determine how required fill is divided before and after content. Sign,
alternate-form, zero-padding, and precision SHALL be available to presentation implementations
without forcing unrelated types to invent numeric behavior. A Formatter helper MUST NOT buffer an
unbounded completed rendering merely to discover its width; a Display implementation that honors
width SHALL determine its content width before emission.

The color option SHALL be permission rather than a mandate. When color is false, a conforming
Display implementation MUST NOT emit ANSI styling because of formatting. When color is true, an
implementation MAY emit balanced ANSI SGR styling and SHALL exclude those control bytes from its
reported or calculated visible width.

#### Scenario: Right-align visible content

- **WHEN** content has visible width three and options request width five with right alignment
- **THEN** formatting emits two fill scalars before the content regardless of either sequence's UTF-8 byte length

#### Scenario: Disable color

- **WHEN** color permission is false
- **THEN** formatting emits the unstyled representation with no option-induced ANSI styling bytes

#### Scenario: Permit color without requiring it

- **WHEN** color permission is true for a Display implementation that has no colored presentation
- **THEN** its ordinary uncolored representation remains conforming

#### Scenario: Stream a value with known width

- **WHEN** a Display implementation can determine its content width from the value and options
- **THEN** it emits padding and content directly without first allocating the completed rendering

### Requirement: Every integer has an allocation-free Display conformance

Canonical standard-library source SHALL define an interface-owned inline `Display` conformance for
every signed and unsigned integer type known to the scalar catalog. Integer Display SHALL emit the
canonical decimal representation, including zero and each type's minimum and maximum value, without
an owned `String`, allocator requirement, formatting intrinsic, one-Writer-call-per-digit loop, or
compiler recognition of formatting declarations. Each scalar witness SHALL read its borrowed
receiver explicitly through `self.*`, then use ordinary scalar actor operations and the shared
source rendering core. It SHALL honor width, alignment, fill, sign, zero-padding, and precision
consistently, while decimal alternate form and color permission SHALL not change the digits unless a
separately documented presentation adds styling.

#### Scenario: Display an integer bound

- **WHEN** an integer's minimum or maximum value is displayed with default options
- **THEN** Writer receives its exact canonical decimal spelling on every supported engine

#### Scenario: Pad a signed integer without allocation

- **WHEN** options request a width larger than a negative integer's sign and digits
- **THEN** formatting places fill or zero padding according to alignment and sign policy without requesting Allocator

#### Scenario: Propagate a Writer failure

- **WHEN** Writer rejects an integer rendering after accepting any prefix
- **THEN** display fails with that `WriterError`, performs no allocator operation, and makes no atomic-output guarantee for the already accepted prefix

#### Scenario: Keep integer formatting ordinary source

- **WHEN** equivalent formatter, interface, and integer implementations are copied under legal user names
- **THEN** `self.*` reads each Copy scalar receiver and the implementation receives the same conformance, Effect, ownership, and lowering behavior without intrinsic registration

### Requirement: Integer parsing survives the rendering rewrite

Every integer actor SHALL continue to parse complete canonical decimal text without allocation and
return either the exact in-range value or the existing typed not-a-number or out-of-range reason.
Removing owned-String rendering MUST NOT change accepted text, rejection offsets, range checks, or
engine parity.

#### Scenario: Parse a displayed integer

- **WHEN** a caller captures an integer's default Display bytes as valid text and parses them through the same integer actor
- **THEN** parsing returns the original value without allocation

#### Scenario: Preserve parse failures

- **WHEN** complete text is malformed or outside the destination integer range
- **THEN** parsing returns the existing precise reason and never depends on a Writer or Formatter

### Requirement: Allocating integer rendering has no compatibility path

The superseded integer APIs that return owned `String` values, their allocation-failure contracts,
and their Formatter-internal append engine SHALL be removed. Canonical source, generated embeddings,
manifests, documentation, examples, and repository callers MUST use the Writer-backed formatting
surface, with no deprecated alias, forwarding wrapper, dual implementation, or hidden conversion
back to owned text.

#### Scenario: Inspect the migrated public surface

- **WHEN** integer and format modules are inspected after the change
- **THEN** no public or private integer rendering path constructs an owned String before writing

#### Scenario: Reject a stale allocating caller

- **WHEN** source calls a removed integer-to-String rendering operation
- **THEN** ordinary name resolution reports that the operation is unavailable rather than selecting a compatibility shim

### Requirement: Nonprimitive operation modules expose importable scope actors

Each canonical nonprimitive standard-library operation module SHALL export an ordinary public
zero-data actor under the qualifier used to present that module's complete operation surface when
no existing declaration already provides that scope. Selecting the scope actor SHALL expose the
same public module operations under that qualifier as a namespace import, without compiler
privilege or a runtime representation.

#### Scenario: Select the RawBuffer scope actor

- **WHEN** source imports `silk.raw_buffer { RawBuffer }` and calls `RawBuffer.from<T>`
- **THEN** name resolution reaches the canonical ordinary-source `from` operation and reports no missing-member diagnostic

#### Scenario: Preserve an example qualifier

- **WHEN** a documented example replaces a redundant namespace import with a selected scope-actor import
- **THEN** every operation qualifier in the example remains unchanged and resolves to the same canonical module operation

#### Scenario: Keep primitive modules as namespaces

- **WHEN** source uses operations from `silk.u8`, `silk.u32`, or `silk.usize`
- **THEN** the canonical import is the unaliased module import and the lowercase primitive qualifier remains available

#### Scenario: Scope actors remain ordinary source

- **WHEN** tooling navigates an imported standard-library scope actor
- **THEN** it reaches a public zero-data declaration in canonical Silk source with no compiler-known actor or module-origin exception
