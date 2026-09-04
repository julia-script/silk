# Bootstrap Intrinsic Boundary Specification

## Purpose

Define the smallest explicit compiler primitive surface from which Silk source can build its
standard library, services, and portable application APIs without hidden name-based privilege.

## Requirements

### Requirement: Callable compiler primitives occupy one sealed namespace

Every source-callable operation selected by compiler identity SHALL be a qualified member of the
compiler-sealed `Intrinsic` namespace. No service, interface, standard-library actor, or ordinary
source declaration name outside that namespace MAY select special elaboration, HIR, MIR, static
evaluation, or LLVM lowering behavior. Language syntax and primitive type identities are not callable
intrinsics merely because the compiler implements them.

#### Scenario: Recognize one explicit intrinsic

- **WHEN** source calls a declared member of `Intrinsic`
- **THEN** analysis records its canonical intrinsic identity and LLVM lowering applies the same primitive contract for each supported target

#### Scenario: Reject hidden name privilege

- **WHEN** an ordinary source actor or operation has the same spelling as a former compiler-known abstraction
- **THEN** it resolves as ordinary source and receives no compiler behavior from its spelling

### Requirement: Compiler features obey the minimal intrinsic rule

A compiler feature SHALL add only the smallest target-neutral primitive contract sufficient for
canonical Silk source to implement the intended public abstraction. Validation, generic lifting,
service policy, presentation policy, and reusable safe composition MUST remain in source whenever
the primitive surface makes that possible. Each admitted intrinsic MUST have a shipped source
consumer or be required directly by language syntax, safety, representation, or backend lowering.

#### Scenario: Reject an abstraction-shaped intrinsic

- **WHEN** a proposed intrinsic duplicates behavior that can be expressed from existing primitives in ordinary Silk source
- **THEN** the intrinsic catalog and its verification reject the new compiler-known operation

#### Scenario: Add one irreducible primitive

- **WHEN** a standard-library implementation cannot perform a required representation or platform operation in Silk
- **THEN** the compiler exposes only that primitive and the public API remains a source declaration over it

### Requirement: Intrinsic safety is explicit and local

Membership in `Intrinsic` SHALL NOT by itself imply safety or unsafety. An intrinsic whose caller
must establish a memory, ownership, pointer, initializedness, or platform ABI invariant MUST require
an explicit unsafe boundary. A primitive whose complete contract preserves safe-code guarantees
SHALL remain callable from safe code even when it can return a typed failure or trap.

#### Scenario: Call a safe scalar primitive

- **WHEN** a standard-library numeric wrapper calls a concrete checked or trapping scalar intrinsic
- **THEN** the call requires no unsafe boundary and preserves the intrinsic's value, result, or trap contract

#### Scenario: Reject unchecked storage outside unsafe

- **WHEN** source calls an intrinsic that reads unproved initialized storage outside an unsafe boundary
- **THEN** analysis rejects the call at that boundary

### Requirement: The intrinsic inventory is closed and auditable

The compiler SHALL publish one deterministic catalog of every intrinsic operation, signature,
safety classification, semantic operation, and supported target. Completion, hover, analysis,
static evaluation, HIR, MIR, LLVM, and LLVM-generated WebAssembly MUST consume that catalog or verified derived
data. Private host imports SHALL be traceable to catalog operations but SHALL NOT become additional
public intrinsic spellings.

#### Scenario: Compare the catalog with compiler branches

- **WHEN** verification scans compiler operation identities, lowering branches, and host imports
- **THEN** every callable compiler primitive maps to one catalog member and no unregistered public operation remains

#### Scenario: Preserve engine parity

- **WHEN** an accepted intrinsic program succeeds, fails, or traps
- **THEN** logical native LLVM and LLVM-generated WebAssembly execution agree on its observable outcome

### Requirement: Raw buffers expose only minimal unsafe initialized views

The sealed `Intrinsic` namespace SHALL provide target-neutral unsafe operations that form shared and
exclusive slices from a live `RawBuffer`, an offset, and a length within its initialized extent. The
operations MUST NOT recognize `Vector` or any other source-defined owner, allocate, copy, resize, or
decide public collection policy. Ordinary Silk wrappers SHALL establish the initialized-range and
aliasing preconditions before invoking them.

#### Scenario: Form a shared initialized view

- **WHEN** an ordinary source wrapper proves that an offset and length are within a live raw buffer's initialized extent
- **THEN** the shared intrinsic returns a view over exactly that range without allocation or copying

#### Scenario: Form an exclusive initialized view

- **WHEN** an ordinary source wrapper holds exclusive access to a live raw buffer and supplies a valid initialized range
- **THEN** the exclusive intrinsic returns the corresponding exclusive view without naming the wrapper type

#### Scenario: Keep collection policy out of the compiler

- **WHEN** a source-defined owner uses the raw-buffer view operations
- **THEN** semantic analysis and lowering depend only on `RawBuffer` and slice invariants, not on the owner's declaration spelling

### Requirement: Intrinsic availability metadata is sealed and auditable

The canonical intrinsic inventory SHALL record a normalized supported-target set for every callable
operation and SHALL expose that data to executable planning, tooling, and inventory tests. Ordinary
source declarations MUST NOT override, infer, or attach compiler target privilege by spelling.

#### Scenario: Audit one restricted operation

- **WHEN** the intrinsic inventory is encoded for tests or tooling
- **THEN** the operation's canonical identity and sorted supported-target set appear in deterministic form

#### Scenario: Refuse source-defined target privilege

- **WHEN** ordinary source declares a function or service using the same name as a restricted intrinsic
- **THEN** it remains an ordinary declaration with no compiler availability metadata

### Requirement: OS filesystem privilege is handle-level and sealed

The `Intrinsic` namespace SHALL contain only the unsafe file open/read/write, directory open/next,
path inspection, directory creation, file removal, directory removal, and generic consuming close
operations required to build an OS provider. Their signatures SHALL use primitive scalars, slices,
explicit scalar output parameters, `bool`, exact `once fn` carriers, and opaque `OsHandle`.
Handle-producing opens SHALL transfer a newly initialized handle only as the argument of the selected
success carrier and SHALL select a zero-argument failure carrier after writing reason outputs; they
MUST NOT require an optionally initialized handle place. Count-producing operations SHALL return
`bool` and write counts and failure details to initialized scalar outputs. No operation may use or
construct a source-defined optional carrier, filesystem service, or domain value.

#### Scenario: Build a source provider from low-level calls

- **WHEN** canonical `OsFileSystem` implements a whole-file read
- **THEN** it composes open, repeated read, and consuming close rather than invoking a compiler-known whole-file operation

#### Scenario: Keep portable operations ordinary

- **WHEN** another source-defined provider implements `FileSystem.readFile`
- **THEN** it can satisfy the service without invoking any OS intrinsic or receiving name-based compiler treatment

#### Scenario: Transfer one opened handle through a carrier

- **WHEN** a file or directory open succeeds
- **THEN** the intrinsic invokes the success carrier exactly once with the newly initialized affine `OsHandle`, cleans the unused failure carrier, and transfers one close obligation without an optional output place

#### Scenario: Refuse an open without initializing a handle

- **WHEN** a file or directory open fails
- **THEN** the intrinsic writes the normalized reason outputs, invokes the failure carrier exactly once, cleans the unused success carrier, and creates no `OsHandle` or close obligation

### Requirement: OS intrinsic invariants are explicit and unsafe

Each OS intrinsic contract SHALL document handle kind and liveness, initialized input/output ranges,
root-confinement inputs, retry behavior, consuming behavior, and target availability. Semantic
analysis SHALL reject safe-call syntax for these operations while preserving the sealed catalog as
the only source-callable compiler privilege.

#### Scenario: Require unsafe acknowledgement

- **WHEN** ordinary source calls an OS handle operation outside the language's unsafe call form
- **THEN** semantic analysis rejects the call even if its argument types otherwise match

#### Scenario: Consume close regardless of outcome

- **WHEN** consuming close reports failure
- **THEN** the source handle is nevertheless dead and cannot be retried or dropped as a live resource

### Requirement: String intrinsics expose only view primitives

The sealed `Intrinsic` namespace SHALL expose only the target-neutral primitives needed to preserve
the abstract `string` representation and implement its compiler-selected operators: unchecked
formation from a live UTF-8 byte view, immutable UTF-8 byte viewing, encoded byte length, and exact
equality. Unchecked formation MUST require an unsafe boundary; inspection and exact equality of
already valid strings SHALL remain safe. The intrinsics MUST NOT allocate, normalize, compare with
locale policy, traverse graphemes, recognize stdlib `String`, or decide owned storage behavior.
Each string intrinsic SHALL declare normalized availability for every current execution target,
and backend preparation SHALL reject reachable use for any target absent from that declaration
before target layout and MIR lowering.

#### Scenario: Audit the intrinsic catalog

- **WHEN** tooling enumerates the deterministic intrinsic catalog after this change
- **THEN** it finds the narrow string view operations with exact signatures, unsafe classification,
  normalized native/LLVM-to-Wasm availability, and no owning-String operation

#### Scenario: Use safe string inspection

- **WHEN** ordinary stdlib code asks an existing valid `string` for its bytes and byte length
- **THEN** it may call the inspection primitives outside unsafe because they cannot violate text or ownership invariants

### Requirement: One unsafe byte-input primitive is admitted

The sealed `Intrinsic` namespace SHALL expose one unsafe native-only byte-input operation taking an
exclusive byte buffer plus exclusive transferred-count, reason, and native-code outputs and returning
`bool`. Success SHALL write the exact transferred byte count, including zero for end of input; failure
SHALL write the normalized low-level reason and native code. The compiler MUST NOT construct or
recognize an optional carrier, `ReadOutcome`, `StreamReadError`, or the `StandardInput` service, and
MUST NOT admit a second input operation for buffering, decoding, or terminal control.

#### Scenario: Report a refused read

- **WHEN** the host refuses a standard-input read
- **THEN** the intrinsic returns `false` and writes the normalized reason and native code without constructing a standard-library value

#### Scenario: Report the end of input

- **WHEN** the host reports that no further bytes will arrive
- **THEN** the intrinsic returns `true` with a zero count and the library decides what that means

### Requirement: Two unsafe child-process primitives are admitted

The sealed `Intrinsic` namespace SHALL expose one unsafe native-only execution operation taking an
executable path, an argument block, an environment block, and a working-directory block as byte
slices plus explicit termination, capture-length, reason, and native-code outputs, and one unsafe
native-only capture operation taking a stream selector, an offset, an exclusive byte buffer, and
exclusive transferred-count and reason outputs and returning `bool`. The argument and environment
blocks SHALL be NUL-terminated entry blocks, and an empty working-directory block SHALL mean the
caller's own directory. A successful execution SHALL retain exactly one capture until the next
execution replaces it. The compiler MUST NOT construct or recognize an optional carrier,
`ProcessRequest`, `ProcessOutcome`, `ProcessError`, or the `ChildProcess` service, and MUST NOT admit
further operations for shells, streaming, or signal delivery.

#### Scenario: Report a failure to start

- **WHEN** the host cannot start the requested program
- **THEN** the execution operation reports failure and writes the normalized reason and native code without constructing a standard-library value

#### Scenario: Report a nonzero exit code as success

- **WHEN** a child runs to completion and returns a nonzero code
- **THEN** the execution operation succeeds and reports that code as data, leaving the meaning to the library

#### Scenario: Copy one completed capture

- **WHEN** a capture reads the retained result of the immediately preceding execution
- **THEN** it returns `true`, commits the requested prefix into the caller's buffer, and writes the exact transferred byte count

### Requirement: Four unsafe process-input primitives are admitted

The sealed `Intrinsic` namespace SHALL expose four unsafe native-only process-input operations: an
argument count with an exclusive `usize` output returning `bool`, and argument, environment-value,
and working-directory lookups each taking an exclusive byte buffer plus exclusive complete-length,
reason, and native-code outputs and returning `bool`. Success SHALL write the value's complete byte
length with the prefix that fits copied into the buffer; failure SHALL write the normalized low-level
reason and native code, where the not-found reason means the value does not exist. The compiler MUST
NOT construct or recognize an optional carrier, `HostInputError`, or the `HostInput` service, and MUST
NOT admit an operation that sets an environment variable, changes the working directory, or parses
arguments.

#### Scenario: Report a value longer than the buffer

- **WHEN** the host holds a value longer than the buffer the caller supplied
- **THEN** the intrinsic returns `true`, copies the prefix that fits, and writes the complete byte length without a separate buffer-too-small protocol

#### Scenario: Report an absent value

- **WHEN** an argument index is past the last argument or an environment name is unset
- **THEN** the intrinsic returns `false` with the not-found reason and the library decides what that means

#### Scenario: Report a refused lookup

- **WHEN** the host refuses an otherwise valid process-input lookup
- **THEN** the intrinsic returns `false` with the normalized non-not-found reason and native code

### Requirement: One target-neutral suspension primitive is admitted

The sealed `Intrinsic` namespace SHALL contain exactly one safe nested-Effect suspension operation
whose contract transfers one deferred Effect to the compiler-owned nested execution boundary and
later returns that Effect's typed outcome. The operation SHALL preserve generic success, failure,
and requirement rows exactly and MUST NOT request a source allocator or report private execution-
storage exhaustion as a typed failure. It MUST NOT expose a continuation type, callback ABI,
scheduler, fiber, pending token, target address, execution-stack allocator, or backend frame layout.
This exact-one constraint applies to nested child-transfer suspension; the separately cataloged
`Intrinsic.park` operation supplies external-wake relinquishment only inside explicit Execution
ownership and MUST NOT be counted or substituted as a nested-transfer operation.

#### Scenario: Audit the suspension seam

- **WHEN** the deterministic intrinsic catalog and its consumers are inspected
- **THEN** exactly one nested-transfer suspension operation is present with LLVM native and WebAssembly artifacts availability, exact channel preservation, and no public continuation-management or allocation operations, while external parking remains a distinct explicit-Execution operation

#### Scenario: Give a same-named function no privilege

- **WHEN** user source defines `Effect.suspend` or another function with the same spelling as the canonical wrapper
- **THEN** it receives ordinary function behavior unless its body explicitly calls the sealed intrinsic

#### Scenario: Keep execution storage out of the intrinsic contract

- **WHEN** tooling renders the nested suspension intrinsic's canonical callable contract
- **THEN** the result contains only the deferred child's `A ! E ? R` channels and no `OutOfMemoryError`, `Allocator`, Execution, or Wake contribution

### Requirement: The public suspension API remains ordinary Silk

The canonical `Effect.suspend` operation SHALL be a shipped ordinary Silk declaration over the
single suspension intrinsic. Generic lifting, row composition, documentation, imports, navigation,
and reusable composition policy MUST remain in source and MUST NOT be selected by standard-library
module identity.

#### Scenario: Navigate to Effect.suspend

- **WHEN** tooling resolves a call to canonical `Effect.suspend`
- **THEN** it navigates to shipped Silk source whose only compiler privilege is its explicit sealed intrinsic call

### Requirement: Row-transforming intrinsics expose canonical callable contracts

Every source-callable row-transforming primitive SHALL declare one canonical `CallableContract`
containing binder kinds/order, fixed parameter modes, parameter and result types, constraints,
capture relationships, and availability. Inventory rendering, signature help, explicit generic
arguments, ordinary call admission, and diagnostic labels SHALL consume that same contract.

Shared, exclusive, and owned requirement binding SHALL be separate sealed operations with selected
requirement row first and `Without<R, S>` results. The sealed post-contract hook SHALL require
constraint evidence and may only validate mode-appropriate place/move legality, record captures,
and construct proof-bearing HIR. It SHALL NOT enumerate candidates, infer access or roles, subtract
rows, or reconstruct an Effect result type.

#### Scenario: Admit source and intrinsic calls through one contract

- **WHEN** an ordinary Silk wrapper and a sealed binding operation have equivalent callable contracts
- **THEN** both calls produce the same generic substitution, wanted, evidence, result row, and diagnostic identity through the common call path

#### Scenario: Reject a hook call without evidence

- **WHEN** a binding post-contract hook is invoked without assumed or concrete proof of its provider-selection wanted
- **THEN** the hook is structurally unavailable rather than performing its own candidate search

#### Scenario: Keep the intrinsic inventory auditable

- **WHEN** intrinsic inventory documentation is generated
- **THEN** fixed modes, selected-row-first binders, constraints, result difference, and availability are rendered from the canonical contracts

### Requirement: Two sealed primitives fund local shared construction

The sealed `Intrinsic` namespace SHALL expose exactly the construction operations
`sharedLayout<T>() -> Layout` and unsafe
`sharedFromAllocation<T>(allocation: Allocation, value: T) -> SharedCore<T>` for local shared
ownership. `sharedLayout` SHALL be pure, allocation-free, target-aware, and specialized by concrete
`T`. `sharedFromAllocation` SHALL consume both arguments, accept only the exact planned layout, and
publish one initialized core without a failure or requirement channel.

Both operations SHALL declare normalized availability for every supported native target and
LLVM-generated WebAssembly. A `sharedLayout<T>` specialization whose complete control block cannot be
represented by the selected target SHALL remain unavailable before MIR and execution, retaining a
stable diagnostic at the intrinsic call; it MUST NOT return a partial `Layout`, runtime validation
member, allocation failure, or trap.

Neither primitive MAY recognize an allocator implementation, allocate storage, expose a raw shared
address or reclaim operation, choose source conflict policy, or recognize a standard-library actor
by spelling. No ordinary declaration outside `Intrinsic` may obtain these contracts from its name.

#### Scenario: Audit the construction inventory

- **WHEN** the sealed intrinsic inventory is encoded for an available target
- **THEN** it contains the two generic construction contracts with their exact safety, access, ownership, failure, and requirement metadata

#### Scenario: Keep layout planning allocation-free

- **WHEN** source evaluates `sharedLayout<T>()`
- **THEN** it receives validated layout data without allocator access, storage acquisition, or a new cleanup obligation

#### Scenario: Reject an unrepresentable control-block layout

- **WHEN** header addition, alignment rounding, or payload placement for concrete `T` exceeds the selected target's representable layout
- **THEN** target layout marks the specialization unavailable with a stable diagnostic at the `sharedLayout<T>` call before MIR, allocation, or execution

#### Scenario: Require unsafe construction syntax

- **WHEN** source calls `sharedFromAllocation<T>` without an explicit unsafe boundary
- **THEN** analysis rejects the call at that source boundary before consuming the allocation or value

#### Scenario: Ignore same-spelled ordinary operations

- **WHEN** ordinary source declares operations named `sharedLayout` or `sharedFromAllocation` outside the sealed `Intrinsic` namespace
- **THEN** both declarations retain ordinary source contracts and receive no intrinsic identity, safety rule, target availability, or lowering behavior from spelling

#### Scenario: Keep policy actors ordinary

- **WHEN** compiler catalogs and phase dispatch are inspected after construction support is added
- **THEN** no entry or branch names `Shared`, `Allocator`, `OutOfMemoryError`, Deferred, Scheduler, or a ready inbox as a privileged actor

### Requirement: Two sealed primitives govern local shared lifecycle

The sealed `Intrinsic` namespace SHALL expose
`sharedClone<T>(self: &SharedCore<T>) -> SharedCore<T>` and
`sharedWithMut<T, A>(self: &SharedCore<T>, use: once fn(&mut T) -> A, onConflict: once fn() -> A) -> A`.
Clone SHALL allocate nothing, invoke no user code, and have no failure or requirement channel. It
SHALL trap before mutation when the target-bounded strong count cannot increment and otherwise
publish exactly one new affine handle without reading, moving, copying, or cleaning `T`.

Access SHALL invoke exactly one callback. It SHALL invoke `use` under one exclusive callback-scoped
borrow when access is available, or `onConflict` without changing the existing active access when it
is not. Access SHALL allocate nothing. After the selected callback returns normally, the unselected
take-once callback environment SHALL receive ordinary callable cleanup exactly once; on successful
access, the borrow SHALL end and availability SHALL be restored before that cleanup and return. No
intrinsic MAY expose the access bit, count, address, last-drop authority, or a
compiler-known conflict value, and no ordinary declaration may gain these contracts by spelling.

#### Scenario: Clone below the count limit

- **WHEN** `sharedClone` observes a strong count below the target maximum
- **THEN** it increments once and returns one new affine handle without allocation or an operation on `T`

#### Scenario: Trap before overflow mutation

- **WHEN** `sharedClone` observes the target maximum strong count
- **THEN** it traps before storing a count or returning a handle

#### Scenario: Select the access callback

- **WHEN** `sharedWithMut` is invoked once with available access and once reentrantly with active access
- **THEN** the first call invokes only `use`, the nested call invokes only `onConflict`, and the nested observation does not release the outer access

#### Scenario: Clean the unselected callback

- **WHEN** each take-once callback owns one affine capture and access selects either success or conflict
- **THEN** the selected callback is consumed by invocation, the unselected callback's capture is cleaned exactly once after normal callback return, and access allocates no storage

#### Scenario: Share one target-selected count boundary

- **WHEN** native execution and LLVM-to-Wasm tests exercise clone at and below the planned count maximum
- **THEN** both consume the same maximum, agree on success below it, and trap before mutation at it

#### Scenario: Audit the lifecycle inventory

- **WHEN** the intrinsic catalog is inspected after lifecycle support is added
- **THEN** clone and callback access are the only new lifecycle calls; no reader, weak, atomic, lock, count, address, access-state, cleanup-authority, conflict-value, or actor-specific operation exists, and same-spelled ordinary declarations receive no privilege

### Requirement: Execution packaging admits only three target-neutral operations

The sealed intrinsic catalog SHALL admit `executionLayout`, unsafe `executionFromAllocation`, and
safe unit-returning `drive` with the exact generic and static-property contracts selected by
SLP-0001. The first two SHALL expose only Layout, Allocation, exact executable values, and opaque
Execution; drive SHALL expose only Execution, one affine branch state, and two NonParking outcome
callbacks whose callable types are `once fn`. The compiler MUST NOT recognize Allocator,
OutOfMemoryError, Execution safe wrappers,
Scheduler, Fiber, Deferred, timer, ready queue, or Coroutine declarations by spelling. The catalog
MUST NOT add a compiler-owned step-result sum, explicit destroy, per-drive endpoint replacement,
general callable erasure, or implicit program-entry owner.

#### Scenario: Audit exact packaging signatures

- **WHEN** the intrinsic inventory is compared with semantic, HIR, MIR, and backend dispatch
- **THEN** all phases agree on exactly the layout, initializer, and drive powers and their safety and static-property metadata

#### Scenario: Admit affine outcome callbacks

- **WHEN** each drive outcome callback owns an affine capture
- **THEN** the intrinsic inventory accepts both callbacks as NonParking `once fn` values and publishes no reusable-call contract

#### Scenario: Build a safe wrapper in ordinary Silk

- **WHEN** ordinary source queries the Layout, allocates through its chosen Allocator, and calls the unsafe initializer
- **THEN** the wrapper exposes its own failure and requirement rows without compiler knowledge of its declaration name

#### Scenario: Rename every source policy actor

- **WHEN** the safe wrapper and its owner actors are renamed while intrinsic calls and semantics remain unchanged
- **THEN** compiler behavior and artifacts remain identical apart from ordinary source identities

#### Scenario: Reject broader lifecycle privilege

- **WHEN** an implementation proposes a step sum, explicit destroy, Scheduler token, or implicit owner to realize the same slice
- **THEN** the intrinsic audit rejects the additional source-callable power as outside the accepted boundary

### Requirement: External parking exposes only Wake, wake, and park

The sealed intrinsic catalog SHALL add opaque affine `Wake`, synchronous consuming `wake(Wake) ->
()`, and effectful unit-returning `park<G,F>(F) -> ()` where `F` is one NonParking
`once fn(Wake) -> G` registration callback. The operations SHALL expose no callback
representation inside Wake, payload channel, scheduler token, explicit cancellation operation,
destroy operation, allocator, timer, queue, Deferred, Fiber, Coroutine, or program-entry policy.
Every phase and target SHALL agree on safety, affinity, local-transfer, reachability, and callback
metadata.

#### Scenario: Audit the wake and park inventory

- **WHEN** intrinsic declarations are compared with semantic, HIR, MIR, and backend branches
- **THEN** all phases expose exactly Wake, wake, and park with matching contracts and no actor-shaped primitive

#### Scenario: Admit an affine registration callback

- **WHEN** a park registration callback owns affine source state
- **THEN** the intrinsic inventory accepts it as a NonParking `once fn`, invokes it at most once, and exposes no reusable-call requirement

#### Scenario: Rename a source waiter actor

- **WHEN** a Deferred-shaped or timer-shaped ordinary source actor is renamed without changing intrinsic calls
- **THEN** registration, notification, and cancellation behavior remains unchanged

#### Scenario: Keep source payload out of Wake

- **WHEN** one waiter stores task identity or result data beside Wake
- **THEN** inspection shows the payload remains ordinary source state and Wake retains only readiness authority

#### Scenario: Reject an explicit intrinsic cancel

- **WHEN** implementation attempts to add source-callable Wake cancellation or Execution destroy for prompt unlinking
- **THEN** the boundary audit rejects it because affine drop plus ordinary guard cleanup already provide the selected contract

### Requirement: Enum backing-value observation uses one sealed intrinsic

The compiler SHALL expose `Intrinsic.enumValue` as the sole source-callable primitive needed to
observe a verified scalar enum's declared discriminant. Each enum declaration's generated
`EnumName.value` associated wrapper SHALL lower through that intrinsic using the enum's canonical
representation result type. No backend or semantic phase SHALL recognize the wrapper name `value` or
a standard-library declaration by spelling, and the intrinsic SHALL NOT construct an enum from an
integer or add conversion policy beyond backing-value projection.

#### Scenario: Lower a generated value wrapper

- **WHEN** source calls `Status.value(Status.Unknown)`
- **THEN** resolution selects the wrapper contributed by the canonical `Status` declaration and lowering uses `Intrinsic.enumValue` with result type `Status`'s representation

#### Scenario: Keep reverse conversion unprivileged

- **WHEN** source attempts to construct `Status` from its representation integer
- **THEN** no enum intrinsic or generated wrapper accepts the operation

### Requirement: Native clocks minimize compiler privilege

The sealed `Intrinsic` namespace SHALL expose exactly three native-only unsafe clock primitives:
monotonic-clock read, resolution, and absolute-deadline wait. It MUST NOT expose a system-clock
operation. `OsSystemClock` SHALL instead declare and call the platform's ordinary C
`clock_gettime` and `clock_getres` functions over a C-layout `timespec` record. Monotonic intrinsic
reads SHALL use scalar seconds and nanoseconds, resolution SHALL use whole nanoseconds, and no
intrinsic SHALL expose platform clock identifiers. The compiler MUST NOT construct or recognize
`Instant`, `SystemClock`, `OsSystemClock`, a standard-library module identity, or system-clock
policy.

Relative waiting, duration-to-deadline arithmetic, public canonical-value validation, provider
types, service conformance, and fatal policy for an unusable host result SHALL remain ordinary Silk
source over those primitives.

#### Scenario: Read monotonic components without constructing Instant

- **WHEN** `OsMonotonicClock` reads its selected clock through `Intrinsic`
- **THEN** the primitive writes target-neutral scalar components and ordinary source constructs the
  public `Instant`

#### Scenario: Keep the system clock outside Intrinsic

- **WHEN** source reaches only a system-clock read
- **THEN** the program retains the ordinary `clock_gettime` foreign import and no system-clock
  intrinsic identity or compiler-runtime symbol

#### Scenario: Build a relative wait in source

- **WHEN** `OsMonotonicClock.waitFor` receives a duration
- **THEN** ordinary source computes one absolute deadline and calls the absolute-wait primitive
  rather than requiring a distinct relative-wait intrinsic

#### Scenario: Copy an OS provider implementation

- **WHEN** equivalent provider source is copied under another legal module and declaration name
- **THEN** it retains equivalent semantics because only explicit foreign declarations and
  intrinsic calls have compiler meaning

### Requirement: Monotonic clock intrinsics preserve explicit validity and failure boundaries

Monotonic clock-read primitives SHALL report whether their scalar outputs were initialized
successfully; failed reads MUST NOT expose partial output. Resolution SHALL report success only for
a positive whole-nanosecond value representable as `u64`. Native absolute wait SHALL accept only
canonical non-negative monotonic deadlines and SHALL report success only after that clock reaches
the deadline. The ordinary-source OS provider SHALL
convert any false result or impossible conversion into a fatal trap because the public service
declares no typed failure channel.

#### Scenario: Reject malformed scalar deadline arguments

- **WHEN** a direct unsafe intrinsic call supplies a negative fraction or at least one billion
  nanoseconds to the absolute-wait primitive
- **THEN** the native boundary reports false without sleeping, while safe ordinary
  source cannot construct that malformed `Instant`

#### Scenario: Preserve atomic monotonic output

- **WHEN** the platform monotonic-clock read fails
- **THEN** the primitive reports false and the source provider traps without constructing an
  `Instant` from partially initialized outputs

#### Scenario: Complete an absolute wait after its deadline

- **WHEN** the native primitive reports successful absolute waiting
- **THEN** a read of the selected monotonic clock is at or beyond the requested deadline

### Requirement: One unsafe native random fill primitive is admitted

The sealed `Intrinsic` namespace SHALL expose exactly one unsafe native-only random operation that
takes an exclusive byte slice, fills the complete slice, and returns `bool`. Success SHALL mean
every requested byte was initialized with host CSPRNG output. Failure SHALL commit no successful
result and SHALL expose no source-level entropy estimate, partial count, algorithm selector,
deterministic seed, native error code, or recoverable fallback. The compiler MUST NOT recognize a
random service, insecure generator, seed value, distribution, or standard-library declaration by
spelling.

#### Scenario: Fill bytes through the minimum boundary

- **WHEN** the ordinary native provider invokes the random intrinsic with a nonempty valid exclusive byte slice
- **THEN** the host reports success only after filling the complete slice

#### Scenario: Keep distributions in source

- **WHEN** standard-library code requests a random `u64`, boolean, or bounded integer
- **THEN** ordinary Silk source derives it from byte filling without another intrinsic operation

#### Scenario: Audit target availability

- **WHEN** tooling enumerates the intrinsic catalog
- **THEN** it finds one random-fill identity available to current native targets but unavailable to LLVM-generated WebAssembly

### Requirement: Recoverable primitives are carrier-neutral

No intrinsic contract SHALL name, construct, match, or recognize source-defined `Option`, `Result`,
or their variants. Existing checked scalar primitives SHALL receive ordinary present and absent
carrier inputs and return their shared result type. The inventory, semantic analysis, HIR, MIR, and
LLVM lowering SHALL treat those carriers through their ordinary exact callable and
value contracts. Completed Effect outcomes SHALL be handled by ordinary Effect composition rather
than an intrinsic. This change SHALL replace the abstraction-shaped existing signatures, SHALL remove
`Intrinsic.effectResult` and all of its compiler support, and SHALL add no replacement source-callable
operation.

#### Scenario: Construct Option in an integer wrapper

- **WHEN** an ordinary checked-integer wrapper supplies the ordinary `some<T>` and `none<T>` constructor functions to its scalar primitive
- **THEN** the primitive selects the correct ordinary carrier and contains no canonical Option or variant identity

#### Scenario: Keep completed Effect reification out of Intrinsic

- **WHEN** ordinary `Effect.result` maps success and catches the complete typed failure in library code
- **THEN** the intrinsic inventory contains no completed-outcome operation and the compiler contains no dedicated HIR, MIR, or backend path for it

#### Scenario: Audit the closed inventory

- **WHEN** the intrinsic inventory is compared before and after migration
- **THEN** abstraction-shaped Option and Result result contracts are gone, `Intrinsic.effectResult` is absent, no replacement callable operation exists, and every remaining changed primitive has one carrier-neutral contract

### Requirement: Static reflection uses the minimum sealed primitive seam

The sealed `Intrinsic` namespace SHALL expose only the irreducible operations required to obtain a
concrete static type's aggregate metadata, construct and inspect immutable static sequences, and
residualize an authorized static field descriptor as an ordinary shared field projection. Metadata
and sequence operations SHALL be unavailable at runtime and SHALL have no LLVM runtime lowering.
The projection bridge MUST consume its descriptor during specialization and
MUST NOT survive as a runtime intrinsic call.

The projection bridge SHALL use an explicit mixed intrinsic contract: its shared owner-reference
parameter remains a runtime lane, its `Field<Owner, Value>` parameter is a required static lane, and
specialization emits the ordinary runtime `&Value` projection after consuming the descriptor. The
intrinsic catalog, calling-shape verification, and residualizer MUST reject any surviving descriptor
lane or projection-intrinsic call. Mixed parameter phases remain sealed intrinsic metadata and MUST
NOT become a privilege inferred for ordinary functions.

Template grammar, placeholder parsing, aggregate-kind policy, visibility policy, field matching,
`Display` selection, Writer composition, and reusable reflection wrappers SHALL remain ordinary
source. No source module, actor, operation, or descriptor spelling outside `Intrinsic` SHALL receive
compiler privilege.

#### Scenario: Copy the public reflection wrapper

- **WHEN** user source defines an equivalent safe wrapper over the admitted reflection intrinsics
- **THEN** it receives the same static descriptors and residual field projections without compiler registration

#### Scenario: Reject reflection at runtime

- **WHEN** a metadata or sequence intrinsic would remain in a runtime calling shape or residual body
- **THEN** specialization reports a static-phase violation and every backend inventory remains reflection-free

#### Scenario: Consume one mixed projection descriptor

- **WHEN** specialization receives `&Owner` in the runtime lane and an authorized `Field<Owner, Value>` in the static lane
- **THEN** it publishes one ordinary `&Value` field projection and no descriptor parameter or intrinsic call

### Requirement: Reflection primitives cannot expose compiler identity or host state

Reflection and static-sequence primitives SHALL return canonical source-semantic data only. They
MUST NOT expose compiler addresses, host objects, allocation capacity, cache identities, backend
layouts, mangled names, private inaccessible names, filesystem state, environment values, time, or
randomness.

#### Scenario: Repeat reflection in fresh processes

- **WHEN** equivalent concrete aggregate types are reflected in two fresh compiler processes
- **THEN** their public descriptor encodings and source provenance are identical and contain no host-specific value

### Requirement: One sealed primitive exposes the selected target only to static evaluation

The sealed `Intrinsic` namespace SHALL expose one safe, zero-argument, static-only target-profile
operation returning a closed primitive profile code for the four canonical bootstrap targets. The
operation SHALL read the compilation target selected by the compiler, SHALL be available to static
evaluation only, and MUST NOT lower to HIR runtime operations, MIR instructions, host
imports, native symbols, or WebAssembly instructions. Its profile codes SHALL be deterministic and
documented so ordinary standard-library source can map them to nominal target enums and derive all
public target facts.

No compiler phase MAY recognize the spelling of the standard-library target module, its enums,
facts, or wrappers. The intrinsic MUST NOT expose backend objects, host detection, arbitrary target
strings, layout offsets, feature probes, or a runtime target query.

#### Scenario: Build the public target API in ordinary source

- **WHEN** the standard-library target module calls the static target-profile intrinsic and maps its result to a nominal architecture enum
- **THEN** a user target check resolves through ordinary imports, calls, enum equality, and static evaluation without compiler-known library spelling

#### Scenario: Reject the target query at runtime

- **WHEN** an ordinary runtime expression calls the target-profile intrinsic outside static evaluation
- **THEN** analysis reports that the intrinsic is static-only and no runtime intrinsic inventory entry is created

#### Scenario: Audit the minimal target seam

- **WHEN** the intrinsic catalog and generated standard-library source are inspected
- **THEN** exactly one static target-profile primitive exists and target policy, enums, pointer-width facts, and presentation remain ordinary source

### Requirement: Two unsafe bulk raw-storage primitives are admitted

The sealed `Intrinsic` namespace SHALL expose one unsafe operation that moves a caller-proven
initialized range of elements into raw storage, taking an exclusive `RawBuffer<T>` borrow, a
destination element offset, a shared `&[T]` source range, and an element count; and one unsafe
operation that sets a caller-proven byte range of a `RawBuffer<u8>` to one repeated byte value. Both
SHALL be callable only inside an `unsafe` block, and the compiler MUST NOT admit a resize, a public
release, or a safe public bulk API alongside them.

#### Scenario: Move a range between two buffers

- **WHEN** source calls the copy primitive with a source range viewed from one buffer and an exclusive borrow of another
- **THEN** every element in the range is transferred in one operation and no per-element primitive is required

#### Scenario: Set a byte range

- **WHEN** source calls the set primitive with a byte offset, a byte length, and a byte value
- **THEN** exactly the selected bytes hold that value and the surrounding bytes are unchanged

### Requirement: A bulk copy moves ownership and defines overlap

The copy primitive SHALL transfer ownership of every element in the range. A structurally Copy
element type SHALL leave the moved-from range readable, and a move-only element type SHALL give up
its moved-from slots. An overlapping source range and destination range SHALL produce the result of
copying through an intermediate buffer, and the compiler MUST NOT emit a runtime overlap check or a
diagnostic for one.

#### Scenario: Give up move-only source slots

- **WHEN** a range of move-only elements is copied out of raw storage
- **THEN** the moved-from slots hold no value and reading one traps

#### Scenario: Copy an overlapping range

- **WHEN** the source range and the destination range of one copy overlap
- **THEN** the result equals the result of copying the whole range through an intermediate buffer
