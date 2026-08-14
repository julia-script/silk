# bootstrap-owned-allocation Specification

## Purpose

Define the minimum target-aware owned-memory and deterministic-cleanup substrate from which Silk
standard-library collections can be implemented without named scopes or compiler-known allocators.

## Requirements

### Requirement: Layout is validated before allocation

`Layout` SHALL be a Copy value containing a target-sized byte count and validated power-of-two
alignment. Construction and repeated-element multiplication SHALL report invalid alignment and
representational overflow as ordinary validation data before any allocator or backend is entered.

#### Scenario: Reject overflowing repeated layout

- **WHEN** an element layout and runtime count exceed the selected target's `usize` range
- **THEN** layout construction returns overflow data and performs no allocation

### Requirement: Allocation is a self-contained affine owner

The allocator capability SHALL accept a validated `Layout` through an explicit exclusive service
requirement and return either the allocation-free typed failure `OutOfMemory` or one affine
`Allocation`. A successful `Allocation` SHALL carry private unforgeable active reclaim authority
containing everything required for infallible release. The representation of that authority is
chosen by the backend and is not observable: a backend MAY carry the address of a backend-private
block header there, and MAY require that header to find the storage a release returns. The authority
MUST remain unnameable, unreadable, and unforgeable from Silk regardless of representation, and no
public `free` may be derived from it. An `Allocation` MUST NOT borrow, retain, or later rediscover
the provider that created it, and failed allocation MUST NOT create storage or reclaim authority.

#### Scenario: Drop after provider borrow ends

- **WHEN** an allocation escapes the call that borrowed `SystemAllocator`
- **THEN** the provider borrow ends, the allocation remains valid, and its eventual Drop releases through its captured reclaim authority exactly once

#### Scenario: Fail atomically under exhaustion

- **WHEN** the selected allocator cannot satisfy one valid layout
- **THEN** the Effect fails with `OutOfMemory` without allocating the failure, publishing an allocation owner, or scheduling cleanup for the rejected request

#### Scenario: Transfer allocation ownership

- **WHEN** an allocation moves through an ordinary function and its original binding leaves scope
- **THEN** only the destination remains live and eventual cleanup consumes the same reclaim authority exactly once

#### Scenario: Carry a backend block header as reclaim authority

- **WHEN** a backend represents an allocation's reclaim authority as the address of its own block header rather than as a null placeholder
- **THEN** the program observes no difference in the allocation's type, lanes, or behavior, and still cannot name, read, or construct that authority

### Requirement: Raw typed storage is narrow and unsafe

The language SHALL expose `RawBuffer<T>` as an affine typed view over one `Allocation` and one
compiler-validated repeated-element layout. Qualified unsafe operations SHALL construct a buffer,
project one bounds-checked lexical `Slot<T>`, initialize an uninitialized slot, move from or destroy
an initialized slot, inspect the logical count, and copy one initialized recursively Copy element
through a shared buffer borrow. Supported elements SHALL include structural unions exactly when all
members are Copy and cleanup-free. A shared copy MUST NOT expose a Slot, move or mutate the buffer,
change initializedness or cleanup state, or allocate. The compiler SHALL validate canonical
type/layout provenance, checked index ordering, slot non-escape, exclusive owner liveness, shared
access, and legal element operations. Unsafe Silk code remains responsible for runtime
initializedness and aliasing invariants; the runtime and compiler MUST NOT add a collection-shaped
initialization bitmap.

#### Scenario: Reject a slot after its buffer moves

- **WHEN** unsafe code retains a projected slot and attempts to move the backing raw buffer
- **THEN** ownership rejects the move before MIR or backend emission

#### Scenario: Reject mismatched typed provenance

- **WHEN** unsafe code attempts to form `RawBuffer<Token>` from a repeated layout planned for another canonical type
- **THEN** semantic analysis rejects the construction and publishes no usable buffer or slot fact

#### Scenario: Keep initializedness an unsafe obligation

- **WHEN** unsafe code reads or takes a value from a slot that its own runtime state has not initialized
- **THEN** the program violates the unsafe operation contract without gaining a compiler-promised initialization bitmap or safe behavior

#### Scenario: Read an all-Copy structural union through two shared aliases

- **WHEN** unsafe code reads one initialized all-Copy union element through each of two live shared borrows of the same raw buffer
- **THEN** both reads preserve the stored active member and payload while buffer ownership, initializedness, and cleanup state remain unchanged

#### Scenario: Reject a shared read of a move-only element

- **WHEN** unsafe code requests a shared raw-buffer read for a move-only nominal or structural-union element type
- **THEN** compiler verification rejects the intrinsic instance before evaluation or backend emission

### Requirement: Drop is synchronous infallible and deterministic

An affine nominal type MAY declare exactly one restricted `Drop` hook. The hook SHALL be
synchronous, infallible, non-allocating, requirement-free, unable to move from or replace `self`,
and unable to let a borrow of `self` escape. It SHALL run once before automatic field cleanup.
Locals SHALL clean in reverse acquisition order and fields in fixed declaration order on
fallthrough, `return`, `break`, `continue`, typed failure, or explicit consuming `drop`; traps carry
no cleanup guarantee. A hook MUST NOT be invoked as an ordinary function.

#### Scenario: Roll back initialized elements on failure

- **WHEN** construction fails after initializing a prefix of move-only elements guarded by an affine owner
- **THEN** the guard's Drop destroys exactly that prefix before the backing allocation releases and the original typed failure propagates unchanged

#### Scenario: Drop early exactly once

- **WHEN** source explicitly drops a live allocation before its lexical block ends
- **THEN** cleanup releases it at that statement and automatic block cleanup does not release it again

#### Scenario: Reject an effectful Drop hook

- **WHEN** a Drop declaration may fail, allocate, require a capability, move from `self`, or escape a self borrow
- **THEN** semantic analysis rejects the declaration before it can enter HIR

### Requirement: Vector is ordinary Silk code

`Vector<T>` SHALL be implemented in the Silk standard library from raw owned storage, length,
capacity, unsafe initialized-slot operations, and Drop. Growth SHALL allocate a replacement buffer,
move or copy exactly the initialized elements, commit only after success, and then drop the old
buffer; the compiler and backends MUST NOT contain collection-shaped allocation primitives.

#### Scenario: Preserve a vector after failed growth

- **WHEN** append requires growth and the replacement allocation fails
- **THEN** append fails with `OutOfMemory`, the original vector retains its prior elements and capacity, and no element or allocation is leaked or dropped twice

### Requirement: Allocator implementations receive no compiler privilege

The compiler SHALL dispatch allocation only through the general nominal capability, role, and
conformance model. Bootstrap SHALL provide `SystemAllocator`, but semantic, HIR, MIR, evaluator, and
backend behavior MUST NOT branch on its nominal implementation type. A future arena or other policy
may participate only by satisfying the same public contract without compiler-known lifetime rules;
provider-dependent escaping allocation remains unavailable.

#### Scenario: Reject allocator-kind branching

- **WHEN** semantic, MIR, evaluator, or backend artifacts are inspected for a system allocation
- **THEN** they describe capability dispatch and reclaim authority without an allocator-kind tag or Arena-specific rule

#### Scenario: Select allocator roles ordinarily

- **WHEN** one Effect requires `Allocator@Scratch` and `Allocator@Durable`
- **THEN** each allocation resolves through its statically selected general capability slot while both successful owners remain independent of the provider values

### Requirement: Allocation policy is source-defined over primitive storage operations

`Allocator` SHALL be an ordinary source-declared service and `SystemAllocator` SHALL be an ordinary
service implementation. The compiler SHALL expose only the primitive operations needed to acquire,
adopt, access, and release storage while preserving ownership and automatic cleanup. Layout
validation, allocation policy, provider construction, safe buffer APIs, and reusable collection
behavior MUST remain in shipped Silk source.

#### Scenario: Allocate through the source service

- **WHEN** a program calls the standard-library Allocator operation with a provided SystemAllocator
- **THEN** service dispatch reaches the source implementation and only its irreducible storage operation lowers as an intrinsic

#### Scenario: Use a pure allocator implementation

- **WHEN** a source-defined quota allocator satisfies Allocator without acquiring platform storage
- **THEN** it uses the same service contract and needs no allocator-specific compiler branch

### Requirement: Unsafe storage primitives remain narrow

Intrinsic operations that adopt allocation ownership or access storage with unproved
initializedness SHALL require unsafe boundaries. Safe source wrappers SHALL validate layout,
bounds, ownership, and initialized-element rules before exposing ordinary values. Query operations
whose contracts cannot violate safe-code invariants SHALL remain safe.

#### Scenario: Reject unchecked adoption in safe code

- **WHEN** source attempts to adopt a primitive allocation without an unsafe boundary
- **THEN** analysis rejects the intrinsic call

### Requirement: Continuation storage uses explicit typed allocation

Each `RunSuspendableEffect` that observes transfer and needs distinct post-child resume state SHALL
derive one validated physical target layout from its finalized post-normalization logical payload
and request that complete size and alignment through the active exclusive `Allocator` service
requirement. Resume state includes control-only state even when no value payload is live. A tail
relay that can propagate the transferred child outcome unchanged without any later work MAY add no
frame. The explicit `SuspendEffect` origin SHALL create the transfer but SHALL NOT allocate merely
for originating it. Physical private headers MAY differ by engine. A successful request SHALL
produce one self-contained affine reclaim obligation owned privately by that continuation;
exhaustion SHALL produce the allocation-free typed failure `OutOfMemory`. A suspendable runner path
that completes synchronously or does not reach the origin MUST NOT allocate merely because it could
suspend. The compiler and backends MUST NOT use a hidden allocation or allocator, select an
allocator implementation, convert exhaustion to a trap, or add allocation failure and allocator
requirements to Effects that cannot reach suspension. A provider implementation selected to
allocate continuation storage MUST have a closed call graph that cannot itself reach suspension;
analysis SHALL reject a selected provider that would recursively require continuation storage in
order to allocate continuation storage.

An explicit suspension SHALL originate an unpublished transfer without starting its deferred child.
As that transfer returns through suspendable callers, each caller that has live post-resume state
SHALL allocate and initialize its own continuation exactly once and prepend it to the unpublished
chain. The driver SHALL publish the completed chain and start the deferred child only after every
required caller continuation is initialized. Any refusal SHALL roll back the entire unpublished
prefix and return `OutOfMemory` without starting the child. A call to a suspendable runner that
completes synchronously or takes a branch that never reaches explicit suspension SHALL allocate no
continuation merely because transfer was possible.

#### Scenario: Observe continuation allocation through a source allocator

- **WHEN** a suspended recursive Effect runs with a source-defined counting allocator
- **THEN** every successful continuation request and release is observed through that allocator's ordinary service contract

#### Scenario: Do not allocate for an untaken suspension branch

- **WHEN** a suspendable Effect runs through a branch that does not reach `Effect.suspend`
- **THEN** the selected allocator observes no continuation request for that invocation

#### Scenario: Complete a suspendable runner synchronously

- **WHEN** a runner classified suspendable takes a dynamic branch that returns typed success or failure without reaching `SuspendEffect`
- **THEN** its caller observes the unchanged outcome, ordinary current-activation cleanup, and zero continuation requests, publications, resume entries, or reclaims

#### Scenario: Relay transfer through a source combinator

- **WHEN** a suspendable child transfers through an ordinary combinator that retains mapper state after the child
- **THEN** the combinator allocates its caller continuation while relaying transfer, and a synchronous completion of the same child path allocates no such continuation

#### Scenario: Allocate one frame for every stateful relay

- **WHEN** one explicit suspension transfers through two ordinary callers that each need distinct post-child resume state
- **THEN** the selected allocator observes exactly two continuation requests in inner-to-outer relay order and observes no request for the transfer origin itself

#### Scenario: Elide a tail relay frame

- **WHEN** an ordinary suspendable caller can relay the transferred child and its typed outcome unchanged with no post-child work or retained state
- **THEN** it adds no continuation request or frame to the unpublished chain

#### Scenario: Refuse an outer relayed continuation

- **WHEN** an inner continuation is initialized but allocation of an outer caller continuation is refused before publication
- **THEN** the unpublished inner continuation rolls back exactly once, the deferred child does not start, and the caller receives unchanged `OutOfMemory`

#### Scenario: End allocator access before running the child

- **WHEN** continuation allocation succeeds and the deferred Effect also requires exclusive allocator access
- **THEN** the allocation call's provider loan ends before the deferred child starts, while the continuation retains only self-contained reclaim authority

#### Scenario: Reject one continuation request atomically

- **WHEN** the selected allocator refuses a valid continuation layout at any frame-producing relay ordinal
- **THEN** that request creates no owner, every earlier unpublished continuation rolls back and reclaims exactly once, the refusing activation retains its untransferred values, and `OutOfMemory` returns without starting the deferred child or publishing a partial chain

#### Scenario: Keep synchronous Effects allocation-free

- **WHEN** an Effect call graph cannot reach the suspension intrinsic
- **THEN** no continuation storage request is made and no `OutOfMemory` or `Allocator` row is added because suspension exists elsewhere in the program

#### Scenario: Reject a recursively suspending continuation allocator

- **WHEN** the allocator implementation selected for continuation storage can reach `Effect.suspend`
- **THEN** analysis rejects that provider selection before MIR or backend emission

### Requirement: Continuation storage releases through captured authority

A successfully initialized continuation SHALL retain its own unforgeable reclaim authority and
MUST NOT retain or rediscover the allocator provider after the allocation call ends. Its storage
SHALL release exactly once after its live values have been moved onward or cleaned, including when
the resumed computation returns normally or propagates typed failure. A target trap retains the
existing no-cleanup guarantee.

#### Scenario: Release after provider borrow ends

- **WHEN** a continuation remains active after its allocator call's provider borrow ends
- **THEN** eventual cleanup releases storage exactly once through captured reclaim authority without consulting the provider again
