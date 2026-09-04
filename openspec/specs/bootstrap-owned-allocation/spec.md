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
requirement and return either the allocation-free typed failure `OutOfMemoryError` or one affine
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
- **THEN** the Effect fails with `OutOfMemoryError` without allocating the failure, publishing an allocation owner, or scheduling cleanup for the rejected request

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
- **THEN** compiler verification rejects the intrinsic instance before backend emission

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
- **THEN** append fails with `OutOfMemoryError`, the original vector retains its prior elements and capacity, and no element or allocation is leaked or dropped twice

### Requirement: Allocator implementations receive no compiler privilege

The compiler SHALL dispatch allocation only through the general nominal capability, role, and
conformance model. Bootstrap SHALL provide `SystemAllocator`, but semantic, HIR, MIR, and
backend behavior MUST NOT branch on its nominal implementation type. A future arena or other policy
may participate only by satisfying the same public contract without compiler-known lifetime rules;
provider-dependent escaping allocation remains unavailable.

#### Scenario: Reject allocator-kind branching

- **WHEN** semantic, MIR, or backend artifacts are inspected for a system allocation
- **THEN** they describe capability dispatch and reclaim authority without an allocator-kind tag or Arena-specific rule

#### Scenario: Select allocator roles ordinarily

- **WHEN** one Effect requires `Allocator at Scratch` and `Allocator at Durable`
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

### Requirement: Local shared control blocks use exact caller-funded allocation

The language SHALL provide a target-selected validated `Layout` for one local shared control block
over concrete `T`. The layout SHALL cover every private header field, padding, reclaim state, and
the initialized value, and SHALL remain nonzero for zero-sized `T`. Ordinary source SHALL request
that layout through its selected allocator before initialization; construction MUST NOT acquire an
allocator implicitly or retain the provider borrow in the result.

If the concrete header, padding, reclaim state, and `T` cannot be represented by the selected
target, the `sharedLayout<T>` specialization SHALL be unavailable before MIR or execution and SHALL
retain a stable diagnostic whose primary span is the intrinsic call. This compile-time target-layout
rejection MUST NOT become runtime `LayoutOverflow` data, a trap, or `OutOfMemoryError`.

An unsafe from-allocation transition SHALL consume exactly one active `Allocation` proven to match
the layout and exactly one value of `T`, initialize count one and available access, retain the
allocation's private reclaim authority, and publish exactly one affine local-shared core. A valid
transition SHALL have no typed failure and MUST NOT expose partial state. The reclaim authority SHALL
remain unnameable and usable only by eventual last-handle cleanup.

#### Scenario: Initialize from the exact requested layout

- **WHEN** ordinary source allocates the result of `sharedLayout<Token>()` and supplies that allocation and one `Token` to the unsafe initializer
- **THEN** one initialized core owns count one, the token, and the private reclaim authority while the allocation and source token bindings are consumed

#### Scenario: Reject mismatched layout provenance

- **WHEN** unsafe source supplies an allocation planned for another concrete type, target, size, or alignment
- **THEN** semantic or MIR verification rejects the initializer with a stable diagnostic at the initializer call and related provenance at the mismatched allocation, and publishes no usable local-shared core

#### Scenario: Preserve ordinary exhaustion cleanup

- **WHEN** the selected allocator rejects the control-block layout with `OutOfMemoryError`
- **THEN** no initializer runs, no shared core or reclaim obligation exists, and ordinary typed-failure cleanup cleans the still-owned value exactly once

#### Scenario: Allocate a zero-sized element control block

- **WHEN** `T` has zero size
- **THEN** the requested layout still contains distinct stable control-block storage and private lifetime state

#### Scenario: End allocator access before shared lifetime

- **WHEN** construction returns after the allocator provider loan has ended
- **THEN** the core remains valid and its eventual last cleanup can release storage through the retained self-contained authority

### Requirement: One Allocation is the indivisible execution reclaim authority

The exact Execution initializer SHALL consume one self-contained Allocation and transfer its private
reclaim authority into one indivisible combined package. No separately reclaimable body, endpoint,
wake header, or initial segment SHALL be created by the intrinsic. Until external Wake retention is
introduced, completion or ordinary drop of the sole Execution SHALL release the package exactly
once after all live values are cleaned. A safe source wrapper SHALL procure the Allocation through
ordinary allocator policy and expose any typed failure before the initializer.

#### Scenario: Transfer one allocation owner

- **WHEN** a valid initializer consumes the matching Allocation
- **THEN** the source allocation binding ends and exactly one Execution package retains its reclaim authority

#### Scenario: Release after completion

- **WHEN** a running execution completes and no Wake can remain
- **THEN** all live package values are cleaned and the same transferred Allocation is released exactly once

#### Scenario: Roll back refused procurement

- **WHEN** source allocation fails before initialization
- **THEN** no reclaim authority transfers and source cleanup remains solely responsible for the body and endpoint values

#### Scenario: Forbid a hidden second package

- **WHEN** package construction is inspected for a parking-capable specialization
- **THEN** it contains one Allocation owner and no intrinsic-created second allocation or allocator access
