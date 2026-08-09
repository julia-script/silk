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
containing everything required for infallible release. It MUST NOT borrow, retain, or later
rediscover the provider that created it, and failed allocation MUST NOT create storage or reclaim
authority.

#### Scenario: Drop after provider borrow ends

- **WHEN** an allocation escapes the call that borrowed `SystemAllocator`
- **THEN** the provider borrow ends, the allocation remains valid, and its eventual Drop releases through its captured reclaim authority exactly once

#### Scenario: Fail atomically under exhaustion

- **WHEN** the selected allocator cannot satisfy one valid layout
- **THEN** the Effect fails with `OutOfMemory` without allocating the failure, publishing an allocation owner, or scheduling cleanup for the rejected request

#### Scenario: Transfer allocation ownership

- **WHEN** an allocation moves through an ordinary function and its original binding leaves scope
- **THEN** only the destination remains live and eventual cleanup consumes the same reclaim authority exactly once

### Requirement: Raw typed storage is narrow and unsafe

The language SHALL expose `RawBuffer<T>` as an affine typed view over one `Allocation` and one
compiler-validated repeated-element layout. Qualified unsafe operations SHALL construct a buffer,
project one bounds-checked lexical `Slot<T>`, initialize an uninitialized slot, move from or destroy
an initialized slot, inspect the logical count, and copy one initialized non-union Copy element
through a shared buffer borrow. The shared copy operation MUST NOT expose a Slot, move or mutate the
buffer, change initializedness or cleanup state, or allocate. The compiler SHALL validate canonical
type/layout provenance, checked index ordering, slot non-escape, exclusive owner liveness, shared
access, and legal element operations. Unsafe Silk code remains responsible for the runtime
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

#### Scenario: Read through two shared aliases

- **WHEN** unsafe code reads one initialized non-union Copy element through each of two live shared borrows of the same raw buffer
- **THEN** both reads return the stored value while buffer ownership, initializedness, and cleanup state remain unchanged

#### Scenario: Reject a shared read of an unsupported element

- **WHEN** unsafe code requests a shared raw-buffer read for a move-only or structural-union element type
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
