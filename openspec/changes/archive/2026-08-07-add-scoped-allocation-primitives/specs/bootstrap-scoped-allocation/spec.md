## Purpose

Define the smallest explicit memory substrate from which unsafe Silk code can build owned dynamic
values while safe callers retain deterministic reclamation and typed failure behavior.

## ADDED Requirements

### Requirement: Layout values are validated and target-aware

`Layout` SHALL be a Copy nominal value containing a representable `Usize` byte size and a validated
power-of-two `Usize` alignment for the selected target. Construction SHALL accept zero size and
every representable target alignment and reject zero or non-power-of-two alignment as ordinary
validation data. `SlotLayout<T>` SHALL pair one concrete element type and layout with a runtime
logical count and the element's aligned stride. Its total byte size SHALL be checked `stride *
count`, including tail padding, and zero when count is zero or the element size is zero.

#### Scenario: Form a repeated element layout

- **WHEN** a valid element layout is repeated by a runtime count whose padded size is representable
- **THEN** `SlotLayout<T>` records the concrete element identity, logical count, checked total size, element alignment, and aligned stride without consulting a backend

#### Scenario: Reject representational overflow

- **WHEN** repeated-element layout multiplication or padding exceeds the selected target's addressable size
- **THEN** layout formation returns ordinary overflow validation data and performs no allocation

#### Scenario: Preserve a zero-sized layout

- **WHEN** a zero-sized layout with valid alignment is constructed or repeated
- **THEN** it remains a valid layout and retains its required alignment and logical element count

### Requirement: Allocation is explicit, unsafe, scoped, and fallible

The sole primitive storage acquisition operation SHALL require an explicit allocator capability and
an explicit active destination scope, accept only a valid `Layout`, and be callable only within an
unsafe boundary. Success SHALL produce one affine owned allocation; exhaustion SHALL produce typed
`OutOfMemory` without creating a live allocation or cleanup record. Omitting a role SHALL select the
nominal `DefaultRole`, never whichever allocator happens to be visible, and no ambient or static
allocator SHALL be synthesized.

#### Scenario: Allocate in a named scope

- **WHEN** unsafe code allocates a valid layout using an explicit allocator role and active destination scope
- **THEN** it receives one affine allocation whose maximum lifetime is that scope

#### Scenario: Surface exhaustion as typed failure

- **WHEN** the selected allocator cannot satisfy a valid request
- **THEN** the operation fails with `OutOfMemory`, registers no cleanup, and leaves prior live allocations unchanged

#### Scenario: Refuse an absent allocator

- **WHEN** a function reaches allocation without the required allocator provider
- **THEN** its contract remains unsatisfied rather than receiving an implicit static or ambient allocator

### Requirement: Every allocation retains its reclaim origin

Each allocation SHALL carry a private unforgeable reclaim ticket sufficient to invoke the release
behavior of the allocator that created it. The creating provider MUST outlive the destination scope.
Automatic cleanup SHALL use the retained ticket rather than resolving the currently provided
allocator, and source code MUST NOT inspect, forge, replace, or invoke the ticket as public `free`.

#### Scenario: Reclaim after provider shadowing

- **WHEN** another allocator for the same role is provided after an allocation is created and the owner then ends
- **THEN** cleanup invokes the original allocator's release behavior exactly once

#### Scenario: Reject a short-lived provider

- **WHEN** an allocation targets a scope that outlives its allocator provider
- **THEN** semantic or ownership analysis rejects the allocation before lowering

### Requirement: Typed slots are unsafe lexical places

Selecting slot `index` from an allocation SHALL require a matching `SlotLayout<T>`, a checked runtime
bound, and an unsafe boundary, and SHALL create a lexical exclusive `Slot<T>` place borrowing that
allocation. The compiler SHALL verify element type, layout provenance, bounds-check structure, and
that the allocation remains live and immovable for the borrow. Unsafe code, not a hidden runtime
bitmap or static dependent-type analysis, SHALL own the invariants that runtime-selected slots do
not alias and are initialized before read, move, or drop. Slot values or borrows MUST NOT escape into
safe owned storage.

#### Scenario: Initialize one typed slot

- **WHEN** unsafe code selects a checked `Slot<T>` and writes one complete owned `T`
- **THEN** the value resides at the compiler-planned stride and the unsafe abstraction may expose it under ordinary ownership rules while the allocation remains live

#### Scenario: Refuse an uninitialized read

- **WHEN** safe code attempts to construct, retain, or read a `Slot<T>` directly
- **THEN** analysis rejects the unsafe storage operation and publishes no safe value fact

#### Scenario: Release unused capacity

- **WHEN** an owned allocation containing never-initialized slots is reclaimed
- **THEN** the storage is released without running `T` cleanup for those slots

### Requirement: Cleanup is deterministic and collection policy stays in Silk

Successful allocation SHALL atomically create one private stable cleanup control block linked to its
destination scope. Moving the affine handle SHALL leave that record stable; explicit or lexical
owner cleanup SHALL release the raw bytes through the originating reclaim capability and disarm the
record, while scope closure SHALL walk records in reverse acquisition order and skip disarmed ones.
Release SHALL never infer or run `T` cleanup for bytes merely because they were accessed through a
slot. A higher affine owner SHALL drop or move its initialized values first.

An affine struct MAY declare one restricted compiler-invoked drop hook that runs before derived field
cleanup. The hook SHALL be typed-infallible, non-allocating, requirement-free, unable to move from or
replace `self` or its fields, and unable to escape a borrow of `self`. General capturing finalizers,
user-callable `free`, and fallible automatic cleanup SHALL NOT be exposed.

#### Scenario: Exit after successful work

- **WHEN** multiple live allocations reach their structured scope boundary
- **THEN** they release exactly once in reverse acquisition order

#### Scenario: Exit after typed failure

- **WHEN** a typed failure leaves a scope containing live allocations
- **THEN** the same reverse-acquisition cleanup completes before the failure propagates

#### Scenario: Drop early

- **WHEN** code explicitly consumes an allocation with `drop` before its scope closes
- **THEN** release runs once at that point and scope closure skips the consumed cleanup record

#### Scenario: Drop initialized elements before bytes

- **WHEN** a Silk-written affine owner tracks an initialized slot prefix and reaches automatic cleanup
- **THEN** its restricted drop hook drops that prefix in the owner's documented order before derived cleanup releases the allocation field's bytes

#### Scenario: Clean only regions exited by a loop transfer

- **WHEN** `continue` exits a loop-body region containing an allocation while another allocation is owned outside the loop
- **THEN** cleanup releases the body allocation and preserves the outer allocation for the next iteration
