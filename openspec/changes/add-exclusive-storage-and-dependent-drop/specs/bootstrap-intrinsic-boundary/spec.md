## ADDED Requirements

### Requirement: Raw storage preserves dependent provenance

RawBuffer and Slot operations SHALL preserve full payload types and lifetime constraints, affine transfer, shared Copy eligibility and owner-backed view provenance. RawBuffer<T> is invariant in T. Slot<'storage, T> is covariant in its exclusive storage-access lifetime and invariant in T; omitted storage lifetimes follow nominal header/local elision. Taking or copying T does not retain the outer slot-access lifetime unless T itself depends on that storage. Raw storage callers remain responsible for bounds, initializedness, aliasing and cleanup before release. No compiler-known collection bitmap or library-name recognition is permitted.

#### Scenario: Extraction retains external payload validity

- **WHEN** an initialized dependent element is taken from raw storage
- **THEN** the element retains its external dependencies independently of the allocation, while a view into that allocation cannot outlive its owner

#### Scenario: Reject a slot escape or short-payload write

- **WHEN** a slot is returned beyond its buffer lifetime or a shorter-lived payload is written through an outer-shortened slot
- **THEN** checking rejects the violated storage lifetime or invariant payload type
