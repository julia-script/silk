## ADDED Requirements

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
