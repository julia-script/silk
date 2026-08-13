## MODIFIED Requirements

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
