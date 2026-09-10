## MODIFIED Requirements

### Requirement: Union values obey affine ownership

Moving an affine payload into an owned union SHALL consume that payload. A union SHALL be Copy only
when every normalized member has the compiler's sealed Copy property; copying that union SHALL
preserve exactly one canonical active member and its complete payload without consuming or mutating
the source. Otherwise the union SHALL remain one affine owner. Shared borrowed values SHALL retain their lifetime arguments as ordinary union members. Exclusive stored views SHALL remain gated until their storage checker is admitted. Cleanup SHALL act on exactly the initialized active payload remainder once.

#### Scenario: Consume an injected owner

- **WHEN** an affine `Token` is injected into `i32 | Token`
- **THEN** the original owner becomes unavailable and the union owns the complete `Token`

#### Scenario: Copy an explicitly all-Copy union

- **WHEN** every normalized member of `i32 | Array<i32, 2>` has sealed Copy evidence
- **THEN** the copy and source retain the same canonical active member and complete payload and neither acquires a cleanup obligation

#### Scenario: Reject structural Copy inference for a union member

- **WHEN** one nominal member contains only Copy fields but declares no `impl Copy`
- **THEN** the complete union remains affine

#### Scenario: Reject a partly affine union copy

- **WHEN** one member of a structural union owns an affine or Drop-bearing field
- **THEN** the complete union remains affine and a requested whole-value copy is rejected

#### Scenario: Retain a stored shared borrow

- **WHEN** a contextual conversion attempts to inject a shared borrow into an owned union
- **THEN** ownership accepts the shared payload and preserves its lifetime and loan obligations through the union

#### Scenario: Clean the active ordinary member

- **WHEN** a union holding a droppable non-nominal member leaves scope
- **THEN** every supported target cleans exactly that active payload once

#### Scenario: Reject a stored borrow

- **WHEN** an injected shared borrow would outlive its source through the union
- **THEN** ownership rejects the invalid escape while retaining the lifetime-bearing union contract
