## MODIFIED Requirements

### Requirement: Union values obey affine ownership

Moving an affine nominal payload into an owned union SHALL consume that payload. A union SHALL be
Copy only when every member has the compiler's sealed Copy property; copying that union SHALL
preserve exactly one canonical active member and its complete payload without consuming or mutating
the source. Otherwise the union SHALL remain one affine owner. Borrowed values SHALL NOT be stored
as union members, and cleanup SHALL act on exactly the active payload once.

#### Scenario: Copy an explicitly all-Copy union

- **WHEN** every nominal member of a structural union validly declares `impl Copy`
- **THEN** the union and its source retain the same active member and neither acquires cleanup

#### Scenario: Reject structural Copy inference for a union member

- **WHEN** one nominal member contains only Copy fields but declares no `impl Copy`
- **THEN** the complete union remains affine

#### Scenario: Consume an injected owner

- **WHEN** an affine `Token` is injected into `Token | End`
- **THEN** the original owner becomes unavailable and the union owns the complete `Token`

#### Scenario: Reject a partly affine union copy

- **WHEN** one member of a structural union owns an affine or Drop-bearing field
- **THEN** the complete union remains affine and a requested whole-value copy is rejected

#### Scenario: Reject a stored borrow

- **WHEN** a contextual conversion attempts to inject a shared or exclusive borrow into an owned union
- **THEN** ownership rejects the conversion without fabricating an owned payload
