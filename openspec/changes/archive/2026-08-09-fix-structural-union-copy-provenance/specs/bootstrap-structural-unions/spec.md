## MODIFIED Requirements

### Requirement: Union values obey affine ownership

Moving a non-Copy nominal payload into an owned union SHALL consume that payload. A union SHALL be
Copy only when every member is recursively Copy and cleanup-free; copying that union SHALL preserve
exactly one canonical active member and its complete payload without consuming or mutating the
source. Otherwise the union SHALL remain one move-only owner. Borrowed values SHALL NOT be stored as
union members, and cleanup SHALL act on exactly the active payload once.

#### Scenario: Consume an injected owner

- **WHEN** a move-only `Token` is injected into `Token | End`
- **THEN** the original owner becomes unavailable and the union owns the complete `Token`

#### Scenario: Copy an all-Copy union

- **WHEN** a `Step | VmDiagnostic` value whose two nominal members contain only Copy fields is copied
- **THEN** the copy and source retain the same canonical active member and complete payload and neither acquires a cleanup obligation

#### Scenario: Reject a partly move-only union copy

- **WHEN** one member of a structural union owns a move-only or Drop-bearing field
- **THEN** the complete union remains move-only and a requested whole-value copy is rejected

#### Scenario: Reject a stored borrow

- **WHEN** a contextual conversion attempts to inject a shared or exclusive borrow into an owned union
- **THEN** ownership rejects the conversion without fabricating an owned payload
