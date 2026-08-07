## ADDED Requirements

### Requirement: Allocation operations specialize by canonical value type

Instance discovery SHALL reach concrete layout, slot access, restricted drop-hook, and cleanup behavior
through allocation operations. Generic typed-slot operations SHALL specialize by canonical concrete
`T`; runtime requested counts, allocator implementations, allocation ordinals, and scope identities
MUST NOT create distinct function instances.

#### Scenario: Reuse one typed allocator helper

- **WHEN** one generic helper allocates slots for the same concrete `Token` type using different runtime counts and destination scopes
- **THEN** discovery produces one helper instance while retaining the distinct runtime scope and count operands
