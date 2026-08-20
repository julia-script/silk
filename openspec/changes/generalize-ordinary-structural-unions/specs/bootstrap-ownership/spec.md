## RENAMED Requirements

- FROM: `Union ownership derives from every nominal member`
- TO: `Union ownership derives from every normalized member`

## MODIFIED Requirements

### Requirement: Union ownership derives from every normalized member

Ownership analysis SHALL classify a union as Copy only when every normalized member is Copy and
cleanup-free. Otherwise the union SHALL be one complete move-only owner whose injection, widening,
binding, storage, assignment, call, and return obey ordinary whole-value move rules. A conversion
MUST NOT duplicate, partially move, or expose the active payload.

#### Scenario: Move a payload into a union

- **WHEN** a move-only `Token` is injected and returned as `Token | End`
- **THEN** ownership transfers the complete `Token` obligation into the returned union and marks the source consumed

#### Scenario: Derive Copy from non-nominal members

- **WHEN** every member of `i32 | Array<i32, 2>` is Copy and cleanup-free
- **THEN** the union is Copy without requiring nominal declarations or user-written conformance

#### Scenario: Widen without duplicating ownership

- **WHEN** a move-only `Token | End` value widens to `Token | End | Fault`
- **THEN** the target receives the single active payload and the source union becomes unavailable
