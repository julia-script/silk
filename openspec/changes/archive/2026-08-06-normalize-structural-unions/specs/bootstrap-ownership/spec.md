## ADDED Requirements

### Requirement: Union ownership derives from every nominal member

Ownership analysis SHALL classify a union as Copy only when every nominal member is Copy and
cleanup-free. Otherwise the union SHALL be one complete move-only owner whose injection, widening,
binding, storage, assignment, call, and return obey ordinary whole-value move rules. A conversion
MUST NOT duplicate, partially move, or expose the active payload.

#### Scenario: Move a payload into a union

- **WHEN** a move-only `Token` is injected and returned as `Token | End`
- **THEN** ownership transfers the complete `Token` obligation into the returned union and marks the source consumed

#### Scenario: Widen without duplicating ownership

- **WHEN** a move-only `Token | End` value widens to `Token | End | Fault`
- **THEN** the target receives the single active payload and the source union becomes unavailable

### Requirement: Union cleanup follows the active member exactly

The cleanup plan SHALL retain one union-owner release whose member cases are ordered by canonical
identity and whose runtime execution releases exactly the active payload according to that member's
ordinary recursive cleanup. Inactive members SHALL perform no cleanup, and moves, replacement,
loop transfers, returns, and traps MUST NOT duplicate the union obligation.

#### Scenario: Clean one active aggregate member

- **WHEN** a `Token | End` owner containing `Token` leaves scope
- **THEN** cleanup releases the `Token` fields in their ordinary order and performs no `End` cleanup

#### Scenario: Replace a mutable union

- **WHEN** assignment replaces an owned union containing `Token` with one containing `End`
- **THEN** the old active `Token` is cleaned once before the new complete union commits

