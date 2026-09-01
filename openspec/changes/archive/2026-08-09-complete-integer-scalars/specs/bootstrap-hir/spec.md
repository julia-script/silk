## ADDED Requirements

### Requirement: HIR carries the complete integer vocabulary

HIR SHALL retain canonical lowercase integer identities, unit/bottom control flow, exact literal magnitude, conversion identity, operation mode, evaluation order, and provenance without host-number or backend-lane approximations.

#### Scenario: Encode a wide integer

- **WHEN** accepted `u64` source contains a value above JavaScript's exact integer range
- **THEN** HIR encodes the exact magnitude and canonical `u64` type

#### Scenario: Encode unit return

- **WHEN** a unit function executes bare `return`
- **THEN** HIR records unit completion with no scalar payload
