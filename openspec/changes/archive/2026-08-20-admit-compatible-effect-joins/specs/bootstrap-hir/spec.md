## ADDED Requirements

### Requirement: HIR retains closed finite Effect alternatives

HIR SHALL represent an admitted Effect join as one closed finite composite that names every exact
construction alternative, its normalized public contract, capture access, and ownership facts. The
representation and its textual encoding SHALL be deterministic and SHALL NOT erase the alternatives
to a universal runtime Effect identity.

#### Scenario: Encode two construction alternatives

- **WHEN** control flow joins two compatible Effects constructed at distinct source sites
- **THEN** HIR records both exact alternatives in canonical order under one normalized Effect contract

#### Scenario: Retain the selected capture contract

- **WHEN** alternatives capture different values with compatible run access and ownership
- **THEN** HIR preserves enough information to construct, run, and clean only the selected alternative
