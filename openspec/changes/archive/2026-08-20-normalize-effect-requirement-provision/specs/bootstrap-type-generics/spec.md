## ADDED Requirements

### Requirement: Requirement selectors name access-independent keys

A requirement-row key SHALL consist of one canonical service identity and one canonical nominal
role identity. Omitting `at Role` SHALL select `DefaultRole`. Shared and exclusive access SHALL be
stored as the demand associated with a key and SHALL NOT participate in key identity. Union SHALL
retain the strongest demand for colliding keys. `Without<R, K>` SHALL accept the selector
`Service` or `Service at Role` and remove the complete matching key regardless of its demand.

#### Scenario: Merge repeated access demands

- **WHEN** a requirement union contains `&Clock at Primary | &mut Clock at Primary`
- **THEN** it normalizes to the single key `Clock at Primary` with exclusive demand

#### Scenario: Subtract an exclusive requirement by key

- **WHEN** `Without<&mut Clock at Primary | &Logger, Clock at Primary>` is specialized
- **THEN** it normalizes to `&Logger`

#### Scenario: Keep independently declared roles distinct

- **WHEN** two modules each declare a visible role named `Primary`
- **THEN** their canonical identities remain distinct and same-spelled selectors do not collide
