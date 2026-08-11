## ADDED Requirements

### Requirement: Intrinsic availability metadata is sealed and auditable

The canonical intrinsic inventory SHALL record a normalized supported-target set for every callable
operation and SHALL expose that data to executable planning, tooling, and inventory tests. Ordinary
source declarations MUST NOT override, infer, or attach compiler target privilege by spelling.

#### Scenario: Audit one restricted operation

- **WHEN** the intrinsic inventory is encoded for tests or tooling
- **THEN** the operation's canonical identity and sorted supported-target set appear in deterministic form

#### Scenario: Refuse source-defined target privilege

- **WHEN** ordinary source declares a function or service using the same name as a restricted intrinsic
- **THEN** it remains an ordinary declaration with no compiler availability metadata

