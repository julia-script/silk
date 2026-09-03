## ADDED Requirements

### Requirement: Evaluation refuses unsupported callback and data-symbol operations before entry

Bootstrap evaluation SHALL detect reachable C callback conversions and foreign data-symbol reads
before executing the entry. It SHALL return the foreign-surface blocked outcome with an empty trace
and SHALL NOT synthesize process addresses, native globals, or ambient callback registrations.

#### Scenario: Block a foreign static read

- **WHEN** the executable closure reads an imported or exported C static during evaluation
- **THEN** evaluation blocks before entry with an empty trace naming the data symbol

#### Scenario: Block a callback conversion

- **WHEN** the executable closure converts an exported function to a C callback during evaluation
- **THEN** evaluation blocks before entry with an empty trace naming the exported callback symbol
