## ADDED Requirements

### Requirement: Function headers publish flow kind and failure contracts

Declaration indexing SHALL retain whether each declaration is ordinary or flow and SHALL resolve
every declared failure member to canonical nominal identity. Damaged, non-nominal, inaccessible, or
unknown members SHALL remain explicit unavailable facts with their originating diagnostics.

#### Scenario: Index a public flow contract

- **WHEN** a public flow declares a normalized row of imported nominal errors
- **THEN** its header exposes the flow kind and canonical row independently of body analysis order
