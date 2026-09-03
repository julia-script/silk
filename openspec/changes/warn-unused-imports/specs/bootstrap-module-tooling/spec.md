# Delta: module tooling

## ADDED Requirements

### Requirement: Safe unused-import removal plan
For each unused valid binding, tooling SHALL offer a snapshot-bound SourceAction only when selector/delimiter or whole-declaration ownership is deterministic. Comments in the owned range SHALL produce warning-only behavior.

#### Scenario: Mixed member list
- **WHEN** the last unused selector follows a used selector
- **THEN** its action removes only the delimiter and unused selector
