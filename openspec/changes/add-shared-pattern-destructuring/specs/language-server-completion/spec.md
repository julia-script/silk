## ADDED Requirements

### Requirement: Pattern-local completion follows visible semantic scope

Expression completion SHALL include shared pattern bindings only where their semantic scope is
active. Irrefutable let bindings SHALL appear after their declaration; match-arm and if-let
bindings SHALL appear only in the selected body and SHALL NOT appear in a mismatch body or after
their lexical scope.

#### Scenario: Complete inside if-let

- **WHEN** completion is requested inside the taken and mismatch bodies of one if-let
- **THEN** the pattern binding appears only in the taken-body result
