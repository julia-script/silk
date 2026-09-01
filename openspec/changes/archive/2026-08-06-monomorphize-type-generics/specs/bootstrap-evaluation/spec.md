## ADDED Requirements

### Requirement: Evaluation executes concrete specializations only

The evaluator SHALL execute generic-origin functions and nominal values solely through their
concrete MIR types, layouts, and instance identities. It MUST NOT introduce interpreter-owned type
arguments, runtime dictionaries, or alternate generic layout decisions.

#### Scenario: Evaluate two identity instances

- **WHEN** one program calls concrete I32 and nominal-struct specializations
- **THEN** evaluation preserves each concrete value and traces the two canonical instance identities
