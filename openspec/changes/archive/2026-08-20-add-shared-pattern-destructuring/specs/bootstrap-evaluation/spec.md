## ADDED Requirements

### Requirement: Evaluation executes statement patterns from logical members

Evaluation SHALL execute MIR statement selections using the scrutinee's canonical logical active
member. It SHALL preserve nested payload values, create only the selected bindings, execute the
correct conditional body, and apply the MIR ownership and cleanup plan without decoding backend
storage or choosing numeric tags independently.

#### Scenario: Evaluate nested local destructuring

- **WHEN** an irrefutable pattern binds fields nested inside two nominal values
- **THEN** evaluation exposes the exact nested payloads to subsequent statements

#### Scenario: Evaluate conditional mismatch

- **WHEN** the active union member does not equal an if-let selector
- **THEN** evaluation creates no taken-body bindings and executes only the mismatch body
