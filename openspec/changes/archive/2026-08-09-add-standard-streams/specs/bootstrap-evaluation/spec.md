## ADDED Requirements

### Requirement: Evaluation records standard-stream writes

With an explicit provider, evaluation SHALL record complete ordered byte events and typed failures without ambient host streams. Repeated evaluation with the same provider behavior SHALL be deterministic.

#### Scenario: Capture several writes

- **WHEN** a program writes a heading and two rows
- **THEN** evaluation records exactly those three byte events in order

