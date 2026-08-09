## ADDED Requirements

### Requirement: Evaluation exposes exact immutable static bytes

Evaluation SHALL model static bytes and views without allocation, mutation, or host-string identity and SHALL encode their events deterministically.

#### Scenario: Read a UTF-8 view

- **WHEN** evaluation observes a non-ASCII text literal's byte view
- **THEN** it returns the exact UTF-8 bytes and `usize` length

