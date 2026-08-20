## ADDED Requirements

### Requirement: Pattern identifiers retain semantic token and statement structure

Semantic tokens SHALL classify shared pattern declaration and reference tokens from their compiler
semantic occurrences. Structural queries SHALL retain statements nested in both bodies of if-let,
while keeping taken-body binding identity absent from the mismatch body.

#### Scenario: Tokenize and structure if-let

- **WHEN** a document contains an if-let with statements in both bodies
- **THEN** the declaration and taken-body uses receive local-binding semantics and both nested statement bodies remain queryable
