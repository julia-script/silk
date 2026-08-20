## ADDED Requirements

### Requirement: Statement-pattern facts are facade queries

The analysis facade SHALL expose shared pattern syntax and facts, exact member evidence, coverage,
irrefutability, bindings and scopes, ownership loans and exits, HIR selections, MIR match regions,
evaluation outcomes, and backend artifacts from one immutable snapshot for match, let, and if-let.
Position-oriented queries SHALL expose pattern declaration identity, references, presentations,
completion visibility, and nested statement structure without tooling reconstructing lexical scope.

#### Scenario: Query one if-let binding

- **WHEN** tooling queries the declaration and uses of a taken-body pattern binding
- **THEN** completion, hover, navigation, semantic occurrences, and statement structure agree on one identity and scope
