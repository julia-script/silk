## ADDED Requirements

### Requirement: Marked operators use ordinary static specialization

An operator over a bounded parameter SHALL select only an operation carrying the matching explicit
marker in that parameter's resolved interface bounds. Specialization SHALL resolve the same
conformance witness as a named bound-operation call and SHALL preserve the operation's literal
operand and result types. Operation names and expected result types MUST NOT participate in
selection.

#### Scenario: Specialize a heterogeneous marked operation

- **WHEN** a generic body applies `*` through a bound whose marked operation accepts `Self` and a distinct scalar type
- **THEN** each concrete specialization selects its ordinary conformance witness and retains the declared result type

#### Scenario: Ignore an unmarked conventional name

- **WHEN** a bound declares `multiply` without an operator marker
- **THEN** `*` remains unavailable even though the operation name resembles multiplication
