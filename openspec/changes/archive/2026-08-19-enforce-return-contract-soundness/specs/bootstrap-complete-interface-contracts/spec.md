## ADDED Requirements

### Requirement: Conformance bodies satisfy specialized operation returns

An inline or mapped conformance operation body SHALL satisfy the interface operation's resolved
return contract after applying `Self`, interface, conformance, and operation generic substitutions.
An invalid body MUST NOT be published as an available witness.

#### Scenario: Reject an invalid specialized witness

- **WHEN** a mapped effect operation declared to succeed with `i32` returns `Effect<i32>` after all substitutions
- **THEN** semantic analysis reports the return mismatch and does not publish that mapping as an available witness
