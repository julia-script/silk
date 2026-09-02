## ADDED Requirements

### Requirement: HIR represents anonymous callable bodies and environments canonically

HIR SHALL retain each anonymous callable's deterministic source-occurrence target, enclosing owner,
explicit ordinary or effect contract, derived invocation mode, ordered parameters, typed body,
surrounding substitution, and ordered captures with canonical binding, access, ownership root, and
dependency facts. The containing expression SHALL construct that exact target and environment;
executable discovery SHALL reach the body through the anonymous value without surface-syntax lookup.
HIR encoding and traversal SHALL be deterministic, and damaged anonymous bodies SHALL remain
explicitly unavailable rather than publishing a partial executable target.

#### Scenario: Retain a capturing anonymous body

- **WHEN** an anonymous callable captures an outer value and is stored before invocation
- **THEN** HIR contains one anonymous target and one construction carrying the capture's canonical identity, access, and source-order ordinal

#### Scenario: Retain an effectful anonymous contract

- **WHEN** an effectful anonymous body declares success, failure, and requirement channels
- **THEN** HIR keeps those channels on its executable contract and keeps invocation distinct from later Effect execution

#### Scenario: Encode occurrences deterministically

- **WHEN** the same module is elaborated repeatedly
- **THEN** anonymous target identities, capture order, body traversal, and encoded HIR bytes remain stable
