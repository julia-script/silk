# Spec Delta

## ADDED Requirements

### Requirement: Declared public types are addressed through semantic queries

Every named declaration's public type or signature SHALL be available through one authoritative
typed semantic request after header collection and before body checking. The answer SHALL preserve
explicit unresolved states and opaque public-signature boundaries, SHALL observe every header,
alias, bound, and conformance answer used, and SHALL NOT reveal an opaque witness or infer an
arbitrary expression type.

#### Scenario: Read a declared function signature

- **WHEN** a caller requests the public type of a named function with declared parameters and result
- **THEN** the answer contains that complete signature and the dependency observations identify its header inputs

#### Scenario: Preserve an opaque public signature

- **WHEN** a named opaque declaration has a declared public signature and a body-dependent representation witness
- **THEN** the public-type request returns only the declared signature without realizing or exposing the witness

#### Scenario: Keep unavailable header state explicit

- **WHEN** a declared type cannot be completed because an alias, bound, or conformance answer is unavailable
- **THEN** the request completes with an unavailable result and anchored diagnostics rather than a provisional type
