## ADDED Requirements

### Requirement: Flow discovery follows static bodies and handlers

Instance discovery SHALL reach each statically selected flow body and catch handler with its concrete
type arguments. It MUST NOT specialize instances by runtime success/failure outcome, payload value,
failure tag, or capture value.

#### Scenario: Reuse one flow instance

- **WHEN** one generic flow is constructed with different values for the same concrete type
- **THEN** discovery produces one body instance and stable handler reachability
