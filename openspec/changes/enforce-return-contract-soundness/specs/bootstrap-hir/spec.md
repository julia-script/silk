## ADDED Requirements

### Requirement: Executable HIR requires a proven return contract

HIR construction SHALL publish an executable function body only when semantic analysis has proven
every reachable return and fallthrough path against the resolved result contract. Source mistakes
SHALL remain semantic diagnostics and MUST NOT masquerade as executable unavailable return nodes.

#### Scenario: Keep an invalid body out of executable HIR

- **WHEN** a declaration returns a value incompatible with its resolved result type
- **THEN** its semantic facts retain the source diagnostic and HIR does not publish an executable body for that declaration
