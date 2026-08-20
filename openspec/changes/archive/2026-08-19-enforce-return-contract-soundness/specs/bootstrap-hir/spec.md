## ADDED Requirements

### Requirement: Executable HIR requires a proven return contract

HIR construction MAY retain typed or explicitly unavailable body structure for inspection, but a
function body SHALL be executable only when semantic analysis has proven every reachable return and
fallthrough path against the resolved result contract. Source mistakes SHALL remain semantic
diagnostics and the function MUST be unavailable to reachability and target-dependent phases.

#### Scenario: Keep an invalid body out of executable HIR

- **WHEN** a declaration returns a value incompatible with its resolved result type
- **THEN** its semantic facts retain the source diagnostic, any retained HIR return is explicitly unavailable, and target-dependent phases cannot consume the body
