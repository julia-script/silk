## ADDED Requirements

### Requirement: HIR is generic-aware before specialization

HIR SHALL retain canonical type parameters in generic declarations and explicit generic-call
operations carrying normalized type arguments or an unavailable specialization cause. HIR MUST NOT
clone a declaration body per concrete call, and its deterministic encoding SHALL preserve the link
from every call to its generic declaration and substitution.

#### Scenario: Keep one generic body

- **WHEN** one generic function is called with `I32` and `Token`
- **THEN** HIR contains one checked declaration body and two calls with distinct concrete substitutions
