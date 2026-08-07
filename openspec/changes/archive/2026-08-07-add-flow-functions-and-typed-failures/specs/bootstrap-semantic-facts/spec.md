## ADDED Requirements

### Requirement: Semantic facts retain flow construction and residual failure rows

Semantic analysis SHALL publish the success and failure contract for flow construction, `run`,
`fail`, and exact-member catch. Unavailable operands, invalid handlers, undeclared propagation, and
non-nominal payloads SHALL retain their causes and MUST NOT fabricate a successful value or empty
row.

#### Scenario: Inspect a handled run

- **WHEN** `Flow.catch<E>` removes the only failure member before an ordinary function runs the flow
- **THEN** facts show the protected row, selected member, handler row, empty residual row, and success type
