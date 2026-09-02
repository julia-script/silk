## ADDED Requirements

### Requirement: Completion follows anonymous callable lexical scope

Completion inside an anonymous callable signature or body SHALL consume compiler-owned recovered
scope facts. Parameter type and result positions SHALL offer the surrounding accessible types and
type parameters. Body expression positions SHALL offer the anonymous parameters plus visible outer
locals, pattern bindings, parameters, declarations, and keywords under ordinary shadowing. The LSP
MUST NOT synthesize a capture list or expose a self-name. At an expression start, ordinary `fn`
SHALL be an applicable keyword; after an expression-position `effect`, completion SHALL distinguish
`fn` from the Effect-block form without hiding either valid continuation.

#### Scenario: Complete inside an anonymous body

- **WHEN** completion is requested in a body where parameter `value` shadows an outer `value` and outer `offset` remains visible
- **THEN** candidates identify the anonymous parameter for `value` and the selected outer binding for `offset`

#### Scenario: Complete an anonymous parameter type

- **WHEN** completion is requested after the colon in `fn(value: ) -> Result { ... }`
- **THEN** completion offers accessible types and enclosing type parameters but excludes value-only locals

#### Scenario: Complete an anonymous callable start

- **WHEN** completion is requested at an expression start or immediately after `effect`
- **THEN** it offers the valid anonymous `fn` continuation from compiler grammar facts without inventing `mut fn` or `once fn` construction
