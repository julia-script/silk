## MODIFIED Requirements

### Requirement: Struct literal facts map source fields to canonical fields

Semantic analysis SHALL resolve a literal target to one canonical nominal struct and retain every
source initializer in source order while mapping valid names to canonical field identities. It SHALL
also publish the complete declaration-ordered mapping used to construct the value. Construction
authority, field completeness, uniqueness, visibility, initializer type compatibility, explicit
type-argument prefix, inferred arguments, every inference origin, and completed substitution SHALL
be independent explicit outcomes with stable causal diagnostics.

#### Scenario: Map reordered initializers

- **WHEN** a valid literal supplies fields in an order different from their declaration
- **THEN** semantic facts retain source order and expose a separate complete declaration-ordered mapping to the same canonical fields

#### Scenario: Diagnose independent field failures

- **WHEN** one initializer is duplicated and another has the wrong type
- **THEN** both source facts and both stable causes remain visible without fabricating a complete construction

#### Scenario: Inspect construction inference

- **WHEN** omitted ordinary parameters are inferred from multiple named fields
- **THEN** facts expose the canonical fields, inferred arguments, each field origin, completed substitution, and nominal result
