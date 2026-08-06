## ADDED Requirements

### Requirement: Struct literal facts map source fields to canonical fields

Semantic analysis SHALL resolve a literal target to one canonical nominal struct and retain every
source initializer in source order while mapping valid names to canonical field identities. It SHALL
also publish the complete declaration-ordered mapping used to construct the value. Construction
authority, field completeness, uniqueness, visibility, and initializer type compatibility SHALL be
independent explicit outcomes with stable causal diagnostics.

#### Scenario: Map reordered initializers

- **WHEN** a valid literal supplies fields in an order different from their declaration
- **THEN** semantic facts retain source order and expose a separate complete declaration-ordered mapping to the same canonical fields

#### Scenario: Diagnose independent field failures

- **WHEN** one initializer is duplicated and another has the wrong type
- **THEN** both source facts and both stable causes remain visible without fabricating a complete construction

### Requirement: Projection facts preserve every canonical step

Semantic analysis SHALL publish one projection step for every postfix field access, including the
subject type, canonical field identity when available, declared result type, exact provenance,
visibility outcome, and causal unavailable state. A failed inner step SHALL leave its known facts
visible and make each dependent outer step explicitly unavailable without alternate lookup.

#### Scenario: Resolve a nested field chain

- **WHEN** `token.span.start` names two available fields
- **THEN** semantic facts expose both ordered canonical field edges and the final scalar type

#### Scenario: Stop after an unavailable inner projection

- **WHEN** the first field in a chain is private or unknown
- **THEN** the first step retains its candidate or missing state and later steps remain caused unavailable without searching another type
