## ADDED Requirements

### Requirement: Contextual aggregate construction preserves nominal authority

Expected-type analysis SHALL resolve `(values...)` only to a named tuple and `.{ fields... }` only
to a source-declared named struct of matching aggregate kind. Contextual construction SHALL enforce
the target declaration's arity or field completeness, visibility, generic inference, member types,
and construction authority exactly as if its type name were written. It MUST NOT bypass a private
field, manufacture access to an opaque nominal, or use shape compatibility to select among types.

When no expected nominal aggregate is already determined, analysis SHALL create exactly one
anonymous nominal aggregate for the literal rather than searching visible declarations by shape.
Unknown, duplicate, missing, inaccessible, or incompatible members SHALL remain independently
queryable and SHALL produce no partial aggregate value.

#### Scenario: Preserve a private construction boundary

- **WHEN** an external caller passes `.{ ... }` to a function parameter whose struct type has one private required field
- **THEN** contextual construction is rejected without revealing or bypassing the hidden representation field

#### Scenario: Avoid shape-based type search

- **WHEN** two visible named structs have the same public fields and an uncontextualized record literal matches both shapes
- **THEN** analysis creates one anonymous nominal record and does not select either visible declaration

#### Scenario: Diagnose the expected tuple position

- **WHEN** a positional literal has the wrong arity or an incompatible element for its expected named tuple
- **THEN** analysis identifies the expected declaration and offending ordinal without producing a partial value
