## MODIFIED Requirements

### Requirement: Field reads are canonical typed HIR projections

Elaboration SHALL lower every valid field read to a typed HIR projection carrying its subject
expression, subject nominal type, canonical field identity, result type, access mode, and exact
source span. Nested projections SHALL remain nested in source order. Access SHALL distinguish non-consuming Copy reads, borrows, and consuming moves. A requested field move SHALL retain a canonical owned-place path and declared lifetime-bearing type for ownership to check initialization, visibility, loan, and Drop boundaries.

#### Scenario: Elaborate a scalar field read

- **WHEN** a valid expression reads `pair.left`
- **THEN** HIR contains a projection from canonical `Pair` through field `left` with its declared scalar result type

#### Scenario: Preserve a partial-move request

- **WHEN** source requests `move outer.inner`
- **THEN** HIR retains the projection and consuming access request with exact provenance for ownership to validate under ordinary partial-move rules
