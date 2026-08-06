## ADDED Requirements

### Requirement: Struct construction is canonical typed HIR

Elaboration SHALL lower each valid struct literal to one typed HIR construction carrying the
canonical nominal type and one initializer per canonical field in declaration order. Each
initializer SHALL retain its own typed HIR expression and source provenance. HIR MUST NOT retain
source-order lookup decisions or recalculate field completeness.

#### Scenario: Elaborate a reordered literal

- **WHEN** semantic facts accept a literal whose source fields are reordered
- **THEN** HIR contains one nominal construction with typed initializers in canonical declaration order

#### Scenario: Keep invalid construction unavailable

- **WHEN** construction authority, completeness, or a field initializer is unavailable
- **THEN** HIR retains an unavailable expression with the originating cause rather than a partial aggregate

### Requirement: Field reads are canonical typed HIR projections

Elaboration SHALL lower every valid field read to a typed HIR projection carrying its subject
expression, subject nominal type, canonical field identity, result type, access mode, and exact
source span. Nested projections SHALL remain nested in source order. This slice's available access
mode SHALL be a non-consuming read of a Copy scalar field; a requested partial move SHALL remain
unavailable for ownership checking.

#### Scenario: Elaborate a scalar field read

- **WHEN** a valid expression reads `pair.left`
- **THEN** HIR contains a projection from canonical `Pair` through field `left` with its declared scalar result type

#### Scenario: Preserve a partial-move request

- **WHEN** source requests `move outer.inner`
- **THEN** HIR retains the projection and consuming access request with exact provenance for ownership to reject

### Requirement: Nominal values cross ordinary HIR call boundaries

HIR function contracts, parameters, bindings, calls, and returns SHALL retain canonical nominal
types alongside built-in types. A constructed or whole-moved aggregate SHALL be usable as an
ordinary argument or result without a struct-specific call kind or backend representation in HIR.

#### Scenario: Call a public factory

- **WHEN** another module calls `Token.make` and the function returns `Token`
- **THEN** the call and result carry the same canonical nominal type as the defining struct declaration
