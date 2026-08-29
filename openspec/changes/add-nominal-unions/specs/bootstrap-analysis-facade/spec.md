## ADDED Requirements

### Requirement: Nominal union facts are analysis-facade queries

The analysis facade SHALL expose immutable query results for union declarations, applied parent
types, ordered variants, fields, visibility, generic substitution, constructors, patterns,
hierarchical coverage, ownership, layouts, HIR, MIR, evaluation, and emission provenance. Tooling
MUST consume those facts rather than reconstructing variant relationships or tag behavior from
syntax.

#### Scenario: Query one union declaration

- **WHEN** a consumer asks for a generic union and one variant at their source positions
- **THEN** the facade returns canonical parent, parameter, variant, field, validity, and source facts from one coherent snapshot

#### Scenario: Preserve recovery isolation

- **WHEN** one variant is damaged beside valid siblings
- **THEN** facade queries expose its unavailable state while retaining navigable facts for the valid variants and unrelated declarations
