## ADDED Requirements

### Requirement: Aggregate literal facts expose context and canonical identity

Semantic facts SHALL record, for every tuple or record literal, whether it used an expected named
aggregate or introduced an anonymous declaration; the canonical resulting nominal identity; every
source element, label, or ordinal; its inferred or expected type; initializer evaluation order; and
its mapping to canonical declaration order. Facts for failed construction SHALL retain the expected
type when one existed, the generated occurrence identity when creation reached that point, and each
independent arity, label, visibility, duplicate, missing, or compatibility cause.

Tooling encodings SHALL present anonymous identities through stable source provenance rather than
invented user-facing names. Repeated analysis of equal source SHALL produce identical facts and
encodings.

#### Scenario: Inspect a contextual call argument

- **WHEN** an editor inspects a record literal passed to a named struct parameter
- **THEN** semantic facts expose the parameter's canonical nominal type and every source-field mapping without requiring the type name at the call site

#### Scenario: Inspect an anonymous binding

- **WHEN** an editor inspects a local initialized by an uncontextualized anonymous record
- **THEN** semantic facts expose its occurrence-based identity, ordered fields, and inferred field types without a fabricated source name
