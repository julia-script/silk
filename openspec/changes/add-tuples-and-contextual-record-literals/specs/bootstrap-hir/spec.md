## ADDED Requirements

### Requirement: Aggregate sugar erases to canonical nominal struct HIR

Elaboration SHALL represent named tuple construction, contextual tuple literals, contextual record
literals, and uncontextualized anonymous aggregate literals as the existing canonical nominal struct
construction shape. Each construction SHALL carry one source or synthesized nominal declaration
identity, typed initializers in canonical field or position order, and source-order evaluation
provenance. Positional projections SHALL become canonical field projections through the synthesized
ordinal identity.

HIR MUST NOT retain a second tuple or structural-record runtime value category, generated source
name, shape-comparison rule, or unresolved expected-type search. An invalid or ambiguous aggregate
expression SHALL remain unavailable with its originating semantic cause rather than lowering a
partial construction.

#### Scenario: Erase a contextual record literal

- **WHEN** `foo(.{ name: makeName(), age: makeAge() })` is accepted for a `Person` parameter
- **THEN** HIR carries one canonical `Person` construction with source-order evaluation provenance and declaration-order fields

#### Scenario: Erase an anonymous tuple

- **WHEN** an uncontextualized tuple literal is bound locally
- **THEN** HIR carries one construction of its synthesized nominal declaration and no tuple-specific runtime node
