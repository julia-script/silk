## ADDED Requirements

### Requirement: Semantic facts retain row contracts and evidence identities

Declaration facts SHALL retain source-shaped symbolic row expressions, member-well-formedness
obligation keys, generic binder references, and ordered constraints with complete provider mode,
selected row, source row, and source origins. Call facts SHALL expose explicit-prefix bindings,
inferred suffix substitutions, wanted constraints, and assumed or concrete evidence without
collapsing span-free semantic identity into source provenance.

Semantic occurrences and navigation SHALL traverse every row/member parameter and constraint term.
Presentation and signature help SHALL render from the same facts used by call admission.

#### Scenario: Publish a constrained provider declaration fact

- **WHEN** analysis collects `where &mut P provides S from R`
- **THEN** its fact identifies exclusive provider mode, binders `P`, `S`, and `R`, the canonical constraint key, and its separate source origin

#### Scenario: Publish a symbolic difference fact

- **WHEN** a generic result contains `Without<R, S>`
- **THEN** facts retain the row parameter, lifted or row-kind selected term, residual obligations, and definitional key until specialization

#### Scenario: Navigate every constraint binder

- **WHEN** tooling asks for occurrences of `S` in a declaration constraint and result row
- **THEN** all source references resolve to the same generic binder without treating contextual keywords as declarations
