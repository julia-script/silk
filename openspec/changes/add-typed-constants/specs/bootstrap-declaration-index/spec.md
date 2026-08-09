## ADDED Requirements

### Requirement: Constants join the canonical declaration index

The declaration index SHALL collect each top-level constant before body analysis with its canonical
identity, visibility, declared primitive type syntax, literal initializer syntax, duplicate state,
and exact source provenance. Constants SHALL share the module's flat top-level namespace with
functions and structs.

#### Scenario: Detect a cross-kind duplicate

- **WHEN** a constant and a function in one module declare the same name
- **THEN** the first declaration remains canonical and the later declaration records the ordinary duplicate identity and diagnostic

#### Scenario: Publish a constant header before function bodies

- **WHEN** a function precedes or follows a valid constant in source order
- **THEN** its body resolves the same canonical constant header without order dependence
