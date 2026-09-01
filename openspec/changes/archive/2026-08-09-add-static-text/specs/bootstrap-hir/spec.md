## ADDED Requirements

### Requirement: HIR carries static data identities and views

HIR SHALL retain decoded bytes, UTF-8 validity, `usize` length, canonical static identity, logical immutable view, and provenance without target placement or owning String behavior.

#### Scenario: Elaborate static UTF-8

- **WHEN** a valid text literal is accepted
- **THEN** HIR encodes exact bytes and one immutable static view
