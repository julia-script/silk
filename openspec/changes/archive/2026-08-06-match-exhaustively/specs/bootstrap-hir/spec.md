## ADDED Requirements

### Requirement: HIR represents matching as an acyclic typed region

HIR SHALL represent one match as a scrutinee evaluated once, its logical access mode and type, and
source-ordered arm regions. Each executable arm SHALL carry its canonical member or universal
coverage, narrowed payload, pattern bindings, optional typed guard, result expression, cleanup
boundary, and join result type. Child, guard, arm, cleanup, and continuation relationships SHALL
remain acyclic and MUST NOT contain physical tags, backend blocks, branch depths, or reconstructed
cyclic control.

#### Scenario: Elaborate a guarded union match

- **WHEN** a shared match has a guarded `Token` arm, an unguarded `Token` arm, and an `End` arm
- **THEN** HIR retains three ordered arm regions over canonical members and one acyclic result join

#### Scenario: Elaborate consuming destructuring

- **WHEN** a consuming arm binds one field and acknowledges omitted fields
- **THEN** HIR carries the complete narrowed payload, bound field access, omitted-field cleanup boundary, and arm result provenance
