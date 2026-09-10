## MODIFIED Requirements

### Requirement: HIR represents matching as an acyclic typed region

HIR SHALL represent one match as a scrutinee evaluated once, its logical access mode and type, and
source-ordered arm regions. Each executable arm SHALL carry its canonical member or universal
coverage, narrowed payload, pattern bindings, optional typed guard, an explicit expression-or-ordinary-statement-block body, normal-completion and lexical-transfer outcomes, cleanup boundary, and join result type. Body statements and expressions SHALL retain exact source provenance. Normally completing blocks SHALL provide unit and noncompleting arms SHALL provide no join value. Transfer targets SHALL belong to the enclosing execution boundary; ordinary arm blocks SHALL NOT create a callable or Effect boundary. Child, guard, arm, cleanup, and continuation relationships SHALL
remain acyclic and MUST NOT contain physical tags, backend blocks, branch depths, or reconstructed
cyclic control.

#### Scenario: Elaborate a guarded union match

- **WHEN** a shared match has a guarded `Token` arm, an unguarded `Token` arm, and an `End` arm
- **THEN** HIR retains three ordered arm regions over canonical members and one acyclic result join

#### Scenario: Elaborate consuming destructuring

- **WHEN** a consuming arm binds one field and acknowledges omitted fields
- **THEN** HIR carries the complete narrowed payload, bound field access, omitted-field cleanup boundary, and arm result provenance

#### Scenario: Retain a return through expression nesting

- **WHEN** a typed ordinary match arm nested in a larger expression returns from the current body
- **THEN** HIR retains the arm statement region, exact return provenance and target, and the noncompleting path without demanding an arm result expression

#### Scenario: Preserve body facts through specialization

- **WHEN** a generic match with ordinary block arms is specialized to a complete application
- **THEN** specialized HIR retains explicit body kinds, typed statements, provenance, canonical selected bindings, completion facts, and enclosing transfer regions
