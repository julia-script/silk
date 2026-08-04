## ADDED Requirements

### Requirement: Inspect the declaration collection
The Syntax Inspector SHALL show one semantic function card per parsed declaration in concrete source
order. Each card SHALL display declaration identity, name state, return type, integer value, return
compatibility, and provenance, and the inspector SHALL provide a duplicate-name preset that displays
the ambiguous lookup state and its semantic diagnostic.

#### Scenario: Inspect two collected declarations
- **WHEN** a developer selects the two-function preset
- **THEN** the semantic view shows ordered `answer` and `main` cards whose ordinals and spans match their concrete branches

#### Scenario: Inspect a duplicate declaration name
- **WHEN** a developer selects the duplicate-name preset
- **THEN** both declarations remain visible, name lookup is shown as ambiguous, and `SEM0003` identifies the later name
