# Spec Delta

## ADDED Requirements

### Requirement: Completed headers expose stable observable projections

Each completed declaration header or recursive header component SHALL expose a canonical observable
projection containing stable owner/component identity, public semantic content, and observable
definition-owned diagnostics. The projection MUST NOT retain revision-specific declaration objects,
body templates, index instances, sessions, incidental pool ordinals, or source positions. Recovered
owners that participate in a result SHALL additionally expose byte-identical authored content as a
validity input.

#### Scenario: Preserve a public signature across unrelated edits

- **WHEN** an unrelated owner changes while one completed public signature projection is equal
- **THEN** readers of that signature validate without executing

#### Scenario: Reject positional identity

- **WHEN** insertion of an unrelated declaration changes traversal ordinals but leaves a stable owner's completed header equal
- **THEN** the stable owner's projection and address remain equal

#### Scenario: Protect a recovered owner

- **WHEN** a parser-recovered owner contributes to a reusable header result and its authored bytes change
- **THEN** its byte-sensitive input changes and affected readers execute
