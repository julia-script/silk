## ADDED Requirements

### Requirement: Formatting preserves generic context

Formatting SHALL render type parameter lists and generic applications canonically without changing
comparison grouping or reserved-template interpretation, and repeated formatting SHALL be
idempotent for valid and recovered generic syntax.

#### Scenario: Format nested applications idempotently

- **WHEN** a source contains nested generic applications and is formatted twice
- **THEN** the second output equals the first byte-for-byte and reparses to the same generic syntax

#### Scenario: Preserve damaged generic syntax

- **WHEN** a generic list is missing a closing bracket
- **THEN** formatting retains the explicit recovery boundary without consuming the following declaration
