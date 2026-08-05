## ADDED Requirements

### Requirement: Inspect parameter declarations and references
The Syntax Inspector SHALL render each function's ordered parameter facts and every bare-identifier
reference relationship. It SHALL show owning function and parameter identities, declaration and
reference spans, declared and expression types, lookup outcome, return compatibility, and
phase-separated diagnostics. Presets SHALL cover resolved, unknown, duplicate, cross-function, and
syntax-unavailable references.

#### Scenario: Inspect a resolved parameter reference
- **WHEN** a developer selects the identity-function preset
- **THEN** the semantic view links the returned `value` to parameter zero and shows `I32` expression type and compatible return

#### Scenario: Inspect an unknown local name
- **WHEN** a developer selects the unknown-parameter-reference preset
- **THEN** the relationship is missing and `SEM0006` identifies the exact returned identifier

#### Scenario: Inspect duplicate local parameters
- **WHEN** a developer selects the duplicate-parameter preset
- **THEN** both declarations remain visible, the reference lists both matches without choosing one, and `SEM0005` identifies the later declaration
