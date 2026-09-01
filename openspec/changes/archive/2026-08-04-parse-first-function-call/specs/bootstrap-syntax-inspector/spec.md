## ADDED Requirements

### Requirement: Inspect the first call expression

The Syntax Inspector SHALL provide valid-call, missing-call-syntax, and unsupported-argument presets.
It SHALL show the call's concrete subtree, exact token slices and spans, unresolved semantic fact,
unavailable compatibility, and separate parser and semantic diagnostic collections.

#### Scenario: Inspect a valid unresolved call

- **WHEN** a developer selects the valid-call preset
- **THEN** the concrete view shows `answer()` as a call expression and the semantic view labels its callee unresolved without displaying an unknown-name diagnostic

#### Scenario: Inspect damaged call syntax

- **WHEN** a developer selects a missing-parenthesis or unsupported-argument preset
- **THEN** explicit missing or error syntax stays visible beside the unavailable call facts and parser-owned diagnostics
