## ADDED Requirements

### Requirement: Inspect the unified diagnostic stream
The inspector SHALL present the compilation's diagnostics from the unified model: each diagnostic
showing its stable code, severity, message, primary span, and originating phase, in the
deterministic driver order. Selecting a diagnostic that carries a causal diagnostic identity
SHALL reveal its originating diagnostic.

#### Scenario: Diagnostics show phase origin in driver order
- **WHEN** the inspected source produces lexical, parser, and semantic diagnostics together
- **THEN** the inspector lists all of them in the deterministic driver order, each labeled with its originating phase, code, and severity

#### Scenario: Follow a causal chain
- **WHEN** a listed diagnostic carries a cause
- **THEN** selecting it reveals the originating diagnostic and its primary span
