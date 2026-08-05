## ADDED Requirements

### Requirement: Inspect nested call concrete syntax
The Syntax Inspector SHALL provide valid and malformed nested-call presets and SHALL display every
inner and outer call branch, argument boundary, recovery node, token slice, and byte span. Until
recursive semantic analysis is added, it SHALL label nested semantic argument facts as unavailable
rather than implying successful resolution or evaluation.

#### Scenario: Inspect a valid nested call
- **WHEN** a developer selects the `identity(identity(42))` preset
- **THEN** the concrete tree shows both call branches and the semantic view labels the nested outer argument as not yet analyzed

#### Scenario: Inspect malformed nested recovery
- **WHEN** a developer selects a damaged-inner-call preset
- **THEN** the inner missing token, outer call boundary, parser diagnostic, and all available downstream facts remain visible
