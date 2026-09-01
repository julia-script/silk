## ADDED Requirements

### Requirement: Inspect the first resolved call relationship

The Syntax Inspector SHALL visualize each present call as a directed caller-to-target relationship
when uniquely resolved and as missing, ambiguous, or syntax-unavailable otherwise. The relationship
view SHALL keep caller, call-site, and target declaration spans available and SHALL remain beside the
concrete tree, function facts, and phase-separated diagnostics.

#### Scenario: Inspect a resolved call edge

- **WHEN** a developer selects the two-function resolved-call preset
- **THEN** the semantic view shows `main → answer`, the target declaration identity, an `I32` call type, and compatible caller return

#### Scenario: Inspect an unknown call target

- **WHEN** a developer selects the unknown-call preset
- **THEN** the relationship is shown as missing, compatibility is unavailable, and `SEM0004` identifies the call-site name

#### Scenario: Inspect an ambiguous call target

- **WHEN** a developer selects the ambiguous-call preset
- **THEN** the relationship shows every matching declaration without choosing one and the existing duplicate-name diagnostics remain visible
