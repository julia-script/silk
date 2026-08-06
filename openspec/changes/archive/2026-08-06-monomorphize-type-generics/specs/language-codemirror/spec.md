## ADDED Requirements

### Requirement: CodeMirror distinguishes generic angles contextually

CodeMirror highlighting SHALL classify type parameters and generic applications consistently with
the accepted syntax while retaining ordinary comparison and reserved-template highlighting in
their respective contexts.

#### Scenario: Highlight generic call and comparison
- **WHEN** one source contains `identity<I32>(value)` and `left < right`
- **THEN** the generic arguments and comparison operator receive their respective canonical styles

