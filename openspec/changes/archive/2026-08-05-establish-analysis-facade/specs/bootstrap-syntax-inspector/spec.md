## ADDED Requirements

### Requirement: Labs consume the analysis facade exclusively

Every inspector lab and the flow model SHALL obtain compiler analysis exclusively through the
analysis facade: snapshots built from the edited sources, facts and diagnostics read from facade
queries, and evaluation triggered as a facade query. Phase modules SHALL NOT be value-imported by
the docs app, and an automated check SHALL enforce the boundary.

#### Scenario: Rebuild labs on the facade

- **WHEN** any lab recomputes its view after an edit
- **THEN** it builds a facade snapshot and reads syntax, facts, HIR, and diagnostics from facade queries only

#### Scenario: Enforce the import boundary

- **WHEN** the docs test suite runs
- **THEN** an automated check fails if any lab or flow model value-imports a compiler phase module instead of the facade
