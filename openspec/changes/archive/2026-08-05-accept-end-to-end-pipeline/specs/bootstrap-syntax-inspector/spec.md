## ADDED Requirements

### Requirement: Inspect the pipeline overview

The docs site SHALL expose a direct-link pipeline-overview lab presenting, for an edited
program: every frontend phase in pipeline order with its status, artifact counts, and diagnostic
counts, the snapshot's total elapsed build time, links to each phase's dedicated lab, and the
planned native stages. Every step of the pipeline SHALL be visible in one place.

#### Scenario: Overview a healthy program

- **WHEN** a developer edits a valid program
- **THEN** the lab lists every phase in order with its counts, zero diagnostics, the elapsed time, and a link to each phase's lab

#### Scenario: Overview a damaged program

- **WHEN** the edited program contains mistakes across phases
- **THEN** each phase row shows its own diagnostic count while later phases remain listed with their explicit recovery states
