## ADDED Requirements

### Requirement: Revision reuse begins from the last atomic commit

The project scheduler SHALL supply the last complete committed analyzed-document map to a new
project analysis callback. The first analysis SHALL receive an empty prior map. A superseded,
failed, interrupted, or still-running analysis MUST NOT become the reuse basis for later work.

#### Scenario: Analyze after one accepted edit

- **WHEN** revision N commits and revision N+1 begins
- **THEN** the N+1 analysis callback receives revision N's complete committed map as its prior value

#### Scenario: Supersede an active revision

- **WHEN** revision N+1 becomes stale while N+2 is queued
- **THEN** N+2 receives the last committed map from revision N rather than any result produced for N+1

### Requirement: Shared workspace results retain their project analysis

Every analyzed-document view produced by one workspace analysis SHALL reference the same completed
project analysis value. Workspace revision analysis SHALL derive its syntax reuse only from the
prior committed value for that workspace.

#### Scenario: Commit several revised root views

- **WHEN** a multi-root workspace revision completes using a prior committed project
- **THEN** every document result references the same new project analysis and its own current root view
