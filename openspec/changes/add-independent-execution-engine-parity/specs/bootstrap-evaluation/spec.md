## ADDED Requirements

### Requirement: Evaluation is the independent-execution state oracle

Evaluation SHALL model each explicit Execution with deterministic logical package identity,
execution-local stack root, suspension frames, endpoint generation, Execution state, wake-cell
state, and reclamation authorities independent of JavaScript promises, stack identity, garbage
collection, or object finalization. It SHALL execute owner-selected drives synchronously until
completion or external relinquishment and SHALL model wake as readiness only. Bounded traces SHALL
record initialization, drive, park, latch, suspend, notify, eligible, resume, complete, cancel,
DestroyPending, cleanup, and release in canonical order.

#### Scenario: Defer body evaluation

- **WHEN** evaluation constructs and stores an Initial Execution
- **THEN** no body event occurs until an owner drive transition

#### Scenario: Preserve two logical roots

- **WHEN** evaluation alternates two executions through several parks
- **THEN** each trace retains a distinct stable root and its own CallDepth sequence

#### Scenario: Sweep every ordering branch

- **WHEN** tests execute wake-during-register, wake-after-dormant, destroy-before-wake, eligible-drop, and reentrant-destroy cases
- **THEN** evaluation emits the selected transition and cleanup sequence for each without depending on host scheduling

#### Scenario: Repeat the oracle

- **WHEN** the same pressure program and scripted readiness events are evaluated repeatedly
- **THEN** results, logical identities, and bounded traces are byte-identical
