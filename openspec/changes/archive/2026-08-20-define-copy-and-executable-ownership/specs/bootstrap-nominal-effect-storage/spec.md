## MODIFIED Requirements

### Requirement: Stored Effect cleanup is exact

An unrun stored Effect, a successful or failed run, and a suspending run SHALL clean every live
environment field exactly once. A concrete reusable Effect whose captures are all Copy snapshots or
shared borrows MAY participate in an aggregate's sealed Copy proof and then has no cleanup. An owned
affine or exclusive capture SHALL keep the aggregate affine. Direct affine extraction SHALL report
the ordinary aggregate partial-move diagnostic, and scoped captures MUST NOT escape through the
nominal.

#### Scenario: Copy a reusable stored Effect

- **WHEN** an aggregate validly implements `Copy` and its concrete stored Effect has only Copy captures
- **THEN** an ordinary read duplicates the complete aggregate without adding cleanup

#### Scenario: Drop an unrun Effect

- **WHEN** a `Deferred` containing an owned environment leaves scope without execution
- **THEN** its live captures are cleaned once without entering the runner

#### Scenario: Drop an affine unrun Effect

- **WHEN** a stored Effect owns an affine capture and leaves scope without execution
- **THEN** its live capture is cleaned exactly once without entering the runner
