## ADDED Requirements

### Requirement: Compatible imported data symbols share one native declaration

Within an executable closure, repeated reachable C data imports SHALL share one LLVM global and one artifact data-import record when their classified C value types agree. Planning SHALL reject incompatible data imports, function/data symbol collisions, duplicate data exports, and data import/export collisions with a diagnostic relating both declarations before emission.

#### Scenario: Application and hosted runtime both read the environment

- **WHEN** the application and GNU hosted source runtime both import `environ` with the same classified C pointer type
- **THEN** both loads use one native global and the artifact records one `environ` import

#### Scenario: Reject incompatible data declarations

- **WHEN** reachable declarations import one data symbol with different classified C types or collide with a function or exported definition
- **THEN** planning reports `SEM0192` relating both declarations and emits no artifact
