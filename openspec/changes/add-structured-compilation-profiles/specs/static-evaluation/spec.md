## MODIFIED Requirements

### Requirement: Static evaluation is reachable, finite, and reproducible

Static functions and mixed bodies SHALL be evaluated for one complete normalized compilation profile and complete static
application when demanded by a constant initializer or the executable-specialization worklist.
Merely loading or indexing an uncalled declaration MUST NOT execute its static body. Equal source,
normalized profile, generic arguments, evidence, and static argument values SHALL produce the same static result,
diagnostics, residual program, and specialization identity across fresh processes.

`StaticEvaluation` SHALL enforce deterministic recursion, work, retained-value, and residual-growth
limits. Exceeding a limit SHALL report a dedicated evaluation-limit diagnostic distinct from
`compileError`, name the exhausted resource, and produce no partial static value or residual
program.

#### Scenario: Leave an uncalled static function unevaluated

- **WHEN** a loaded module declares a static function containing `compileError` but no reachable constant initializer or specialization calls it
- **THEN** the declaration remains indexable without reporting the compile error

#### Scenario: Evaluate one reachable concrete application

- **WHEN** executable discovery reaches the same mixed function with two different static argument values
- **THEN** the compiler evaluates and records two distinct deterministic residual specializations

#### Scenario: Report a resource limit separately

- **WHEN** static recursion or looping exceeds its deterministic evaluation budget
- **THEN** compilation reports an evaluation-limit diagnostic rather than presenting the failure as a source-requested compile error

### Requirement: Target information is static and closed

The selected compilation SHALL publish one immutable normalized profile before ordinary reachable specialization. Initial immutable target/artifact inputs SHALL be available during package-default bootstrap, without exposing an incomplete profile. The completed profile SHALL contain resolved validated package parameters. Static-only sealed Intrinsic queries SHALL expose individual stable logical facts. Ordinary Silk wrappers SHALL own nominal domain enums and ergonomic APIs. The whole-target Profile enum and Intrinsic.targetProfile ordinal operation SHALL not exist.

Target facts MUST NOT be readable at runtime, changed by source, inferred from the host when an explicit selection exists, or recomputed by an execution engine. Static cache identity SHALL include canonical profile identity alongside source and static application identity.

#### Scenario: Select code by target architecture

- **WHEN** a mixed function calls the standard-library static architecture query and compares its result with the `Wasm32` enum member
- **THEN** a WebAssembly compilation selects the wasm arm and every native compilation selects the other arm before residual HIR is produced

#### Scenario: Keep the selected target out of runtime state

- **WHEN** runtime code is emitted from a target-specialized function
- **THEN** the artifact contains the selected residual operations but no target-profile parameter, runtime target probe, or static target query

#### Scenario: Isolate same-target package configuration

- **WHEN** one process evaluates the same ordinary static helper under same-target profiles whose package boolean differs
- **THEN** each evaluation returns its own configured value and repeated applications cannot reuse the other profile
