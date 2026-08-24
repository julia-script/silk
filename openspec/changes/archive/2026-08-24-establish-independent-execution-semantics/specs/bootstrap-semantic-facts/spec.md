## ADDED Requirements

### Requirement: Semantic facts derive detached and non-parking executable properties

Analysis SHALL derive `Intrinsic.Detached` for an exact value or executable representation only
when it owns every value required for later invocation and cleanup and retains no external lexical
or provider loan. An empty Effect requirement row MUST NOT by itself establish Detached. Analysis
SHALL derive `Intrinsic.NonParking` for an exact callable only when its specialized transitive call
graph cannot reach `Intrinsic.park`; direct work and nested-only suspension SHALL remain permitted.
Open generics SHALL preserve either fact only through an explicit sealed-property bound. Facts and
failure causes SHALL be deterministic under substitution, caching, serialization, and inspection,
and ordinary source declarations MUST NOT acquire either property by spelling. A failed Detached or
NonParking bound/application SHALL publish one deterministic source diagnostic at the failed
obligation, retain the complete lexical/provider-loan or transitive-park causal path, and remain
distinct from an unsatisfied Effect requirement-row diagnostic.

#### Scenario: Reject a captured lexical loan

- **WHEN** an exact Effect captures a caller-owned shared or exclusive reference
- **THEN** its Detached fact is unsatisfied with the capture provenance as the cause

#### Scenario: Distinguish an owned provider from a borrowed provider

- **WHEN** one closed Effect retains a provider loan and another owns an equivalent detached provider value
- **THEN** the first fails Detached despite its empty requirement row and the second satisfies Detached when every nested capture is detached

#### Scenario: Keep detachment orthogonal to local execution affinity

- **WHEN** a closed Effect owns a local `Shared<T>` handle and retains no external lexical or provider loan
- **THEN** it satisfies Detached while its canonical execution affinity remains `LocalExecution`, and neither fact grants thread transfer

#### Scenario: Inspect a nested nominal loan

- **WHEN** an executable owns a nominal whose nested field retains an external loan
- **THEN** Detached is unsatisfied and inspection preserves the complete field-to-loan cause

#### Scenario: Keep payload opacity independent

- **WHEN** an exact executable has an opaque producer result type but its complete environment is owned
- **THEN** Detached remains satisfied because the proof concerns environment and drop provenance rather than success or failure payload spelling

#### Scenario: Admit nested-only work in a non-parking callback

- **WHEN** an exact callback performs direct work and can reach `Effect.suspend` but cannot reach `Intrinsic.park`
- **THEN** NonParking is satisfied

#### Scenario: Reject transitive external parking

- **WHEN** an exact callback reaches `Intrinsic.park` through a specialized provider or ordinary helper
- **THEN** NonParking is unsatisfied and reports its stable diagnostic at the failed obligation with a deterministic transitive-reachability cause

#### Scenario: Diagnose detached admission independently of requirement rows

- **WHEN** a closed Effect has an empty requirement row but retains an external provider loan at an Execution wrapper call
- **THEN** analysis reports the Detached diagnostic at the failed bound/application with the provider path and does not report an unsatisfied requirement row

#### Scenario: Ignore privileged-looking source names

- **WHEN** ordinary source declares types or functions named Execution, Wake, Detached, NonParking, Scheduler, Fiber, Deferred, Timer, or Coroutine
- **THEN** semantic facts grant no intrinsic identity, property, or suspension mode by spelling

### Requirement: Sealed Execution values seed canonical local affinity

Every available sealed `Intrinsic.Execution<A>` SHALL have canonical execution affinity
`LocalExecution`, independent of the available affinity of `A`. That fact SHALL propagate through
nominals, unions, arrays, callable and Effect environments, and suspended frame ownership by the
existing `ExecutionAffinity` lattice. It SHALL NOT publish an execution-instance identity or grant
thread transfer. A malformed or unavailable `A` SHALL keep the Execution representation
unavailable, and an ordinary source nominal named Execution MUST NOT receive the sealed seed.

#### Scenario: Derive local affinity for an unrestricted result

- **WHEN** an available `Intrinsic.Execution<A>` has an unrestricted result representation
- **THEN** the Execution itself is `LocalExecution` and a container or executable capturing it recursively remains `LocalExecution`

#### Scenario: Preserve unavailable result structure

- **WHEN** the result representation of `Intrinsic.Execution<A>` is malformed or unavailable
- **THEN** the Execution representation remains unavailable rather than fabricating a local usable value

#### Scenario: Deny a same-spelled nominal the sealed seed

- **WHEN** ordinary source declares a nominal named Execution with otherwise unrestricted fields
- **THEN** its affinity derives only from its ordinary components and receives no intrinsic local-affinity privilege
