## ADDED Requirements

### Requirement: Semantic facts retain local shared ownership and execution affinity

Analysis SHALL publish `Intrinsic.SharedCore<T>` as one canonical sealed generic nominal with an
affine `LocalSharedStrong` role and local execution affinity. The fact SHALL retain the canonical
element type while exposing no address, count, access-state, layout-lane, or reclaim-authority
representation. Execution affinity SHALL compose recursively through nominal fields, arrays,
normalized unions, shared and exclusive references, borrowed views, callable environments, Effect
environments, and later execution frames. A reference or borrowed view SHALL join the affinity of
its exposed type with the canonical borrow-root dependency it retains. An unresolved canonical type
parameter SHALL contribute an available `ParameterDependent` outcome naming its canonical parameter
identity; specialization SHALL substitute concrete arguments and normalize the complete affinity
again.

The recursive join SHALL be `Unavailable` when any component is unavailable and SHALL retain every
distinct causal diagnostic identity in canonical traversal order. Otherwise it SHALL be
`LocalExecution` when any component is local. Otherwise it SHALL be `ParameterDependent` with the
canonically ordered union of every contributing parameter identity when any component is parameter
dependent. Otherwise it SHALL be `Unrestricted`. An ordinary source declaration MUST NOT gain this
role or affinity from its spelling. `LocalExecution` SHALL identify one same-thread execution domain
and SHALL NOT contain or imply an execution-instance, fiber, or Scheduler identity. Analysis SHALL
publish the deterministic affinity outcome for later execution and parallel-transfer consumers
without adding current Scheduler policy, transfer syntax, a transfer-eligibility verdict, or a
transfer diagnostic.

#### Scenario: Inspect a local shared core fact

- **WHEN** analysis realizes `Intrinsic.SharedCore<Token>`
- **THEN** its fact names the sealed intrinsic nominal, `Token`, the affine `LocalSharedStrong` role, and `LocalExecution` affinity without publishing representation lanes

#### Scenario: Specialize the core through a generic wrapper

- **WHEN** one generic wrapper containing `Intrinsic.SharedCore<T>` is specialized once with a Copy `T` and once with an affine `T`
- **THEN** both specializations retain the canonical intrinsic identity, exact `LocalSharedStrong` role, `LocalExecution` affinity, and their respective canonical element type

#### Scenario: Resolve parameter-dependent aggregate affinity

- **WHEN** analysis realizes a generic nominal containing an unconstrained field `T` before specialization and then specializes it with an unrestricted type and a local shared core type
- **THEN** the generic fact is `ParameterDependent` with exactly `T`'s canonical identity, the unrestricted specialization normalizes to `Unrestricted`, and the local-core specialization normalizes to `LocalExecution`

#### Scenario: Preserve malformed specialization evidence

- **WHEN** analysis attempts to specialize `Intrinsic.SharedCore<Missing>` and the element type cannot be resolved
- **THEN** the core fact remains unavailable with the element-resolution cause and publishes neither `Unrestricted` affinity nor an available local-shared verdict

#### Scenario: Join all-unrestricted components

- **WHEN** a nominal, array, normalized union, callable environment, or Effect environment contains only components with `Unrestricted` affinity
- **THEN** its recursively derived affinity is `Unrestricted`

#### Scenario: Propagate affinity through a capture

- **WHEN** an ordinary Effect or callable captures a nominal value that contains a local shared core
- **THEN** the executable environment's recursively derived affinity is `LocalExecution` even when every other captured field is `Unrestricted`

#### Scenario: Propagate affinity through a retained borrow

- **WHEN** a callable or Effect captures a shared or exclusive reference or borrowed view whose exposed type or canonical borrow-root dependency contains a local shared core
- **THEN** the reference or view and the executable environment both derive `LocalExecution` affinity

#### Scenario: Propagate local affinity through arrays and unions

- **WHEN** an array or normalized union contains one `LocalExecution` component and every remaining available component is `Unrestricted`
- **THEN** the complete value's recursively derived affinity is `LocalExecution`

#### Scenario: Ignore privileged-looking source names

- **WHEN** ordinary source declares nominals named `SharedCore`, `Shared`, `Deferred`, `Scheduler`, or `LocalRuntimeHandle`
- **THEN** analysis publishes ordinary nominal facts for every declaration and grants no local-shared role or intrinsic affinity by spelling

#### Scenario: Preserve unavailable affinity evidence

- **WHEN** one component needed to derive a nominal, array, normalized union, callable, or Effect affinity is unavailable while another component is `LocalExecution`
- **THEN** the complete affinity is `Unavailable`, retains the unavailable component's cause, and is not reported as local or unrestricted

#### Scenario: Order multiple unavailable causes deterministically

- **WHEN** two or more components needed for one aggregate or executable affinity are unavailable with distinct causal diagnostic identities
- **THEN** the complete affinity is `Unavailable` and retains every distinct cause once in canonical component-traversal order

#### Scenario: Publish affinity for a future transfer consumer

- **WHEN** inspection reads a local shared core or recursively containing value that a future parallel-transfer model could consume
- **THEN** it receives the canonical `LocalExecution` outcome without any current transfer syntax, eligibility verdict, or diagnostic

#### Scenario: Keep local affinity independent of execution identity

- **WHEN** a local shared core moves through suspension, parking, resumption, or between independently resumable frames in one same-thread local execution domain
- **THEN** every frame observes the same `LocalExecution` outcome with no execution-instance, fiber, or Scheduler identity in the fact
