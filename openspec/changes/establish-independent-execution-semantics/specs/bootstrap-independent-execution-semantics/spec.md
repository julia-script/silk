## Purpose

Defines the target-neutral semantic identity, suspension modes, propagation boundary, and affine
lifecycle of an independently resumable Effect execution.

## ADDED Requirements

### Requirement: Suspension modes remain statically distinct

Every reachable complete specialization SHALL have one deterministic suspension summary containing
exactly the modes its static call graph can reach: no suspension, nested transfer, and external-wake
parking. Before complete specialization, an unresolved call SHALL conservatively retain every mode
permitted by its declared static contract. Direct execution MUST NOT acquire suspension machinery;
nested-only execution MUST retain direct-child/LIFO completion policy; an explicit Execution SHALL
own an erased body even when no suspension is reachable; external-wake machinery SHALL appear only
when parking is reachable inside that explicit Execution. Importing or naming ordinary library
actors MUST NOT change a summary.

#### Scenario: Select direct execution

- **WHEN** a complete program can reach neither nested suspension nor external parking
- **THEN** its summary is direct and no suspension or independent-execution machinery is selected

#### Scenario: Preserve the nested transfer tier

- **WHEN** ordinary `run` can reach `Effect.suspend` but cannot reach external parking
- **THEN** its summary selects the existing nested/LIFO transfer tier without Execution or Wake machinery

#### Scenario: Own a non-suspending body explicitly

- **WHEN** ordinary source explicitly constructs an Execution around a non-suspending closed Effect
- **THEN** the result owns an erased never-driven-droppable body but no dormant continuation or wake state

#### Scenario: Select external parking only within a delimiter

- **WHEN** a closed Effect inside explicit Execution construction can transitively reach `Intrinsic.park`
- **THEN** that Execution selects external-park capability and the ordinary source owner driving it does not inherit that capability through `drive`

#### Scenario: Preserve permitted modes before specialization

- **WHEN** an open generic call has not yet selected a complete implementation and its declared contract permits external parking
- **THEN** the summary conservatively includes external parking until each reachable complete specialization is summarized

### Requirement: Independent execution has one owner-neutral affine lifecycle

`Intrinsic.Execution<A>` SHALL be opaque, affine, and initially non-thread-transferable. A newly
constructed value SHALL be Initial; moving an Initial or Eligible value into drive SHALL make it
Running; external parking SHALL relinquish it as Dormant; live readiness SHALL hold it Notifying
until its fixed endpoint returns and then make a still-live execution Eligible; completion SHALL
transfer `A` without returning an Execution; ordinary affine drop SHALL destroy any owned Initial,
Dormant, or Eligible execution. Safe source MUST NOT clone, re-enter, concurrently drive, drive
after completion, or use a moved execution. Driving Dormant or Notifying SHALL perform the defined
fatal intrinsic-state trap before invoking a drive callback.

#### Scenario: Defer first activation

- **WHEN** an owner stores an Initial Execution and drives another eligible execution first
- **THEN** the stored body performs no source operation until its owner later moves it into drive

#### Scenario: Relinquish and later resume

- **WHEN** one drive reaches external parking, a later wake completes notification, and the owner selects the execution again
- **THEN** the first drive returns it only through suspension ownership, notification makes it Eligible only after the endpoint returns, and the later drive resumes at its sole saved continuation

#### Scenario: Complete without a second handle

- **WHEN** a drive completes with `A`
- **THEN** exactly the completion callback receives `A` and the branch state, no Execution is returned, and further drive is unrepresentable

#### Scenario: Trap a premature drive

- **WHEN** an owner attempts to drive a Dormant or Notifying execution
- **THEN** execution traps before progress, endpoint replacement, or any completion or suspension callback

#### Scenario: Keep the lifecycle owner-neutral

- **WHEN** ordinary source implements Scheduler-shaped and Coroutine-shaped owners over the same Execution type
- **THEN** both use the same Initial, drive, dormant, eligible, completion, and drop contracts without compiler recognition of either owner

### Requirement: Explicit execution does not define implicit program-entry ownership

This capability SHALL define ownership only for explicitly constructed Executions. A complete entry
whose specialized graph can reach external parking and has no explicit Execution delimiter MUST be
rejected with responsibility distinct from unsatisfied Effect requirements. The compiler MUST NOT
create a global Scheduler, infer a library owner, or define final outcome delivery. Any implicit
entry adapter requires the separate SLP-0003 contract.

#### Scenario: Reject a park-capable unowned root

- **WHEN** a complete entry closes all service requirements but can reach external parking without an explicit Execution delimiter
- **THEN** analysis rejects the missing execution owner independently of requirement-row checking

#### Scenario: Accept an explicitly owned root body

- **WHEN** ordinary source closes the body environment and explicitly constructs an Execution for a source driver
- **THEN** the external-park capability is owned by that Execution and no implicit root policy is selected
