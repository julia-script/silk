## ADDED Requirements

### Requirement: Independent execution pressure programs are connected ordinary Silk

The pressure corpus SHALL contain readable connected source programs for source-owned result
waiting, deferred first activation, same-thread timer readiness, cancellation before readiness, and
alternate Coroutine-shaped ownership. Each program SHALL expose its ordinary Allocator and failure
rows at source construction points, close every detachable execution environment with owned values,
and use the same general Execution/Wake substrate. Cross-engine assertions SHALL cover observable
values, activation and readiness order, cleanup/release order, and declared boundary diagnostics.

#### Scenario: Run the Scheduler-shaped connected witness

- **WHEN** the corpus drives a waiter and producer through ordinary task storage and a ready inbox
- **THEN** evaluation, native, and Wasm agree on deferred activation, waiter park, producer publication, task-specific readiness, waiter resume, and final value

#### Scenario: Run the timer-shaped connected witness

- **WHEN** the corpus drives an explicitly owned joining parent and same-thread reactor
- **THEN** all engines agree on sibling progress, timer notification, outer eligibility, result data, and cancellation cleanup

#### Scenario: Run the alternate-owner witness

- **WHEN** the corpus drives one Coroutine-shaped source wrapper through two yielded payloads and completion
- **THEN** all engines agree on payload order and reuse the same intrinsic transitions without Scheduler facts

#### Scenario: Drop the alternate owner while yielded

- **WHEN** the Coroutine-shaped fixture drops its Execution while a yielded payload and Wake remain in the source port
- **THEN** all engines agree on cancellation, exact port/frame/endpoint cleanup, late Wake no-op behavior, and final package release without Scheduler-specific lowering

#### Scenario: Diagnose the unowned root boundary

- **WHEN** a complete entry closes providers but reaches external parking without explicit Execution ownership
- **THEN** the corpus records the stable diagnostic code and span for the missing delimiter without asserting message text or supplying an implicit owner

#### Scenario: Share analysis work cheaply

- **WHEN** several assertions or engines consume one pressure source program
- **THEN** tests build one realized Analysis snapshot, use evaluation for semantics, use Wasm where codegen matters, and route native coverage through the designated differential corpus
