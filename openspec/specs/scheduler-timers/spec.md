# Scheduler Timers Specification

## Purpose

Define scheduler-owned timed suspension and its event-registration lifecycle so waiting parks one
task, preserves deterministic local progress, and can later compose with non-timer event sources.

## Requirements

### Requirement: One parked generation owns one event registration

For every future scheduler-owned monotonic wait, `LocalScheduler` SHALL park the calling task once
and associate that parked Execution generation with exactly one active registration and exactly one
affine Wake. A registration identity SHALL distinguish the task and its parked generation, SHALL
not be reused while a stale source notification could still exist, and SHALL make a notification
for a cancelled, completed, or older generation inert. This change SHALL arm exactly one timer
interest per wait and SHALL publish no public multi-interest API. Before root publication or child
adoption, the scheduler SHALL reserve enough timer-index capacity for every live task plus the
candidate task. Once a Wake is installed, request installation, arming, claiming, and disarming
SHALL allocate nothing and SHALL add no typed failure path.

Registration identities and source indexes SHALL be per-run for the timer-only backend, and typed
teardown SHALL make every notification from that run unreachable before a later run may restart its
counters.

#### Scenario: Arm one timer for one parked generation

- **WHEN** a task calls a scheduler-owned wait for a future deadline
- **THEN** the task relinquishes its Execution once and the scheduler retains one registration and
  one Wake until the timer completes or is cancelled

#### Scenario: Ignore an obsolete generation

- **WHEN** a timer notification names a task generation that has already completed, been
  cancelled, or been superseded by a later park
- **THEN** the notification cannot make the task's current Execution eligible

#### Scenario: Pre-fund arming before publication

- **WHEN** a root or child becomes observable and later parks on its first timer
- **THEN** the timer is armed without allocating storage or introducing another typed failure

#### Scenario: Keep the future event boundary private

- **WHEN** the timer capability is imported or executed
- **THEN** no public Reactor service, raw polling token, multi-interest wait, or non-timer event API
  becomes part of the application contract

### Requirement: Timer order is stable and readiness remains FIFO

`LocalScheduler` SHALL order active timers by their absolute monotonic deadlines. Timers with
different deadlines SHALL become eligible in ascending deadline order, and timers with equal
deadlines SHALL become eligible in registration order. Claiming a due timer SHALL consume its Wake
at most once and use the task's existing readiness endpoint, so newly due tasks append to the same
FIFO ready queue as Fiber completion and `yieldNow` readiness.
Registration order SHALL come from a checked per-run monotonically increasing counter, and the
scheduler SHALL trap before that counter can wrap or reuse an order value.

#### Scenario: Wake the earliest deadline first

- **WHEN** several tasks wait for distinct future deadlines
- **THEN** the scheduler makes each task eligible in ascending deadline order as the parent clock
  reaches those deadlines

#### Scenario: Preserve equal-deadline registration order

- **WHEN** several tasks register the same absolute deadline
- **THEN** they append to the ready queue in the order their timer registrations were accepted

#### Scenario: Retain FIFO position after timer completion

- **WHEN** a timer becomes due while other tasks are already ready
- **THEN** its task is appended after those ready tasks and before tasks made ready by later
  notifications

#### Scenario: Refuse registration-order reuse

- **WHEN** accepting another timer would exhaust the per-run registration-order counter
- **THEN** the scheduler traps before publishing a duplicate order value or arming the timer

### Requirement: The run loop observes timers without starving tasks or deadlines

Each scheduler turn SHALL refresh the scheduler-owned cached mark from the parent monotonic clock,
claim every timer due at that mark, and drive at most one selected ready task before observing timer
registrations again. When no task is ready and at least one timer remains active, the timer-only
backend SHALL wait against the parent provider until the earliest absolute deadline, refresh the
mark, and resume timer collection. An incomplete root SHALL be stalled only when its ready queue is
empty and no active timer registration remains. An active timer registration prevents stalled
detection until it completes or is cancelled.

#### Scenario: Let ready work advance during another task's wait

- **WHEN** one task waits for a future timer while another task is ready
- **THEN** the scheduler drives the ready task without first blocking the host until the timer
  deadline

#### Scenario: Prevent yield traffic from starving a due timer

- **WHEN** ready tasks repeatedly yield while a timer becomes due
- **THEN** the scheduler observes the timer between task activations and eventually appends its task
  to the ready queue

#### Scenario: Block only with no ready work

- **WHEN** the ready queue is empty and the earliest active timer is in the future
- **THEN** the scheduler may block its host thread against the parent clock until that deadline

#### Scenario: Do not stall with a timer pending

- **WHEN** the root is incomplete, no task is ready, and at least one timer registration remains
  active
- **THEN** `LocalScheduler.execute` waits for timer progress instead of raising
  `LocalScheduler.StalledError`

#### Scenario: Let an idle nested scheduler yield outward

- **WHEN** an inner scheduler has no ready task and waits on a timer through its outer
  scheduler-owned parent clock
- **THEN** the inner wait parks the outer task and unrelated outer ready tasks may progress

#### Scenario: Keep nested execute a synchronous cooperative scope

- **WHEN** an inner scheduler continuously retains ready work
- **THEN** its `execute` call does not promise outward fairness, and inner `yieldNow` operations
  schedule only among inner tasks until the inner scheduler becomes idle or terminates

### Requirement: In-driver timer removal releases every retained authority

When a timed task completes through another permitted path, is cancelled by structured lifetime,
or is removed during typed scheduler shutdown while the driver is active, `LocalScheduler` SHALL
disarm its active registration, remove its timer interest, and take its retained Wake without
signaling readiness before destroying the dormant Execution, then drop the detached Wake as the
final package authority. Normal timer completion SHALL remove the same registration before invoking
its Wake exactly once. Typed scheduler shutdown SHALL leave no registration, timer entry, Wake, or
cached per-run clock state observable by a later `execute` call.

#### Scenario: Cancel a sleeping descendant

- **WHEN** a parent terminates while its child is parked on a future timer
- **THEN** structured cancellation removes the child's timer, detaches its Wake, destroys the
  child's dormant Execution, and then releases the detached Wake

#### Scenario: Complete a timer exactly once

- **WHEN** a timer reaches its deadline and its task is still parked on the matching generation
- **THEN** the scheduler removes the registration and consumes its Wake exactly once

#### Scenario: Reuse after timed shutdown

- **WHEN** one `execute` call ends with active timers and the same `LocalScheduler` value executes a
  second program
- **THEN** the second run starts with fresh per-run registration state, an empty timer set, and no
  reachable notification or readiness from the first run

### Requirement: Whole-driver destruction cancels unfinished Fibers without readiness

When a containing Execution destroys a parked `LocalScheduler.execute` instead of letting its drive
loop perform in-driver removal, destruction of the scheduler's complete per-run owner SHALL release
every inner Execution, completion authority, registration, timer entry, and retained Wake. It SHALL
publish cancellation to observers of unfinished inner Fibers and SHALL NOT invoke a retained Wake or
otherwise publish readiness while releasing the run.

#### Scenario: Destroy an outer task parked in a nested scheduler

- **WHEN** an outer scheduler destroys a task whose nested `LocalScheduler.execute` remains parked
  with an incomplete root and active timer descendants
- **THEN** the nested scheduler releases every inner registration, Wake, and Execution
- **AND** an escaped Fiber observing an unfinished inner task receives `Fiber.Cancelled` exactly once
- **AND** no cancelled inner timer resumes
- **AND** every allocation owned by the nested run is released

### Requirement: Timer policy remains ordinary source

Scheduler-owned clock clients, registrations, timer ordering, cached time, and run-loop policy SHALL
remain canonical ordinary Silk source over the existing `Execution`, `Wake`, `Shared`, allocation,
collection, and clock contracts. No source declaration SHALL receive semantic or lowering privilege
from its name or module identity. This capability SHALL add no compiler-selected scheduler, timer,
ambient clock, cross-thread wake delivery, or new clock intrinsic.

#### Scenario: Dispatch an ordinary scheduler provider under other names

- **WHEN** an ordinary-source `Scheduler` provider is written under other legal declaration and
  module names
- **THEN** the compiler dispatches `Scheduler.prepare` through the public contract without
  recognizing canonical `LocalScheduler` or timer declarations
