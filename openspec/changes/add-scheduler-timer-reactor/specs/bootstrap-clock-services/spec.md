## ADDED Requirements

### Requirement: LocalScheduler provides a task-local monotonic timeline

`LocalScheduler.execute` SHALL require one explicitly supplied parent `MonotonicClock` and SHALL
lexically replace that capability inside every owned task with a scheduler-provided
`MonotonicClock`. The scheduler-provided `now` SHALL return the parent mark cached for the current
scheduler turn without parking, and its `getResolution` SHALL return the positive parent resolution
cached for the run. Equal reads within or across turns SHALL remain valid. The scheduler-provided
`waitUntil` and `waitFor` SHALL preserve the same logical parent timeline while parking only the
calling task.

For a positive relative wait, the driver SHALL make one fresh call to its parent provider after the
task parks, add the duration without wrapping, and arm that absolute deadline. The resulting mark
MAY itself be cached when the parent is another scheduler-owned clock; freshness is defined by the
provider-call timeline rather than by an uncached operating-system read. A zero relative wait and
an absolute deadline already reached on the scheduler's cached timeline SHALL complete without
parking. Direct use of `OsMonotonicClock`, including an explicit lexical replacement inside a
scheduled task, SHALL retain the existing blocking provider behavior.

#### Scenario: Read cached time without yielding

- **WHEN** a task calls scheduler-provided `MonotonicClock.now` more than once during one activation
- **THEN** each call completes without parking and may return the same cached canonical mark

#### Scenario: Cache the parent resolution

- **WHEN** a scheduler run begins under a parent monotonic provider with a positive resolution
- **THEN** task-local `getResolution` returns that resolution without dispatching another parent
  query from the task

#### Scenario: Start a relative wait from one fresh parent-provider call

- **WHEN** a task calls scheduler-provided `waitFor` with a positive duration
- **THEN** the driver makes one fresh parent-provider call, derives one checked absolute deadline,
  and does not reuse the earlier mark cached for that inner scheduler turn

#### Scenario: Park only the waiting task

- **WHEN** one scheduled task calls scheduler-provided `waitUntil` or `waitFor` for a future deadline
  while another task is ready
- **THEN** the waiting task parks and the ready task may run before the deadline is reached

#### Scenario: Complete an already reached wait locally

- **WHEN** a task requests `waitFor(0)` or a `waitUntil` deadline no later than the cached mark
- **THEN** the operation returns without parking or requiring positive parent-clock advancement

#### Scenario: Preserve direct OS blocking

- **WHEN** a task explicitly provides `OsMonotonicClock` around an inner wait instead of using the
  scheduler-provided clock
- **THEN** that inner wait may block the host thread according to the existing OS-provider contract

### Requirement: Monotonic deadlines use one canonical checked derivation

The public `MonotonicClock` actor SHALL expose a pure `deadlineAfter(start, duration)` operation
that validates canonical monotonic components, performs checked split-field duration addition, and
traps before seconds overflow or representation wrap. `OsMonotonicClock.waitFor` and the
scheduler-owned relative-wait path SHALL both use this operation rather than maintain duplicate
deadline arithmetic.

#### Scenario: Carry fractional duration canonically

- **WHEN** `deadlineAfter` adds a duration whose fractional component crosses one second
- **THEN** it returns canonical seconds and nanoseconds with the carry applied exactly once

#### Scenario: Preserve negative-origin arithmetic

- **WHEN** `deadlineAfter` adds a nonnegative duration to a canonical mark before the epoch
- **THEN** it returns the exact canonical later mark without unsigned conversion

#### Scenario: Trap before deadline overflow

- **WHEN** the checked sum cannot be represented as a canonical monotonic mark
- **THEN** `deadlineAfter` traps before wrapping or returning a reused deadline
