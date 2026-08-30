## Why

The native monotonic-clock waits block the host thread, so calling `MonotonicClock.waitFor` or
`waitUntil` inside `LocalScheduler` prevents every other ready Fiber from advancing. The scheduler
needs to own timed suspension now, while establishing a registration lifecycle that can later admit
HTTP, WebSocket, and other event sources without replacing the Fiber or ready-queue model.

## What Changes

- **BREAKING** Make `LocalScheduler.execute` consume a parent `MonotonicClock` capability and bind a
  scheduler-owned `MonotonicClock` client, alongside the `Scheduler` client, into every root and
  child task.
- **BREAKING** Extend the Scheduler child-preparation contract so forked work may use both its owned
  `Scheduler` and scheduler-owned `MonotonicClock` providers.
- Make scheduler-provided `waitFor` and `waitUntil` park only the calling Execution and register one
  timer interest instead of invoking the parent provider's blocking wait from the task.
- Add a private per-run registration lifecycle and stable deadline heap. Each parked generation
  retains exactly one Wake, and due timers enter the existing FIFO ready queue in deadline and
  registration order.
- Give the scheduler a libuv-style cached monotonic mark per turn. Relative waits obtain one fresh
  parent mark in the driver before their absolute deadline is armed.
- Publish one canonical checked `MonotonicClock.deadlineAfter` operation so direct OS waits and
  scheduler-owned relative waits cannot diverge in split-field deadline arithmetic.
- Change the run loop so an empty ready queue waits for the earliest timer and raises
  `LocalScheduler.StalledError` only when the incomplete root has no runnable task or active event
  registration.
- Make cancellation and typed shutdown disarm timer interests, detach retained Wakes, destroy
  dormant Executions, and then release the detached Wakes as the final package authority.
- Keep the registration machinery private in this slice. It is shaped for later multi-interest and
  non-timer event sources, but this change does not publish a speculative Reactor or raw I/O SPI and
  does not define timer-versus-I/O tie ordering.

## Capabilities

### New Capabilities

- `scheduler-timers`: Scheduler-owned timed suspension, registration identity and lifecycle,
  deterministic timer ordering, event-loop progress, and timer cleanup.

### Modified Capabilities

- `bootstrap-clock-services`: Distinguish directly supplied blocking OS waits from the
  scheduler-owned clock provider whose waits park only the current task and whose reads use the
  scheduler's cached parent timeline.
- `bootstrap-single-threaded-fibers`: Extend task environments, readiness progress, stalled
  detection, fairness, cancellation, shutdown, and provider reuse to include scheduler-owned timer
  registrations.

## Impact

- Standard-library actors: `silk.scheduler`, `silk.fiber`, `silk.local_scheduler`,
  `silk.monotonic_clock`, and their generated manifest and navigation documentation.
- Scheduler and clock fixtures, evaluator semantics, direct-Wasm coverage with pure scripted clock
  providers, and native acceptance using `OsMonotonicClock` as the parent event-loop clock.
- Existing `LocalScheduler.execute` call sites must explicitly provide a parent monotonic clock, and
  custom Scheduler implementations must provide the expanded child environment.
- No new compiler-known scheduler, timer, event-loop actor, intrinsic, ambient provider, public
  Reactor service, socket polling API, or cross-thread wake mechanism is introduced.
