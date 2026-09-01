# Fibers and local scheduling

Silk's standard library provides structured cooperative concurrency with explicit scheduler
ownership. A `Fiber<A, E>` is an affine handle to one child computation: it can be observed once,
and its lifetime remains attached to the task that created it.

This is concurrency, not parallelism. `LocalScheduler` runs one task at a time on one thread. A
task continues until it completes, parks while waiting, or calls `Fiber.yieldNow`.

## Run a program under a scheduler

The application creates a scheduler and selects its entry operation explicitly. Silk does not
install a global scheduler or infer one at `main`:

```silk
import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
import silk.fiber { Fiber }
import silk.local_scheduler { LocalScheduler }
import silk.monotonic_clock as MonotonicClock
import silk.scheduler { Scheduler }
import silk.system_clock as SystemClock
import silk.system_clock { Instant }

struct ParentClock {
  mark: Instant
}

effect fn parentNow(self: &mut ParentClock) -> Instant {
  return SystemClock.make(
    SystemClock.seconds(&self.mark),
    SystemClock.nanoseconds(&self.mark),
  )
}

effect fn parentResolution(self: &mut ParentClock) -> u64 { return 1 }

effect fn parentWaitUntil(self: &mut ParentClock, deadline: Instant) -> () {
  self.mark = move deadline
  return ()
}

effect fn parentWaitFor(self: &mut ParentClock, duration: u64) -> () {
  let deadline = MonotonicClock.deadlineAfter(&self.mark, duration)
  return run parentWaitUntil(move self, move deadline)
}

impl MonotonicClock.MonotonicClock for ParentClock {
  now: ParentClock.parentNow
  getResolution: ParentClock.parentResolution
  waitUntil: ParentClock.parentWaitUntil
  waitFor: ParentClock.parentWaitFor
}

effect fn work() -> i32 {
  return 42
}

effect fn program() -> i32
! OutOfMemoryError | Scheduler.TaskIdExhaustedError | Fiber.Cancelled
? &mut Scheduler.Scheduler | &mut MonotonicClock.MonotonicClock {
  let child = run Fiber.forkChild<i32, never>(work())
  return run Fiber.join<i32, never>(move child)
}

effect fn recover(
  error: OutOfMemoryError
    | Scheduler.TaskIdExhaustedError
    | Fiber.Cancelled
    | LocalScheduler.StalledError,
) -> i32 {
  drop error
  return -1
}

pub fn main() -> i32 {
  let mut scheduler = LocalScheduler.make()
  let mut clock = ParentClock { mark: SystemClock.make(0, 0) }
  let scheduled = Effect.catchAll(
    LocalScheduler.execute(&mut scheduler, program()),
    recover,
  ) |> Effect.provideMut<MonotonicClock.MonotonicClock>(&mut clock)
  return run move scheduled
}
```

`LocalScheduler.execute` stores the lazy root as task zero, provides owned Scheduler and
MonotonicClock clients, and drives the ready queue and private event sources until that root
terminates. The explicit outer clock is the driver's event-wait boundary: a deterministic provider
can advance logical time, while `OsMonotonicClock` blocks the host thread only when the scheduler
has no ready task. Application code uses Fiber and clock operations; it does not drive the scheduler
loop itself.

The complete error row is deliberate:

- `OutOfMemoryError` reports task-storage allocation refusal;
- `Scheduler.TaskIdExhaustedError` reports exhausted task identities;
- `Fiber.Cancelled` is the join result of structured cancellation; and
- `LocalScheduler.StalledError` means the root is incomplete with no ready task or active event
  registration remaining.

Fatal traps remain outside this typed recovery path.

## Forking is deferred and atomic

`Fiber.forkChild(work)` consumes a lazy child Effect. The Scheduler prepares every resource the
child needs before publishing it. If preparation fails, the caller receives a typed failure and no
Fiber or runnable child escapes.

Publishing a child only makes it ready. It does not run the child body inline. With
`LocalScheduler`, deterministic FIFO readiness lets `forkChild` return its handle before the
child's first activation.

The child's unresolved requirement row may contain only its owned Scheduler and MonotonicClock
providers. Provide any other services before forking.

## Sleeping parks one Fiber

Inside `LocalScheduler.execute`, `Effect.sleep`, `MonotonicClock.waitFor`, and
`MonotonicClock.waitUntil` use the scheduler-owned clock replacement. A future deadline stores the
task's Wake in its private registration state while the timer queue indexes only the registration
identity, then returns control to the driver so ready siblings continue before the timer fires.
Zero-duration and reached deadlines return immediately.

Clock reads are cached for one driver turn, so repeated `MonotonicClock.now()` calls in one task
activation may be equal. The driver refreshes its mark before each selected task and derives a
positive relative wait from one fresh parent-provider read. When no task is ready, it calls the
outer provider's blocking `waitUntil` for the earliest timer. Calling `OsMonotonicClock` directly,
outside a scheduler-owned task clock, still blocks the calling host thread.

## Observe exactly once

Both observation operations consume the Fiber:

| Operation     | Result                                                                   |
| ------------- | ------------------------------------------------------------------------ |
| `Fiber.await` | `Fiber.Outcome<A, E>` containing success, typed failure, or cancellation |
| `Fiber.join`  | `A`, with the child's `E` and `Fiber.Cancelled` in the failure channel   |

If the child is complete, observation returns immediately. Otherwise the current task parks until
completion wakes it. Reusing the consumed handle reports the ordinary ownership diagnostic
`OWN0001`.

Dropping a Fiber abandons observation; it does not cancel or detach the child. The child remains
owned by its parent task.

## Child lifetime is structured

Every child stays linked to the parent task that forked it. When a parent succeeds, fails, is
cancelled, or is stopped during stalled shutdown, the scheduler cancels and releases every
unfinished descendant before another task can observe the parent's terminal result.

The current API has no detached task, daemon task, reparenting, or public interrupt operation.
Returning or dropping a Fiber handle does not change the task tree.

## Readiness is deterministic FIFO

`LocalScheduler` appends readiness notifications to one FIFO queue. `Fiber.yieldNow` places the
current task behind tasks that are already ready. Duplicate readiness is suppressed while a task
is queued.

FIFO is a readiness rule, not preemption or a fairness guarantee. CPU-bound work that never parks,
yields, or completes prevents other local tasks from running. Parallel execution, multithreading,
and preemptive scheduling are not part of the current alpha.

## Fibers and suspension are different

`Effect.suspend` gives a recursive Effect cycle an explicit stack-safe boundary. It does not create
a task, park, yield, or choose a scheduler.

Fibers are independently resumable tasks owned by a Scheduler. Use suspension for stack safety
inside one computation; use Fibers when computations must make cooperative progress independently.

## See also

- [Effects, failures, and services](./effects.md)
- [Recursion and stack safety](./recursion.md)
- [Language reference: single-threaded schedulers and Fibers](../reference/single-threaded-fibers.md)
- [Standard library: Fiber](./stdlib/fiber.md)
- [Standard library: Scheduler](./stdlib/scheduler.md)
- [Standard library: LocalScheduler](./stdlib/local-scheduler.md)
- [Standard library: Execution](./stdlib/execution.md)
