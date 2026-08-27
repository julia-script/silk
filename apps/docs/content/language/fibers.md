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
import silk.scheduler { Scheduler }

effect fn work() -> i32 {
  return 42
}

effect fn program() -> i32
! OutOfMemoryError | Scheduler.TaskIdExhaustedError | Fiber.Cancelled
? &mut Scheduler.Scheduler {
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
  return run Effect.catchAll(
    LocalScheduler.execute(&mut scheduler, program()),
    recover,
  )
}
```

`LocalScheduler.execute` stores the lazy root as task zero, provides an owned Scheduler client,
and drives the ready queue until that root terminates. Application code uses Fiber operations; it
does not drive the scheduler loop itself.

The complete error row is deliberate:

- `OutOfMemoryError` reports task-storage allocation refusal;
- `Scheduler.TaskIdExhaustedError` reports exhausted task identities;
- `Fiber.Cancelled` is the join result of structured cancellation; and
- `LocalScheduler.StalledError` means the root is incomplete but no task can make progress.

Fatal traps remain outside this typed recovery path.

## Forking is deferred and atomic

`Fiber.forkChild(work)` consumes a lazy child Effect. The Scheduler prepares every resource the
child needs before publishing it. If preparation fails, the caller receives a typed failure and no
Fiber or runnable child escapes.

Publishing a child only makes it ready. It does not run the child body inline. With
`LocalScheduler`, deterministic FIFO readiness lets `forkChild` return its handle before the
child's first activation.

The child's unresolved requirement row may contain only its owned Scheduler provider. Provide any
other services before forking.

## Observe exactly once

Both observation operations consume the Fiber:

| Operation | Result |
| --- | --- |
| `Fiber.await` | `Fiber.Outcome<A, E>` containing success, typed failure, or cancellation |
| `Fiber.join` | `A`, with the child's `E` and `Fiber.Cancelled` in the failure channel |

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
- [Language reference: Fibers and cooperative scheduling](./reference.md#58-fibers-and-cooperative-scheduling)
- [Standard library: Fiber](./stdlib/fiber.md)
- [Standard library: Scheduler](./stdlib/scheduler.md)
- [Standard library: LocalScheduler](./stdlib/local-scheduler.md)
- [Standard library: Execution](./stdlib/execution.md)
