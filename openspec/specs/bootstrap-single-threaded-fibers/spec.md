# bootstrap-single-threaded-fibers Specification

## Purpose

Define an explicit, source-level single-threaded scheduler and affine Fiber model that composes over
Silk's owner-neutral Execution and Wake substrate without introducing compiler-selected policy.

## Requirements

### Requirement: Applications select and enter a scheduler explicitly

The standard library SHALL define `Scheduler` as an ordinary source service and SHALL define a
`LocalScheduler` provider with an explicit generic `execute` operation. `execute` SHALL receive the
program as a lazy consuming Effect, require one explicit parent `MonotonicClock`, bind owned
Scheduler and scheduler-owned MonotonicClock providers for that program, create the root Execution
as the scheduler's first task, and drive tasks privately until the root terminates. The compiler
MUST NOT select, construct, recognize, or enter a Scheduler or clock provider implicitly.

#### Scenario: Execute a scheduler-owned root

- **WHEN** ordinary `main` constructs a `LocalScheduler`, explicitly provides a parent monotonic
  clock, and passes a lazy program to `execute`
- **THEN** the program begins only after `execute` creates and publishes its root task with owned
  Scheduler and MonotonicClock clients

#### Scenario: Keep the entry operation explicit

- **WHEN** a program can reach Fiber parking but does not call a scheduler entry operation or otherwise construct an explicit Execution owner
- **THEN** the existing unowned-execution diagnostic remains responsible and no scheduler is inserted at `main`

#### Scenario: Keep the parent clock explicit

- **WHEN** an application calls `LocalScheduler.execute` without providing its required parent
  `MonotonicClock`
- **THEN** ordinary requirement closure rejects the entry rather than installing an OS clock

#### Scenario: Select another provider

- **WHEN** an application supplies another ordinary source implementation of `Scheduler`
- **THEN** Fiber operations dispatch to that implementation without compiler registration or construction of `LocalScheduler`

### Requirement: LocalScheduler execute preserves root outcomes

`LocalScheduler.execute` SHALL return the root program's success value and SHALL propagate its typed
failure unchanged. Root Execution construction or scheduler-owned storage allocation refusal SHALL
raise `Allocator.OutOfMemoryError`; an empty ready queue and empty active-registration set while the
root remains incomplete SHALL raise `LocalScheduler.StalledError`. Fatal traps SHALL remain
fatal traps outside typed recovery.

#### Scenario: Return root success

- **WHEN** the root program succeeds with a value after any children it observes have completed
- **THEN** `execute` returns that exact value

#### Scenario: Propagate root typed failure

- **WHEN** the root program fails with a typed value `E`
- **THEN** `execute` raises that same `E` without converting it to a scheduler error

#### Scenario: Report scheduler allocation refusal

- **WHEN** root packaging or scheduler-owned storage cannot be funded before root publication
- **THEN** `execute` raises `Allocator.OutOfMemoryError` after cancelling and cleaning the run

#### Scenario: Reject an unfunded child and continue the run

- **WHEN** timer-capacity reservation or TaskStore insertion fails while publishing a child
- **THEN** `forkChild` raises `Allocator.OutOfMemoryError` to the parent, publishes no child, and
  leaves the scheduler able to continue running existing tasks

#### Scenario: Report a stalled root

- **WHEN** the root is incomplete, the local ready queue is empty, and the active-registration set
  is empty
- **THEN** `execute` raises `LocalScheduler.StalledError` after cancelling and cleaning the remaining task tree

#### Scenario: Wait instead of reporting a timer as stalled

- **WHEN** the root is incomplete, the ready queue is empty, and at least one timer registration is
  active
- **THEN** `execute` waits for the earliest timer rather than raising `LocalScheduler.StalledError`

### Requirement: Child creation is atomic and deferred

`Fiber.forkChild` SHALL accept one consuming lazy Effect whose unresolved runtime requirements are
at most the child Scheduler and child MonotonicClock providers, and SHALL return an affine
`Fiber<A, E>`. It SHALL fund the completion state, child Scheduler client, child clock client,
readiness endpoint, timer-capacity reservation, task identity, and child Execution before making
the child observable. Publication failure SHALL raise its exact typed scheduling or allocation
failure and SHALL not return a Fiber or leave a runnable child. A successfully published child MUST
NOT begin its first activation before `forkChild` returns its Fiber to the parent.

#### Scenario: Fork closed work

- **WHEN** the supplied child Effect has no service requirements
- **THEN** it is accepted by the child contract and runs under the scheduler's owned child environment

#### Scenario: Fork nested Fiber work

- **WHEN** the supplied child Effect uses `Fiber.forkChild` itself
- **THEN** the child receives its own owned Scheduler client and can publish its own child

#### Scenario: Fork timed child work

- **WHEN** the supplied child Effect uses `MonotonicClock.waitFor` or `waitUntil`
- **THEN** the child receives its own scheduler-owned clock client and can park on a timer without
  capturing the parent's clock provider

#### Scenario: Defer first child activation

- **WHEN** a parent successfully publishes a child
- **THEN** the parent resumes and receives the Fiber before the child body performs its first source operation

#### Scenario: Reject publication atomically

- **WHEN** any required child allocation, timer capacity, task identity, or task-store publication fails
- **THEN** `forkChild` raises the exact failure, publishes no child, and ordinary cleanup destroys
  every prepared but unpublished value; successful unused capacity MAY remain reserved

### Requirement: Fiber observation is affine and non-polling

The standard library SHALL expose a consuming `Fiber.await` operation that returns a public
success, typed-failure, or cancellation outcome and a consuming `Fiber.join` operation that returns
the success value while raising the typed failure or `Fiber.Cancelled`. Observing an incomplete
Fiber SHALL park the current Execution and register at most one waiter; observing a completed Fiber
SHALL complete without parking. Neither operation SHALL poll or allocate after the Fiber exists.

#### Scenario: Join a successful child

- **WHEN** a parent consumes a Fiber whose child succeeds with `A`
- **THEN** `join` returns that exact `A`

#### Scenario: Join a failed child

- **WHEN** a parent consumes a Fiber whose child fails with `E`
- **THEN** `join` raises that exact `E`

#### Scenario: Await cancellation as data

- **WHEN** a consumer awaits a Fiber cancelled by structured lifetime cleanup
- **THEN** `await` returns the public cancellation outcome and does not fabricate a child failure

#### Scenario: Park one pending observer

- **WHEN** a task joins an incomplete child
- **THEN** the task relinquishes its Execution once and is made eligible once when the child outcome becomes observable

#### Scenario: Consume the observation authority

- **WHEN** a Fiber has been moved into `await` or `join`
- **THEN** ordinary affine ownership makes a second observation through that handle unrepresentable

### Requirement: Child lifetime is structured by its parent task

Every child created by `forkChild` SHALL remain owned by the task that created it until the child
terminates or the parent terminates. When a parent succeeds, fails, or is cancelled, the scheduler
SHALL cancel every unfinished descendant before another scheduled task can observe the parent's
terminal outcome. Dropping a Fiber handle alone SHALL abandon observation but SHALL NOT detach or
immediately cancel the child. A Fiber handle that escapes through a terminating parent SHALL
observe cancellation if its child was still running.

#### Scenario: Cancel unfinished descendants on parent success

- **WHEN** a parent returns successfully while one or more descendants remain unfinished
- **THEN** the scheduler cancels and cleans the unfinished descendant subtree before dispatching the next task

#### Scenario: Cancel unfinished descendants on parent failure

- **WHEN** a parent raises a typed failure while one or more descendants remain unfinished
- **THEN** the same structured cancellation occurs without replacing the parent's failure

#### Scenario: Keep an unobserved child attached

- **WHEN** a parent drops its Fiber handle but continues running
- **THEN** the child remains scheduled as that parent's child and is cleaned when it or the parent terminates

#### Scenario: Observe an escaped cancelled handle

- **WHEN** a parent returns a Fiber for an unfinished child to its own observer
- **THEN** parent termination cancels that child and consuming the escaped handle observes `Fiber.Cancelled`

### Requirement: Local scheduling is deterministic FIFO

`LocalScheduler` SHALL use one deterministic FIFO ready order. A readiness signal SHALL enqueue a
task at most once until that task is selected; stale readiness for a completed or cancelled task
SHALL be ignored safely. `Fiber.yieldNow` SHALL relinquish the current Execution, append its task to
the ready tail without allocation, and permit it to resume after tasks already ready. Timer
readiness SHALL enter the same queue in deadline and timer-registration order. The driver SHALL
observe event sources after driving at most one selected task so an indefinitely replenished ready
queue cannot starve a due timer.

#### Scenario: Resume a joining parent after completion

- **WHEN** the root parks in `join`, the child completes, and the completion signal wakes the root
- **THEN** the root is appended once and later resumes after its `join`

#### Scenario: Yield behind existing work

- **WHEN** a running task calls `yieldNow` while other tasks are ready
- **THEN** those already-ready tasks are selected before the yielding task resumes

#### Scenario: Yield with no competitor

- **WHEN** a running task calls `yieldNow` and no other task is ready
- **THEN** it becomes eligible without allocation and can be selected again

#### Scenario: Append a due timer behind ready work

- **WHEN** a timer becomes due while one or more tasks are already in the ready queue
- **THEN** the timer's task is appended after those tasks through its existing readiness endpoint

#### Scenario: Observe timers between activations

- **WHEN** runnable tasks continuously yield while a timer reaches its deadline
- **THEN** the scheduler observes and enqueues the due timer without requiring the ready queue to
  become empty

#### Scenario: Ignore a stale ready node

- **WHEN** cancellation or completion removes a task whose preallocated ready node remains queued
- **THEN** the scheduler discards that node without selecting another task or confusing a later task identity

### Requirement: Scheduler shutdown is complete and reusable

Before `LocalScheduler.execute` returns or raises a typed error, it SHALL cancel every unfinished
descendant, disarm every active event registration, release every retained timer Wake, and release
every scheduler-owned task, mailbox, readiness node, timer entry, completion authority, cached clock
state, and container entry. It SHALL restore the provider to an empty reusable state. Fatal traps
remain governed by the language's existing trap cleanup boundary.

#### Scenario: Clean descendants after root completion

- **WHEN** the root reaches any typed terminal outcome with unfinished descendants, including a
  descendant parked on a timer
- **THEN** `execute` performs structured cancellation and returns with no task, timer registration,
  or timer Wake retained by the scheduler

#### Scenario: Reuse one scheduler value

- **WHEN** one call to `execute` returns or raises a typed error and the caller invokes `execute` again on the same `LocalScheduler`
- **THEN** the second program starts with a fresh empty task tree, per-run registration state,
  ready queue, timer queue, and cached parent-clock state, with first-run notifications unreachable

### Requirement: Fiber scheduling remains ordinary source over the existing substrate

Scheduler, LocalScheduler, Fiber, completion, task-storage, and ready-queue behavior SHALL be
implemented in canonical navigable Silk source over `Execution`, `Wake`, `Shared`, allocation, and
collection contracts. The sealed Execution substrate SHALL expose an exactly-once
`notifyInitial` operation which changes a stored `Initial` Execution to `InitialReady` and
synchronously invokes its fixed non-parking readiness endpoint without activating its body. It
MUST NOT choose scheduler policy, allocate, or expose scheduler storage. No source declaration
SHALL receive semantic or lowering privilege from its name or module identity. Native LLVM and
LLVM-generated WebAssembly SHALL agree on results, typed failures, cancellation, wake ordering,
and cleanup.

#### Scenario: Notify only after publication

- **WHEN** an ordinary source scheduler successfully stores a prepared child Execution and calls `Execution.notifyInitial`
- **THEN** the fixed readiness endpoint runs exactly once before the child body and a later drive starts the body from its initial path

#### Scenario: Reject duplicate initial notification

- **WHEN** `Execution.notifyInitial` is called twice or on an Execution outside `Initial`
- **THEN** the existing fatal invalid-state boundary rejects the call without a second readiness notification

#### Scenario: Copy the scheduling protocol

- **WHEN** equivalent scheduler and Fiber declarations are written under other legal names
- **THEN** they receive the same analysis, ownership, lowering, and execution behavior without compiler registration

#### Scenario: Run the vertical root-fork-join program

- **WHEN** `execute` creates a root, the root forks a child, the root parks in `join`, the child completes, and the root resumes
- **THEN** native and LLVM-generated WebAssembly produce the same root result and terminal cleanup state

#### Scenario: Keep trivial programs scheduler-free

- **WHEN** a program does not import or call the scheduler and Fiber source
- **THEN** its analysis and generated artifacts acquire no Scheduler, task store, ready queue, Execution, or Wake machinery from this capability
