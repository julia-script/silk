## Context

See [proposal.md](proposal.md) for motivation. The compiler and standard library already provide:

- affine `Intrinsic.Execution<A>` values with explicit construction, drive, suspension ownership,
  completion, cancellation-by-drop, and engine parity;
- affine `Intrinsic.Wake` values with at-most-once readiness notification;
- `Execution.park`, whose non-parking registration callback installs one Wake and whose guard is
  retained while dormant;
- detached `Shared<T>` storage with callback-scoped access; and
- ordinary allocator, HashMap, Vector, and Effect source.

The independent-execution pressure fixture already proves a fixed two-task Deferred/Fiber-shaped
program. It deliberately does not provide dynamic task publication, a reusable scheduler, root
ownership, structured cancellation, or public API. `ExecutionBoundary` also correctly rejects a
park-capable root which has no explicit owner.

The scheduling design must remain ordinary Silk source. In particular, it cannot make the compiler
recognize `Scheduler`, `Fiber`, or `LocalScheduler`, cannot make Wake callbacks allocate, and cannot
retain a borrowed application provider inside a detached Execution. The compiler-owned Execution
substrate may expose only the target-neutral initial-readiness transition that ordinary schedulers
need after publishing a prepared Execution.

## Goals / Non-Goals

**Goals:**

- Define one small provider protocol and one deterministic local implementation.
- Make the root and every child the same scheduler-owned task shape.
- Make task creation an all-or-nothing scheduling boundary with deferred first activation.
- Make join, completion notification, cancellation, and cooperative yield allocation-free after
  task construction.
- Prevent ownership cycles and stale-readiness ABA without weak references or a scheduler-specific
  intrinsic.
- Preserve typed program failures while separating scheduler failures and fatal traps.
- Leave `LocalScheduler` empty and reusable after every typed exit.

**Non-Goals:**

- An implicit scheduler at `main` or a compiler-owned root adapter.
- Parallel or thread-transferable execution, work stealing, priorities, timers, I/O reactors, or
  cross-thread Wake delivery.
- Detached or daemon Fibers, explicit public interruption, racing, timeouts, Fiber-local storage,
  or multi-consumer results in the first slice.
- A universal task representation, general-purpose async runtime, or general deque abstraction.
- Compatibility with the fixed test-local scheduler prototypes or deleted SLP document structure.

## Decisions

### 1. Entry is a concrete provider operation named `execute`

The application constructs its selected provider and explicitly calls its entry operation:

```silk
pub fn main() -> i32 {
  let mut scheduler = LocalScheduler.make()
  return run LocalScheduler.execute(&mut scheduler, program())
}
```

Conceptually, the contract is:

```silk
pub effect fn execute<A, E>(
  self: &mut LocalScheduler,
  program: once Effect<A ! E ? &mut Scheduler>
) -> A
! E | Allocator.OutOfMemoryError | LocalScheduler.StalledError
```

The lazy program has no application borrow when it becomes detached. A closed Effect is valid in
the declared child/root contract; a program using Fiber operations consumes the owned Scheduler
provider installed by `execute`. Other application services must be closed with owned providers
before this boundary.

`execute` reifies the root's typed success or failure into its completion storage, wraps that work
in `Execution<()>`, and drives it as task zero. A trap is never materialized as a Fiber outcome.

**Alternatives considered:** `run` reads awkwardly as `run Scheduler.run`; `await` describes
observing an existing Fiber; `start` can imply returning before completion. A separate root adapter
would duplicate the scheduler's ownership responsibility. An effectful `main` requiring Scheduler
would create an implicit provider-selection problem the language intentionally does not solve.

### 2. Scheduler is a service; Fiber is the application-facing API

`Scheduler` is an ordinary service implemented by a per-task client. Its provider-facing operation
prepares one child publication. Canonical application code uses:

```silk
effect fn program() -> i32
? &mut Scheduler {
  let child = run Fiber.forkChild(work())
  return run Fiber.join(move child)
}
```

The initial Fiber module exposes:

- `forkChild<A, E>(once Effect<A ! E ? &mut Scheduler>) -> Fiber<A, E>` with allocation and task-id
  failures;
- consuming `await<A, E>(Fiber<A, E>) -> Fiber.Outcome<A, E>`;
- consuming `join<A, E>(Fiber<A, E>) -> A ! E | Fiber.Cancelled`; and
- `yieldNow() -> ()` over the current Execution's fixed readiness endpoint.

`await`, `join`, and `yieldNow` do not need a Scheduler service dispatch: they compose directly over
the completion and current-Execution protocols. `forkChild` is the only initial Fiber operation
which selects the Scheduler provider. The provider operation remains a documented SPI so custom
ordinary-source schedulers can implement the service, but `Fiber.forkChild` is the canonical
application call.

The public source may use a harmless module import cycle between `silk.scheduler` and `silk.fiber`;
Silk already plans module cycles as one deterministic component. The value layout itself remains
indirect through Shared cells and Execution packages, so this does not create an invalid recursive
inline type.

**Alternatives considered:** a compiler-known Scheduler would violate minimal compiler privilege.
A marker service with no operation cannot give Fiber code access to provider policy. Putting root
execution on the service itself would require calling an operation on a provider that does not yet
exist inside the program.

### 3. Every task receives a distinct owned Scheduler client

Each task body binds an owned `SchedulerClient` containing:

- that task's monotonic `TaskId`;
- its own `Shared<TaskMailbox>`;
- shared handles to the per-execute identity source and ready inbox; and
- the readiness endpoint/node reserved for that task.

Creating a child also creates a new child mailbox and client before constructing the child
Execution. A parent mailbox can temporarily own a prepared child Execution, but that Execution owns
the different child mailbox. The graph is therefore a tree:

```text
parent task -> parent Execution -> parent client -> parent mailbox
                                      |
parent mailbox -> prepared child Execution -> child client -> child mailbox
```

No mailbox points to its own TaskEntry, the TaskStore, or the scheduler. A ready endpoint points only
to its ready inbox and preallocated node. These restrictions prevent strong ownership cycles while
still allowing arbitrary nested forks.

**Alternatives considered:** placing the TaskStore in every client creates a direct
task-store/Execution/provider cycle. One global submission mailbox creates a cycle when a queued
child Execution captures the same mailbox. Weak references would add a new ownership primitive to
solve a topology problem ordinary source can avoid.

### 4. Fork uses prepare, park, publish, and resume

`Fiber.forkChild` is one atomic scheduling boundary:

1. The selected Scheduler client allocates the result payload, non-generic completion signal,
   child mailbox, ready node, child client, and `Execution<()>`, and reserves a never-reused TaskId.
2. It returns an affine pending-publication value to the Fiber wrapper, ending the exclusive
   service dispatch before any park.
3. The wrapper's non-parking park registration moves the prepared submission and the parent's Wake
   into the parent's one-slot mailbox.
4. `Execution.drive` returns the parked parent to the driver. The driver attempts the only
   publication-time growing operation: insertion into its task HashMap.
5. On success, the driver records `Published`, wakes/enqueues the parent first, then calls
   `Execution.notifyInitial` on the stored child. The child's fixed readiness endpoint enqueues its
   preallocated node. FIFO order therefore makes `forkChild` return before the child's first
   activation.
6. On insertion refusal, the driver destroys the prepared child, records `Rejected`, and wakes the
   parent. The wrapper raises the exact refusal and returns no Fiber.

A task cannot begin another fork until this fork resumes, so its mailbox needs one publication slot
rather than a growing submission queue. Every allocation precedes the mailbox commit. Wake paths,
mailbox state changes, parent restoration, and ready-node linking are all infallible.

**Alternatives considered:** returning the Fiber immediately after placing a submission in a
fallible queue exposes a handle before task-store publication succeeds. Letting the child run first
breaks the chosen deferred-first-activation contract. Parking inside the service operation retains
an unnecessary exclusive provider access across suspension; returning an affine pending token
keeps the ownership proof local.

Initial readiness is one explicit transition on the existing compiler-owned Execution substrate:

```text
Initial --Execution.notifyInitial--> InitialReady --Execution.drive--> Running
```

`notifyInitial` takes an exclusive borrow of a successfully stored Execution, changes the state
before synchronously invoking the fixed non-parking readiness endpoint, and succeeds exactly once.
It does not activate the body, allocate, choose a queue, expose scheduler storage, or touch Wake
generation. A duplicate call or a call in another state is a fatal invalid-state trap. Dropping an
`InitialReady` Execution performs the same cleanup as dropping an unstarted `Initial` Execution.
This is the smallest target-neutral primitive that lets an ordinary source scheduler publish a
prepared Execution atomically before making it ready.

**Alternatives considered:** storing an executable publication closure would require a structural
Effect to have a runtime representation. Returning an opaque provider-specific publication value
from a bodyless service operation is not a supported ABI. Exposing the LocalScheduler ready node in
the Scheduler protocol would leak policy. Automatically notifying inside `Execution.make` would
make readiness observable before task-store insertion can succeed.

### 5. The TaskStore is driver-owned and identities are never reused within execute

The scheduler driver owns one ordinary `HashMap<TaskId, TaskEntry>`; it is not Shared and never
appears in a task capture. `TaskId` is a monotonic `u64` reserved from a small Shared identity source.
IDs are never reused within one `execute`, so stale ready nodes cannot name a different task. The
counter resets only after the previous run's complete typed shutdown and a new ready inbox has been
created. Overflow raises a typed `Scheduler.TaskIdExhaustedError` during fork preparation.

`TaskEntry` stores the optional suspended/initial Execution, parent and intrusive child links,
mailbox and completion-signal handles, its ready node, and one cancellation-worklist link. While an
activation is being driven, the map entry remains present in a `Running` state and the Execution is
moved out. Suspension restores it to that existing entry; completion removes the entry after
descendant cleanup.

The current HashMap needs one general source-level foundation: callback-scoped mutable access to an
existing value. This permits moving an Execution out of a TaskEntry and later restoring it without
removing and fallibly reinserting the key/value pair. Initial task insertion remains explicitly
fallible; mutation of an existing entry is allocation-free and returns no escaping borrow.

**Alternatives considered:** `{slot, generation}` reuse adds ABA bookkeeping without saving an
allocation in this first implementation. Removing a task before every drive makes ordinary
reinsertion a new OutOfMemoryError scheduling point. A Shared TaskStore reintroduces access loans
at every suspension transition and makes ownership cycles easier to form.

### 6. Readiness uses one intrusive preallocated node per task

`LocalScheduler` owns a `Shared<ReadyQueue>` with head and tail Shared node handles. Every task
preallocates one `Shared<ReadyNode>` containing its TaskId, next link, and `enqueued` bit. The fixed
Execution readiness endpoint owns clones of the queue and that node.

The non-parking ready callback appends the node only when `enqueued` is false. Append clones and
links existing Shared handles; it never allocates. Dequeue clears `next` and `enqueued`, allowing
the same task node to be used for a later suspension generation. A missing TaskId in the TaskStore
means the node is stale and is discarded. Queue access ends before the selected task is driven, so
the callback never conflicts with a driver-held Shared access.

`yieldNow` parks with a registration that immediately consumes its Wake. The fixed endpoint appends
the current node at the ready tail. If there is no competitor, the same task is simply the next
selection.

**Alternatives considered:** Vector append/reserve is effectful and therefore illegal in the ready
callback. Scanning every task for eligibility weakens policy clarity and scales poorly. A general
deque would solve a larger problem than this capability needs.

### 7. Completion separates a generic payload from a type-erased cancellation signal

Each Fiber owns two Shared cells allocated before publication:

- `CompletionPayload<A, E>`, which stores exactly one success or typed-failure outcome; and
- non-generic `CompletionSignal`, whose phase is Pending with at most one waiter, Ready, or
  Cancelled.

The task wrapper owns producer handles. Normal completion writes the payload first, changes the
signal to Ready, consumes any waiter Wake, and returns `()` from the homogeneous task Execution.
The TaskEntry needs only a clone of the non-generic signal, so the scheduler can publish Cancelled
for an arbitrary task before dropping its Execution. A consumer which sees Ready takes the generic
payload exactly once; a consumer which sees Cancelled needs no payload.

If a task completes normally, waking its observer can enqueue that observer before the drive
completion callback runs. This is safe in the single-threaded driver: the callback cancels the
completed task's descendants and removes the entry before the loop can dispatch any newly ready
observer. Parent completion is therefore not observably published before structured cleanup.

**Alternatives considered:** storing a generic completion cell directly in homogeneous TaskEntry
requires unsafe erasure or a heterogeneous callback. Dropping a cancelled Execution without a
separate signal leaves an escaped Fiber pending forever. Multi-waiter storage would introduce
allocation and cloning policy not required by an affine Fiber.

### 8. Parent-child links define structured lifetime

Every TaskEntry has one parent ID and intrusive child/sibling links. Normal child completion unlinks
the child. Parent success, typed failure, cancellation, or stalled shutdown walks every unfinished
descendant, publishes Cancelled, drops its Execution, removes its TaskEntry, and leaves any queued
ready node stale.

Cancellation uses the existing per-entry cancellation link as an allocation-free iterative
worklist. It does not recurse on the native stack and cannot fail partway through cleanup. Dropping a
Fiber handle changes only observation ownership; it does not mutate the task tree. No operation in
the first slice detaches or reparents a child. Consequently, a Fiber returned through its parent's
terminal value is valid but observes Cancelled if the child had not already completed.

**Alternatives considered:** cancellation-on-handle-drop makes ordinary ownership cleanup change
task policy implicitly. Allowing children to outlive parents requires a separate scope/daemon
contract. Forbidding Fiber values in parent results would require negative type constraints the
language does not otherwise need.

### 9. execute shuts down on every typed terminal path

`execute` allocates fresh run state, prepares task zero through the same completion and task-entry
shape as children, queues it, and repeatedly drains ready nodes. Root success or failure triggers
descendant cancellation before the result is returned or raised. An empty ready queue with a
pending root is `StalledError`; the same cancellation path runs before raising it. Prepared submissions
which have not been adopted are destroyed while their mailboxes are drained.

After typed shutdown, the TaskStore and ready queue are empty, all per-run Shared roots are dropped,
and the `LocalScheduler` value can execute another program. Cancelled Wakes retained elsewhere are
inert and keep only their old execution package/run-state handles alive; they cannot enqueue into a
later run because reuse creates a fresh ready inbox.

### 10. Verification follows the cheapest vertical path

The first proof is one evaluator fixture in which `execute` creates task zero, root forks one child,
root join parks, child completes, and root resumes with the result. The fixture also records that
the child did not activate before fork returned and that shutdown retained no task.

After the vertical proof:

1. extract callback-scoped HashMap mutation and verify it synchronously;
2. generalize TaskId, TaskStore, per-task mailbox, completion, and intrusive ready queue actors;
3. add allocation-ordinal sweeps for root preparation, child preparation, and publication insertion;
4. add evaluator semantic cases for failure, cancellation, yield order, stale readiness, stalled
   shutdown, nested forks, dropped handles, escaped handles, and provider reuse;
5. add the representative programs to the shared native acceptance corpus and run direct Wasm only
   where independent-execution lowering is under test; and
6. publish canonical modules, generated stdlib embedding, language-reference documentation, and
   public doc comments only after the behavior is proven.

This ordering keeps failures attributable: the first slice proves ownership topology, the second
proves reusable storage, and only then does the public API become the migration target.

## Risks / Trade-offs

- **[Provider SPI leaks low-level preparation vocabulary]** → Keep it small, document it as the
  custom-scheduler contract, and make Fiber operations the only tutorial/application surface.
- **[A harmless module import cycle could obscure invalid value recursion]** → Keep every recursive
  ownership edge indirect through Shared or Execution and add the module-cycle program to semantic
  analysis coverage.
- **[HashMap mutable access could leak a value borrow]** → Make access callback-scoped, require a
  unit-returning non-parking callback, return only a presence boolean, and add ownership rejection
  tests for attempted escapes and suspension.
- **[Wake callbacks could conflict with queue access]** → End every queue borrow before drive and
  keep callbacks non-parking, allocation-free, and limited to the ready inbox/node.
- **[Cancellation could become stack- or allocation-dependent]** → Use the intrusive per-entry
  cancellation worklist and test deep task trees without timing assertions.
- **[Retained cancelled Wakes can delay memory release]** → Preserve the existing documented Wake
  contract; prove they are inert and cannot affect a reused scheduler.
- **[The first API has no detach or explicit interrupt]** → Treat those as separate capabilities
  requiring explicit lifetime policy rather than weakening `forkChild` semantics now.

## Migration Plan

1. Add the general HashMap operation and the end-to-end vertical fixture without publishing Fiber
   modules.
2. Replace the fixture's fixed task slots and queue with the reusable source actors.
3. Add the canonical Scheduler, Fiber, and LocalScheduler modules and migrate every in-repository
   caller and fixture in the same change.
4. Delete superseded fixed Deferred/Fiber/scheduler prototype code; retain no compatibility alias or
   root-adapter terminology.
5. Regenerate the standard-library manifest and documentation artifacts, then run the repository's
   full check and release-candidate verification required for package-content changes.

Because the repository is green-field, rollback is a source revert of the complete change rather
than a dual API or migration shim.
