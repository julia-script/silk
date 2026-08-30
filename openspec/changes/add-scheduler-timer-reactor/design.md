## Context

See [proposal.md](proposal.md) for motivation. The current `LocalScheduler` owns homogeneous task
Executions, per-task readiness nodes, a FIFO ready queue, structured parent-child links, and
complete typed shutdown. A parked Execution creates one affine Wake whose readiness callback
appends its preallocated task node. The current scheduler treats an empty ready queue as stalled.

`MonotonicClock` combines reads, resolution, absolute waits, and relative waits in one replaceable
service. `OsMonotonicClock.waitUntil` is the native blocking primitive and `waitFor` derives an
absolute deadline in source. A service provider cannot inherit three operations and override only
one, so scheduler ownership requires a complete task-local clock provider. The compiler's Execution
and Wake substrate already supplies the only privileged lifecycle operation this design needs.

The first event source is time. Future HTTP, WebSocket, process, and other asynchronous operations
will require the same scheduler transition from one dormant Execution generation to one readiness
notification, but their public readiness and error contracts are not yet known.

## Goals / Non-Goals

**Goals:**

- Keep native blocking clock operations at the application edge while making scheduler-owned waits
  suspend only one task.
- Preserve one FIFO ready queue and one source-level owner for every dormant Execution and Wake.
- Make timer registration, completion, cancellation, and shutdown allocation-safe and exact.
- Shape private run state around registrations and event sources so later I/O does not replace task
  suspension, readiness, or stalled detection.
- Preserve deterministic semantics with scripted parent clocks on every engine.

**Non-Goals:**

- A public Reactor, Poller, Event, timeout-race, socket-readiness, HTTP, or WebSocket API.
- Cross-thread wake delivery, preemption, parallel task execution, or background host threads.
- Timer-versus-I/O tie ordering before a concrete I/O source exists.
- Replacing the existing blocking behavior of `OsMonotonicClock` when it is used directly.
- New compiler-known actors, timer intrinsics, ambient providers, targets, or host imports.

## Decisions

### 1. The driver uses the parent clock and tasks see a complete lexical replacement

`LocalScheduler.execute` retains an exclusive parent `MonotonicClock` requirement. Driver code calls
that parent before or between task activations. Root and child Execution bodies instead own two
frame-local clients and lexically provide mutable access to them: the existing Scheduler client and
a new task-clock client implementing the complete `MonotonicClock` service.

The task-clock client and Scheduler client share per-run handles but remain distinct internal
actors. `Scheduler.prepare`, `Fiber.forkChild`, the root wrapper, and the task wrapper accept lazy
work whose child environment contains both service requirements. Every client owns only detached
Shared handles and task identity; the loans are created inside the independently owned Execution
frame, and no parent provider loan crosses `Execution.make`.

This lexical nesting composes. If a scheduler runs inside another scheduled task, the inner driver
sees the outer task-clock client as its parent. An inner idle timer can therefore park the outer
Execution rather than blocking unrelated outer tasks. Nested `execute` remains a synchronous
cooperative scope: only an idle inner scheduler yields outward. If inner work keeps its own ready
queue continuously populated, the inner call does not promise fairness to outer siblings, and
`yieldNow` schedules only among inner tasks.

**Alternatives considered:** Partial service override is not expressible and would make
`waitUntil` remain a blocking escape hatch. Splitting a new Timer service from MonotonicClock would
avoid a complete provider replacement but contradicts the selected API direction and forces a
second task environment capability. Capturing the parent provider in task clients would retain an
exclusive loan across independent Executions and is invalid.

### 2. Reads are cached per turn, while relative waits start from a fresh driver mark

Per-run clock state stores the parent resolution and the latest canonical monotonic components.
`execute` obtains the resolution and initial mark before publishing root readiness. At the start of
each scheduler turn, the driver reads the parent once and updates the cached mark before collecting
due timers. Task-clock `now` reconstructs an `Instant` from that cache without parking;
`getResolution` returns the cached positive value.

A positive `waitFor` request stores its duration and Wake, then parks. Immediately after drive
returns, the driver removes the Wake from the run inbox, makes one fresh call to its parent
provider, and uses the public canonical `MonotonicClock.deadlineAfter` operation also used by the
OS provider. A nested scheduler's parent call may itself observe the outer scheduler's cached mark;
freshness is defined on the provider-call timeline, not as an uncached operating-system read. This
avoids both an artificial scheduling boundary for `now` and an undersleep caused by reusing the
inner turn's earlier cache read. Zero `waitFor` and an already-reached `waitUntil` return before
parking. Overflow retains the clock contract's fatal behavior. The driver takes the Wake out of
Shared storage before the potentially trapping parent call, breaking the mailbox/package cycle at
the trap point; the design does not promise local cleanup after a fatal trap.

**Alternatives considered:** Dispatching every task `now` through the driver would make observation
a yield point and reorder Fibers. Deriving a relative deadline from cached turn time matches one
libuv detail but may count time spent earlier in a long activation; a fresh driver mark preserves
the stronger existing relative-wait contract.

### 3. One task generation is the registration authority

The driver owns one private, preallocated per-run `Shared<TimerRequestInbox>`. Every Scheduler
client receives a clone and passes another clone to each task-clock client. Because the driver is
single-threaded and processes a selected task's request immediately after its activation, one inbox
slot is sufficient and no timer protocol is added to the public `Scheduler.PreparedTask` contract.
The task clock's non-parking `Execution.park` registration callback installs the timer request and
the generation's sole Wake in that inbox without allocating.

Every TaskEntry gains a monotonically advanced wait generation and an `Idle` or `Armed` phase. A
pending inbox request is the pre-arm state; after drive returns, the driver takes it, advances the
generation, creates the private registration identity from TaskId and generation, and transfers the
Wake into `Armed` state. There is no `Registering` TaskEntry phase because the park callback cannot
borrow the TaskStore.

Timer and future event-source indexes store only registration identities. Resolution looks up the
TaskEntry and atomically claims the registration only when its generation still matches. A late or
stale notification therefore cannot wake a completed task or a later parked generation. Generation
exhaustion traps instead of reusing an identity that a future host backend might still report.
Identities and source indexes are per-run in this timer-only backend. Teardown makes all first-run
notifications unreachable before counters may restart. A future host backend MUST either close and
drain its source during teardown or add a run epoch to registration identities.

One parked generation has only one Wake, so a future timeout race will attach several interests to
one registration rather than fabricate independent waits. The first claimed interest will remove
its siblings and consume the Wake. This change implements only one timer interest and publishes no
multi-interest surface.

**Alternatives considered:** A second registration HashMap duplicates the TaskStore and adds
allocation to every park. Storing Wakes in heap nodes complicates cancellation and lets source
containers retain Execution packages. Using TaskId without a generation permits a delayed event to
wake a later wait by the same task.

### 4. A stable private min-heap indexes timer interests

The per-run timer source owns a binary min-heap of entries containing the deadline components,
registration order, registration identity, and no Wake. The comparator orders canonical split
seconds and nanoseconds lexicographically, then registration order. TaskEntry records the active
heap index so cancellation can remove an arbitrary timer in logarithmic time; heap swaps update the
affected entries' indexes in the TaskStore.

Registration order comes from a checked per-run monotonically increasing counter. The scheduler
traps before the counter would wrap or reuse an order value. The heap representation and its
operations remain private functions inside `silk.local_scheduler`; this change does not create a
separately visible TimerQueue actor.

At most one timer is active per task in this slice. Before root publication or child adoption, the
driver ensures heap capacity is at least `TaskStore.length + 1`, reserving only the difference from
the heap's current length. Installing the inbox request, advancing registration state, arming,
claiming, and disarming after the Wake is installed perform no allocation and introduce no typed
failure. If heap append can nevertheless report allocation refusal after capacity was proven, that
impossible result is an invariant trap.

Root setup refusal makes `execute` raise `OutOfMemoryError` after typed cleanup. Child reserve or
TaskStore insertion refusal rejects publication, returns `OutOfMemoryError` to the parent, and lets
the run continue. A successful reserve followed by failed TaskStore insertion may leave unused
capacity, but heap contents and ordering remain unchanged.

**Alternatives considered:** An ordered intrusive list is allocation-free but makes insertion
linear. Lazy heap tombstones retain nodes and can retain Wakes until distant deadlines. A heap of
Shared nodes introduces avoidable reference cycles and extra allocations. A public generic heap is
larger than the timer capability requires; the first implementation remains private to
`silk.local_scheduler` unless a separately justified reusable collection emerges.

### 5. Event collection is a scheduler turn, not an empty-queue afterthought

The driver loop becomes:

1. refresh cached time from the parent;
2. pop and claim every timer due at that mark, consuming Wakes in heap order;
3. take and drive at most one ready task;
4. process the selected task's scheduler request and the one per-run timer inbox; and
5. repeat from time refresh.

Due Wakes invoke the existing fixed readiness endpoint and append to the existing FIFO. Tasks
already ready stay ahead of a newly due timer. Equal timer deadlines enter FIFO in registration
order. Inspecting sources after at most one activation prevents an endlessly replenished ready
queue from starving time.

When no task is ready, the driver examines active registrations. In this timer-only slice, a timer
means calling the parent `waitUntil` for the earliest absolute deadline, then looping to refresh and
recheck. An empty active-registration set means the root is genuinely stalled. The wait is isolated
behind a private event-collection operation so a future poll backend can block for either host
readiness or the duration until the next timer without replacing TaskEntry, registration, or
ready-queue policy.

**Alternatives considered:** Waiting only when the ready queue empties can starve timers under
perpetual yield traffic. Draining all ready tasks before collecting events has the same problem.
Publishing a Poller now would guess at readiness versus completion semantics and error ownership
before any I/O capability exists.

### 6. Cancellation detaches the Wake before destroying the Execution

Normal timer completion first removes the heap entry and changes `Armed` to `Idle`, then consumes
the Wake. Structured cancellation, parent completion, stalled shutdown, and typed scheduler failure
use the inverse path: remove every active source interest and take the Wake without invoking its
readiness endpoint, destroy the dormant Execution and completion authority, then drop the detached
Wake as the final package authority. Task removal asserts no active registration remains.

The iterative descendant-cancellation worklist remains the structured-lifetime mechanism. Its
entry-removal step receives access to the timer source so every removed task is disarmed. Root
shutdown drops the empty timer heap and clock cache with the rest of the per-run state. Retained
readiness from another existing facility remains inert under the current TaskId rules, but this
change deliberately leaves no timer Wake retained outside the scheduler.

**Alternatives considered:** Lazy cancellation by generation alone is safe against false wakeups
but retains heap storage and Wake-owned Execution packages. Dropping a still-linked Wake before
destroying the Execution can release the package while its dormant frame is still needed for
cleanup; detaching first and dropping it last avoids both hazards.

### 7. Only timer-local ordering is specified

The timer source promises deadline order and registration order for ties. The ready queue continues
to promise FIFO in the order readiness endpoints run. This change does not define how a future host
poll batch is ordered, nor whether a timer or I/O interest wins when both are observed in one batch.
Those semantics belong to the first concrete non-timer source, where readiness, completion, typed
errors, and timeout races can be evaluated together.

**Alternatives considered:** Sorting hypothetical I/O events by registration identity would create
an attractive but ungrounded cross-platform promise. Declaring timers globally prior to or after
all future sources would prematurely decide timeout-race semantics.

### 8. Verification starts with scripted time and the cheapest scheduler tier

Evaluator fixtures use a source-level scripted parent clock and one Analysis snapshot per program.
They prove cached reads, zero and past waits, relative deadline arithmetic, sibling progress,
distinct and equal timer order, yield traffic, cancellation, stalling, root outcomes, nested
schedulers, the nested synchronous-scope liveness boundary, registration-order exhaustion, and
provider reuse. The yield-fairness fixture advances the scripted parent on each driver refresh and
requires a due timer to fire before a known later yield while ready work remains. Allocation-ordinal
tests cover root timer preparation, child timer preparation, heap-capacity reservation, and
publication rollback, including more live children than the heap's initial capacity before any
child parks.

Direct Wasm uses only pure source clock providers and proves the same scheduling results without a
clock import. Representative timer programs enter the shared native differential corpus. A small
native acceptance case provides `OsMonotonicClock` outside `execute` and proves that another ready
task progresses before a short future timer, without elapsed-time assertions. Existing clock tests
continue to prove the OS provider's direct blocking ABI separately.

**Alternatives considered:** Per-feature native compiler loops and timing thresholds are expensive
and flaky. Evaluator semantics plus shared differential coverage isolate scheduler policy from the
already-tested native clock boundary.

## Risks / Trade-offs

- **[Every LocalScheduler entry now needs a parent monotonic clock]** → Treat this as an explicit
  green-field breaking change, migrate every caller, and document custom pure parents for portable
  tests and direct Wasm.
- **[A task-local cached mark is not an ambient high-resolution read]** → Specify turn caching and
  use one fresh driver read for each positive relative wait.
- **[A Wake briefly forms a cycle through the run inbox]** → Have the driver take the request
  immediately after drive and never perform another fallible operation while the Wake remains in
  Shared storage.
- **[Heap index bookkeeping can corrupt cancellation]** → Centralize swap/remove operations in the
  private local-scheduler heap functions and test every root, middle, tail, completion, and
  cancellation removal shape.
- **[Polling the parent clock once per turn adds overhead]** → Keep reads source-level and measure
  only in opt-in benches; correctness requires bounded timer observation under ready traffic.
- **[A future I/O backend may require several interests per task]** → Keep one registration per
  parked generation and source indexes keyed by registration identity; extend interests rather
  than replacing the lifecycle.
- **[Parent clock traps while the driver owns a timer Wake]** → Move the Wake out of Shared storage
  before the parent call so the mailbox/package cycle is absent at the trap point; do not claim
  local cleanup after a fatal trap bypasses Drop.

## Migration Plan

1. Add public canonical deadline derivation plus private clock state, per-run registration inbox,
   and local timer-heap functions with synchronous invariant tests before task integration.
2. Expand Scheduler child contracts and task wrappers to bind owned Scheduler and MonotonicClock
   clients; migrate every fixture and call site in the same change.
3. Integrate timer registration, driver time refresh, timer completion, cancellation, and
   timer-aware stalled detection behind the existing `LocalScheduler.execute` boundary.
4. Replace all scheduler documentation and tests that claim an empty ready queue alone is stalled;
   retain the direct OS-provider blocking documentation outside the scheduler replacement.
5. Regenerate standard-library manifests and documentation, run repository verification in the
   required order, and run release-candidate checks for changed package contents.

Rollback is a source revert of the complete change. The repository's green-field policy forbids a
dual scheduler entry, compatibility clock shim, or retained blocking scheduler path.
