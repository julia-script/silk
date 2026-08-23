# SLP-0001: Independently resumable Effect executions

SLP: 0001
Status: Accepted direction
Revision: 31
Author: Julia Ortiz
Created: 2026-08-21
Updated: 2026-08-22
Discussion: —
Review record: [r001](reviews/r001.md), [r002](reviews/r002.md), [r003](reviews/r003.md), [r004](reviews/r004.md), [r005](reviews/r005.md), [r006](reviews/r006.md), [r007](reviews/r007.md), [r008](reviews/r008.md), [r009](reviews/r009.md), [r010](reviews/r010.md), [r011](reviews/r011.md), [r012](reviews/r012.md), [r013](reviews/r013.md)
Review state: Cap — bounded review ended at r013 with revision 30 preserved; author resolution completed at revision 31
Depends on: [values and types](../../docs/language/values-and-types.md), [generics, interfaces, and specialization](../../docs/language/generics-interfaces-and-specialization.md), [unsafe code, intrinsics, and targets](../../docs/language/unsafe-intrinsics-and-targets.md), [runtime and standard-library boundary](../../docs/language/runtime-and-standard-library.md), [Effect suspension](../../docs/language/effect-suspension.md), SLP-0002
Split from: —
Split into: SLP-0002, SLP-0003
Supersedes: —
Superseded by: —
Revisit when: —
Resolution: Author accepted the revision 30 Scheduler-grade capability point after resolving SLP-0002 as Accepted direction. The selected model retains owner-controlled first activation, task-specific push readiness, one recoverable caller-funded package, and the fixed affine lifecycle and cleanup contracts; non-directional compiler, diagnostic, and cross-engine realization questions are delegated to OpenSpec.
OpenSpec handoff: [establish-independent-execution-semantics](../../openspec/changes/establish-independent-execution-semantics/proposal.md), [add-independent-execution-packaging](../../openspec/changes/add-independent-execution-packaging/proposal.md), [add-external-wake-parking](../../openspec/changes/add-external-wake-parking/proposal.md), [add-independent-execution-engine-parity](../../openspec/changes/add-independent-execution-engine-parity/proposal.md), [prove-independent-execution-separation](../../openspec/changes/prove-independent-execution-separation/proposal.md)

## Summary

Silk should complete its existing compiler-private coroutine foundation into an owner-neutral,
affine, independently resumable Effect execution. The compiler already saves live continuation
state for `Effect.suspend`; that operation uses the machinery for a nested policy in which a known
child starts immediately and its completion resumes the direct parent. This proposal adds the
missing powers required by fibers and event waits: detach an unfinished execution from the target
call stack, let an ordinary-source owner drive it until completion or relinquishment, park it behind
a race-free external wake, resume it later in owner-selected non-nested order, and destroy dormant
state with exact cleanup.

The representation remains compiler-private, but the behavior is not implicit. Explicit sealed
operations establish suspension modes, and static reachability selects the cheapest applicable
lowering: direct code, the existing nested/LIFO transfer path, or independently owned execution.
`Fiber`, `Scheduler`, `Deferred`, timers, and a possible future public Coroutine API remain ordinary-
source policies over that substrate. This SLP selects the substrate and its separation invariants,
not those canonical APIs.

The selected sealed surface has two opaque affine, initially non-thread-transferable capabilities,
`Execution<A>` and `Wake`; two compiler-owned static properties, `Intrinsic.Detached` and
`Intrinsic.NonParking`; and five operations. `Detached` lets an ordinary generic wrapper prove that
an erased Effect or retained readiness endpoint owns its complete environment rather than hiding a
call-site-only lifetime check. `NonParking` proves that a runtime-invoked callback cannot recursively
reach external-wake parking, though it may run direct or nested-only Effects.

The operations expose one exact layout for an execution and its lifetime-bound readiness endpoint,
initialize both from one caller-funded package, drive the execution through callback-shaped
completion and suspension outcomes, park it by giving ordinary source one fixed-layout Wake value,
and consume that value to signal readiness. The endpoint contains detached source state plus a
reusable NonParking callback over that state; it is fixed when the Execution is constructed and
reused across every park. Moving the execution through unit-returning `drive` enforces single
activation; one affine branch-state argument lets completion and suspension callbacks share owner
state without allocation.
Ordinary affine drop provides dormant destroy, and existing `Effect.result` lets the standard
library reify typed failure before construction. `park` retains and cleans registration state
internally and returns unit. No compiler-owned step-result type, explicit destroy, per-drive
callback erasure, or scheduler token is required.

`Execution<A>` never erases a lexical loan or a borrowed provider. Ordinary source must eliminate
the body's requirements before construction and move any state that must survive into the closed
Effect. A high-level `Fiber.fork` may borrow its current `Scheduler` while allocating and inserting a
task, but the witness constructs its execution from a closed child Effect and ends the Scheduler
borrow before returning. Automatic provider inheritance, including a cycle-safe owned Scheduler
view for nested Fiber operations, is a later concurrency-library decision. Arbitrary borrowed child
environments are not silently extended by this intrinsic.

The source sufficiency witness depends on SLP-0002 for allocation-backed local shared ownership.
That dependency supplies ordinary-source Deferred state, Fiber result state, and ready inboxes; it
does not make suspension itself allocate through, fail through, or depend on a public Allocator.

Each Execution has compiler/runtime-owned stable wake-control storage. Every park reinitializes it
for one generation shared by the suspended execution and that generation's opaque affine Wake; the
Wake must be consumed or dropped before a later drive can reach another park. Destroying the
execution first marks the current generation cancelled and drops endpoint values `O` and `R`, so a
Wake retained elsewhere remains safe to consume and becomes a no-op. The entire combined Allocation
remains retained as inert cancelled-cell storage until that Wake is consumed or dropped; an
indivisible affine Allocation is never partially released.
Transient runtime ownership keeps the cell alive through registration and notification. On live
readiness, the runtime ends cell mutation, holds the execution in a non-drivable Notifying state,
and invokes the endpoint callback under an invocation retain. Reentrant destruction records
deferred endpoint cleanup; only after the callback returns may the runtime either make the live
execution Eligible or drop its state and release the Allocation. A source registration guard may
unlink that Wake promptly, but source cleanup is never part of the memory-safety proof. Once
task-specific push readiness is selected, the opaque capability is the narrow solution under
[generics, interfaces, and specialization](../../docs/language/generics-interfaces-and-specialization.md)'s exact-representation direction, on which this Candidate's acceptance depends.

Packaging a source-constructed `Execution` and its fixed readiness endpoint uses one explicit
caller-funded `Allocation` obtained through ordinary source policy. Safe wrappers therefore
expose their Allocator and allocation-failure channels, while the compiler recognizes neither actor
by spelling. Once constructed, compiler-managed continuation-stack growth remains fatal like the
existing suspension-frame substrate. Placement, growth, pooling, and later migration remain target
realization choices within that visible procurement boundary. A statically non-parking wrapper
supplies a zero-sized no-op endpoint, so its specialized combined Layout contains only the owned
body/lifecycle package: it activates no wake cell, retains no readiness state, and allocates no
continuation state. Whether a complete program entry is implicitly adapted into an Execution is the
separate SLP-0003 thesis.

The realization must preserve clear semantic seams between continuation construction, execution
ownership and lifecycle, suspension/resumption policy, and ordinary-source scheduling. Adding a new
owner or policy later must not require replacing liveness, frame layout, resume dispatch, or cleanup
machinery. This is an architectural constraint, not a requirement for a stable internal ABI, public
continuation type, or prescribed source-file layout.

## Problem and evidence

The desired program contains multiple unfinished Effects. One waits for a source-owned condition or
system event without occupying its OS thread; another becomes ready and progresses; the first later
continues at its saved point. Ordinary Silk must be able to implement the owner and policy without
making `Fiber`, `Scheduler`, `Deferred`, or `sleep` compiler-known.

Current Silk is already partly coroutine-based. `Effect.suspend` originates a compiler-private
transfer, callers retain post-child state in statically planned frames, and a private iterative
driver resumes them after the known child completes. The compiler derives stable suspension and
resume identities, post-normalization liveness, Copy/borrow/affine frame access, success and failure
cleanup, and one maximum target layout across mutually exclusive states.

What current Silk lacks is not continuation construction. It lacks an independently owned execution
and a non-nested resumption protocol. Native continuation storage is thread-local and strictly LIFO;
the only resume trigger is completion of the direct transferred child. Ordinary source cannot hold
an unfinished execution, observe that driving it relinquished rather than completed, register a
wake without a lost-notification race, resume it later in owner-selected order, or destroy its
dormant compiler-private state.

The first inexpressible operation appears after a waiting actor has checked and registered its
source-owned state:

```silk,ignore
effect fn await<A>(self: &Deferred<A>) -> A {
  if let Some<A> {value} = Deferred.tryTake(self) {
    return move value
  }

  // BLOCKED: ordinary Silk cannot atomically register a wake for this execution,
  // relinquish it without completing the Effect, and later continue here.
  ???
  return run await(self)
}
```

Registration and relinquishment cannot be unrelated operations. A notification may occur between
them and be lost, or a future parallel owner may try to resume the execution before its current
activation has returned. The sealed boundary must make that transition linearizable without
learning what a Deferred, queue, timer, or scheduler is.

The supplied deep-research report supports the same missing-capability diagnosis. It is evidence,
not authority: its proposed Waker shapes must still pass Silk's ownership, target-neutrality, and
minimal-compiler-privilege audits.

## Driving examples: current and desired

### Case: Await a source-owned result while another execution progresses

#### Intent

Start one child that awaits a one-shot result, let another child publish the result, and join them.
One local scheduler provider should make deterministic progress on one OS thread.

#### Current Silk

```silk,ignore
effect fn awaitAnswer(answer: Deferred<i32>) -> i32 {
  return run Deferred.await(answer) // no operation can relinquish and later resume this execution
}
```

`Effect.suspend(Deferred.await(answer))` only transfers to a known nested child. It has no dormant
execution, waiter wake, or opportunity to run the producer.

#### Desired Silk

```silk,ignore
// Illustrative library API; canonical names and lifecycle policy are not selected here.
effect fn program() -> i32
! OutOfMemoryError
? &Scheduler | &mut Allocator {
  let answer = run Deferred.make<i32>()
  let waiterAnswer = Deferred.clone(&answer)
  let producerAnswer = Deferred.clone(&answer)
  let waiter = run Fiber.fork(effect {
    return run Deferred.await(move waiterAnswer)
  })
  let producer = run Fiber.fork(Deferred.succeed(move producerAnswer, 42))

  run Fiber.join(move producer)
  return run Fiber.join(move waiter)
}

effect fn prepareProgram<
  A: Allocator + Intrinsic.Detached,
  O: Intrinsic.Detached,
  R: fn(&O) -> () + Intrinsic.Detached + Intrinsic.NonParking
>(
  scheduler: LocalScheduler,
  programAllocator: A,
  readyState: O,
  onReady: R
) -> Intrinsic.Execution<Result<i32, OutOfMemoryError>>
! OutOfMemoryError
? &mut Allocator {
  let closed = program()
    |> Effect.bindRequirementOwned<Scheduler>(move scheduler)
    |> Effect.bindRequirementOwned<Allocator>(move programAllocator)
  let completed = Effect.result(move closed)
  return run Execution.make(move completed, move readyState, move onReady)
}
```

There is no concurrency-specific run operation. `program()` has one scheduling service requirement
and an explicit allocator requirement at the calls that construct source-owned shared state.
`prepareProgram` closes both with distinct owned providers, reifies typed allocation failure into
ordinary Result data, and uses a separate borrowed Allocator only for the outer combined Execution
package. Its source driver supplies detached readiness state and a reusable callback; that endpoint
is fixed for the outer Execution's lifetime.
The Scheduler service may compose a ready queue, executor, or timer reactor, but it does not hide
ordinary source allocation merely to reduce the visible requirement row.

#### Observable result

An ordinary source test owner drives the Initial
`Execution<Result<i32, OutOfMemoryError>>`. Awaiting relinquishes the waiter execution. The Scheduler
drives the producer, publishes the value, makes the waiter eligible, and later resumes the waiter at
its saved point. This concrete queue order completes the outer drive with `Success(42)` and never
dynamically parks the outer execution, although its static park capability remains safely delimited
by explicit Execution construction. No two executions are active simultaneously on the local
Scheduler.

#### Boundary case

Calling `program` directly from a complete entry remains rejected even after both Scheduler and
Allocator requirements are provided. Its specialized graph can reach external parking through
`Fiber.join -> Scheduler.awaitReady`, but no explicit Execution delimiter owns the root. That
diagnostic is distinct from an unsatisfied service requirement; SLP-0003 decides whether entry
adaptation supplies the missing implicit owner. The compiler creates no global Scheduler and
recognizes none of `program`, `Fiber`, or `Deferred` by spelling.

### Case: Wait for a system timer from an explicitly owned child execution

#### Intent

Let a child wait for a system event without blocking a ready sibling. Sleep depends on Timer rather
than Fiber or Scheduler; its provider prepares the event registration before entering the
NonParking registration callback.

#### Current Silk

Silk has no canonical sleep operation. A target binding may block its OS thread, but current
`Effect.suspend` cannot register a timer, relinquish the unfinished execution to its owner, and
resume after the system event. A blocking sleep in one future local fiber would block every sibling
on that thread.

#### Desired Silk

```silk,ignore
// Illustrative library API. Sleep requires Timer, not Fiber or Scheduler.
effect fn delayedValue() -> i32
! TimerError | OutOfMemoryError
? &Timer {
  run Timer.sleep("1 second")
  return 42
}

effect fn concurrentTimer<P: Timer + Intrinsic.Detached>(ownedTimer: P) -> i32
! TimerError | OutOfMemoryError
? &Scheduler | &mut Allocator {
  let child = delayedValue()
    |> Effect.bindRequirementOwned<Timer>(move ownedTimer)
  let delayed = run Fiber.fork(move child)
  let immediate = run Fiber.fork(effect { return 1 })

  drop run Fiber.join(move immediate)
  return run Fiber.join(move delayed)
}

// The child-owning Scheduler does not own the Effect that performs the joins.
// LocalTimer.make returns one provider for the child and one same-thread
// reactor handle for the explicit outer owner.
struct LocalTimerSetup {
  timer: LocalTimer
  reactor: LocalTimerReactor
}

struct PreparedConcurrentTimer {
  execution: Intrinsic.Execution<Result<i32, TimerError | OutOfMemoryError>>
  reactor: LocalTimerReactor
}

effect fn prepareConcurrentTimer<
  A: Allocator + Intrinsic.Detached,
  O: Intrinsic.Detached,
  R: fn(&O) -> () + Intrinsic.Detached + Intrinsic.NonParking
>(
  setup: LocalTimerSetup,
  scheduler: LocalScheduler,
  childAllocator: A,
  readyState: O,
  onReady: R
) -> PreparedConcurrentTimer
! OutOfMemoryError
? &mut Allocator {
  let LocalTimerSetup {timer, reactor} = move setup
  let outer = concurrentTimer(move timer)
    |> Effect.bindRequirementOwned<Scheduler>(move scheduler)
    |> Effect.bindRequirementOwned<Allocator>(move childAllocator)
  let completed = Effect.result(move outer)
  let execution = run Execution.make(move completed, move readyState, move onReady)
  return PreparedConcurrentTimer {
    execution: move execution,
    reactor: move reactor
  }
}
```

`Timer.sleep` first performs any fallible allocation or system registration preparation, exposing
its declared failure before parking. It then calls `park` with an ordinary NonParking callback
that installs Wake into the prepared registration. If the timer already fired, registration
consumes Wake immediately and the handshake latches readiness; otherwise it stores Wake and returns
an unlink guard. In this initial local model `LocalTimer` and `LocalTimerReactor` own handles to the
same SLP-0002 source state. The explicit driver retains the reactor and polls or fires it on the same
thread; reactor delivery extracts Wake under short source access and consumes it after that access
ends. Moving Wake to a worker-thread callback is rejected because Wake is not yet transferable. The
child receives a concrete owned Timer provider because a detachable Effect cannot retain the
caller's borrowed requirement provider.

#### Observable result

An ordinary source driver owns both fields of `PreparedConcurrentTimer` and retains its own view of
the readiness state passed to `prepareConcurrentTimer`. It drives the Initial execution, retains the
Suspended execution returned through its one affine branch state, and polls the retained reactor.
The reactor consumes the timer Wake on the same thread; readiness propagates from the delayed child
through the Scheduler inbox to the fixed outer endpoint. After that endpoint publishes readiness,
the driver takes the retained execution and drives its Eligible state. It eventually observes
`Success(42)` or ordinary reified Timer/allocation failure data. Wake notification itself performs
no allocation. Unlike `Fiber.join`, this source driver never calls `park`; it owns its wait/poll
turn directly.

The explicit driver's ownership trace is:

| Turn | Driver-owned state | Action |
| --- | --- | --- |
| initial | Initial outer Execution, readiness view, reactor | drive the outer Execution |
| suspended | Suspended outer Execution, readiness view, reactor | retain the Execution and poll or fire the reactor |
| notified | Eligible outer Execution selected through the readiness view, reactor | move the Execution through the next drive |
| complete | `Result<i32, TimerError | OutOfMemoryError>`, reactor | observe the result and drop the reactor |

#### Boundary case

A direct call to `concurrentTimer` at a complete program entry is not enabled by this proposal
alone. Owning the delayed child does not own the parent joiner; when `Scheduler.awaitReady` parks that
parent, no explicit source owner exists to retain and redrive it. SLP-0003 decides whether and how
entry adaptation supplies that implicit owner while preserving ordinary `run` syntax. Likewise, a
timer provider that moves Wake to an OS callback thread is rejected until a parallel proposal makes
Wake transferable and supplies the required atomic semantics.

If the driver instead drops the Suspended outer execution before firing the retained reactor,
destruction cancels the outer Scheduler-wait Wake and cleans the Scheduler and child, which in turn
cancels the child's timer Wake. A later same-thread reactor turn consumes that child timer Wake as a
no-op, publishes no outer readiness, and permits no redrive.

A deliberately blocking provider may remain direct where the target permits it. If closed-world
specialization proves that the selected provider cannot reach external-wake suspension, no
independently resumable representation is required. If a generic provider's pre-specialization
summary still permits that mode, the compiler conservatively preserves the resumable path until each
reachable explicit Execution selects its static target. Pay-for-use is based on static reachability,
not the runtime fiber count.

### Case: Let the ordinary owner choose first activation

#### Intent

Let a source-defined owner enqueue heterogeneous closed Effects and decide when each begins, rather
than forcing every Fiber-like wrapper to run its child immediately to its first park or completion.

#### Current Silk

Exact Effect representations cannot join into one homogeneous task store. Ordinary source can call
each concrete Effect immediately, but then first-activation order is already fixed before the
Scheduler receives ownership.

#### Desired Silk

```silk,ignore
struct StartMarker { started: bool }

fn markStarted(marker: &mut StartMarker) -> () {
  marker.started = true
}

fn readStarted(marker: &StartMarker) -> bool {
  return marker.started
}

effect fn firstActivation() -> bool
! OutOfMemoryError
? &Scheduler | &mut Allocator {
  let marker = run Shared.make(StartMarker { started: false })
  let childMarker = Shared.clone(&marker)
  // TaskOutput is the Scheduler actor's nominal zero-field success type.
  let child = effect {
    let ownedMarker = move childMarker
    Shared.withMut(&ownedMarker, markStarted)
    return TaskOutput {}
  }

  run Scheduler.schedule(move child)

  let before = Shared.with(&marker, readStarted)
  if before { return false }

  drop run Scheduler.driveOne()
  return Shared.with(&marker, readStarted)
}
```

An owner that wants eager start may call `drive` immediately; an owner that wants deferred start may
store the uniform Initial Execution. The intrinsic selects neither scheduling policy.

#### Observable result

`firstActivation` returns `true`. `Scheduler.schedule` procures the combined execution/endpoint
package before observationally publishing `TaskReady {execution}` together with its initial ReadyTask
identity, so `driveOne` can select it.
The body performs no source operation before that first drive. Dropping the Initial Execution
instead cleans its owned body environment and package exactly once without invoking the body or any
drive callback.

#### Boundary case

A primitive that accepts exact `F` only for immediate first activation and manufactures
`Execution<A>` only after parking cannot implement deferred heterogeneous tasks in ordinary source.
It forces eager-to-first-relinquishment semantics into the compiler boundary. This proposal keeps
the Initial state because first-activation ownership and later resumption are two phases of the same
affine execution lifecycle, not because it proposes general callable erasure.

### Case: Preserve the cheaper nested suspension path

#### Intent

Keep stack-safe recursive Effect transfer without acquiring independent ownership, wake state, or
scheduler interleaving.

#### Current Silk

```silk,ignore
effect fn count(value: i32) -> i32 {
  if value == 0 { return 0 }
  return 1 + run Effect.suspend(count(value - 1))
}
```

#### Desired Silk

The source and observable semantics remain unchanged.

#### Observable result

The program uses bounded native and Wasm machine stack. A known child starts immediately, its
completion is the only trigger that resumes its parent, and logical order remains nested. The
executable gains no public execution handle, wake registration, scheduler, or atomic state.

#### Boundary case

Replacing `Effect.suspend` with external-wake parking is not semantics-preserving. Parking may leave
no immediate successor and may allow an unrelated eligible execution to run first.

### Case: Preserve reuse by a future explicit coroutine owner

#### Intent

Ensure the compiler substrate is not secretly Scheduler-shaped. A later proposal should be able to
define an explicit asymmetric Coroutine by using the same owner-neutral execution lifecycle, without
replacing frame and resume machinery.

#### Current Silk

Ordinary source cannot hold and step an unfinished Effect execution, so it cannot implement a
transparent Coroutine over arbitrary Effect code. It can only build a manual state machine whose
pending state infects every participating operation.

#### Desired Silk

```silk,ignore
// Future pressure example only; this SLP does not select this API or its yield typing.
effect fn coroutineExample() -> i32
! OutOfMemoryError
? &mut Allocator {
  let port = run CoroutinePort.make<i32>()
  let body = effect {
    let ownedPort = move port
    run Coroutine.yield(&ownedPort, 1)
    run Coroutine.yield(&ownedPort, 2)
    return 3
  }
  let mut coroutine = run Coroutine.make(move body)

  let first = run Coroutine.resume(&mut coroutine)
  let second = run Coroutine.resume(&mut coroutine)
  let third = run Coroutine.resume(&mut coroutine)
  return Coroutine.verifySequence(move first, move second, move third)
}
```

A future source wrapper could allocate a local shared port through SLP-0002, move one handle into the
closed execution, retain the other beside the execution, and have `resume` drive it again. Its
construction therefore exposes ordinary Allocator and allocation-failure channels unless it is
given reusable source storage. Typed yielded values need not be transported by the compiler control
protocol, but their shared source channel is not free or hidden.

#### Observable result

Adding such an owner may require a later public API and rules for yielded borrows or resume inputs,
but it reuses execution construction, stepping, frame ownership, resume dispatch, and destruction.
It does not require a second coroutine implementation.

The wrapper cannot directly redrive the Dormant execution returned after each yield. `yield` stores
both its source payload and the Wake supplied by `park` in the shared port. Each later `resume`
consumes that Wake, lets the Execution's fixed endpoint mark its port ready, observes the execution
as Eligible, and only then drives it again:

```text
resume #1 -> drive Initial -> yield stores { Yielded(1), Wake } -> retain Dormant Execution
resume #2 -> consume Wake -> publish while Notifying -> callback return marks Eligible -> drive -> store { Yielded(2), Wake }
resume #3 -> consume Wake -> publish while Notifying -> callback return marks Eligible -> drive -> completion returns Done(3)
drop while yielded -> destroy Execution -> retained Wake becomes a consuming no-op -> clean port
```

#### Boundary case

This SLP does not promise general continuations, multi-shot resume, cloning, arbitrary frame entry,
symmetric transfer, yielded references, or cross-thread resume. Those features may require separate
language decisions. The reuse constraint is satisfied when an ordinary source owner can drive the
same affine execution substrate without Scheduler-specific compiler behavior.

## Goals and non-goals

### Goals

- Complete the existing compiler-private coroutine foundation into owner-neutral, independently
  resumable Effect executions.
- Make a minimal source-defined local Fiber runtime for closed leaf tasks possible without making
  Fiber compiler-known; cycle-safe nested/provider-inheriting fibers remain dependent concurrency
  policy.
- Preserve one `Effect<A ! E ? R>` programmer model; ordinary combinators do not expose Pending.
- Establish race-free register-before-suspend and wake-before-dormant behavior with a storable,
  affine readiness capability.
- Give each independent execution stable continuation storage and exact resume and destroy paths.
- Make source-created execution and its lifetime-bound readiness endpoint caller-funded through
  exact Layout and Allocation, while later compiler continuation-stack growth remains fatal like
  existing suspension-frame exhaustion.
- Make generic detachment an explicit compiler-owned static bound, so safe wrappers prove their
  environment contract once rather than relying on hidden specialization-time rejection.
- Make execution construction a control delimiter: external-wake capability propagates within the
  owned execution, not through the ordinary source owner driving it.
- Separate continuation mechanics, execution lifecycle, suspension policy, and source scheduling so
  later owners or policies can reuse the core.
- Preserve the cheaper existing nested/LIFO lowering where external-wake suspension is unreachable.
- Preserve direct code and absence of suspension runtime where every suspension mode is unreachable.
- Demonstrate sufficiency with non-normative Scheduler, Fiber, Deferred, timer, and future-coroutine
  pressure cases.

### Non-goals

- Select canonical Fiber, Scheduler, Deferred, Coroutine, scope, join, cancellation, racing, timer,
  or Stream APIs.
- Define structured-concurrency child lifetimes, fairness, shutdown, or detached-task policy.
- Expose a public continuation, general coroutine frame, resume label, or universal Pending result.
- Make every Effect heap-allocated, independently owned, or scheduler-driven.
- Require `Effect.suspend` to pay external wake, independent ownership, or atomic costs.
- Put ready queues, work stealing, priorities, timers, I/O reactors, or host event loops in the
  compiler.
- Define preemptive scheduling, a stable runtime ABI, or one target-specific representation.
- Define implicit ownership, waiting, polling, or outcome delivery for a complete program entry;
  SLP-0003 owns that separate adapter thesis.
- Define the parallel memory model, transfer/share derivation, atomics, OS threads, or work stealing.
- Define advanced public coroutine typing such as yielded borrows, resume inputs, symmetric transfer,
  multi-shot continuations, or cross-thread resumption.
- Expose a configurable growth provider for compiler-private continuation-stack segments after
  source-funded construction.

## Current language model

An Effect is a lazy computation value; `run` executes one layer to success or typed failure.
`Effect.suspend` wraps `Intrinsic.suspendEffect`, which originates a nested stack-safe child transfer.
Concrete suspendability propagates through specialized calls and selected providers. Callers relay a
transfer to one private iterative driver while retaining exact post-child state in compiler-planned
frames.

The current mechanism already contains the essential coroutine ingredients:

- stable suspension and resume identities;
- specialized may-suspend reachability;
- post-normalization live-local discovery;
- Copy, borrowed-dependency, and affine-transfer frame access;
- success and typed-failure restoration and cleanup;
- one maximum target layout across mutually exclusive states; and
- evaluator, native, and Wasm transfer/resume paths.

Its policy and storage are narrower than a general independently resumable execution. Transfers are
nested, the successor is the known child, resumption follows direct-child completion, and native
frame storage is thread-local and LIFO. There is no source-ownable execution value, external wake,
dormant destroy entry, or scheduler-order resumption.

### How far current Silk can go

Ordinary Effects can be retained and selected, but one `run` completes before its caller continues.
They are lazy values, not independently runnable executions. `Effect.suspend` removes covered
recursive cycles from the target call stack, but it does not let another ready execution progress.

Ordinary source can model concurrency manually by returning `Ready<A> | Pending<State>` and storing
every live value itself. That proves queues and scheduling policy need no compiler privilege, but it
is a different API: every combinator must propagate Pending and arbitrary existing Effects cannot be
paused transparently.

The exact wall is therefore not “make a coroutine.” Current Silk already does that privately. The
wall is “detach and own the coroutine independently, return control without completion, and resume
it through a non-nested policy.”

## Proposed language model

The proposal names the compiler-private substrate an **independently resumable Effect execution**.
It is affine: exactly one owner may drive or destroy it, it cannot be cloned or re-entered, and at
most one activation may run at a time. The owner is explicit ordinary source, such as a
source-defined Scheduler or future Coroutine wrapper. Whether the compiler/target implicitly
creates an equivalent owner for a complete entry Effect is delegated to SLP-0003. The compiler
recognizes no source owner by library name.

```text
Effect source
    |
    v
compiler-private continuation foundation
    |  live state, frame layout, resume labels, cleanup
    |
    +-- nested execution stack
    |     `-- direct-child completion resumes parent      (Effect.suspend)
    |
    `-- independently owned execution
          +-- source-owned park policy                    (possible yield)
          `-- external wake makes execution eligible      (park)
                    |
                    v
          ordinary-source owner and policy
          Scheduler / Fiber / future Coroutine
```

An owner drives one execution until it produces a typed completion or relinquishes control. A
relinquished execution remains unfinished; ordinary Effect combinators inside it do not observe a
Pending value. A park additionally installs an external wake protocol and becomes dormant. Waking
does not directly execute the continuation or transport its `A`; it only makes that execution
eligible for a legal future drive. Source-owned Deferred, timer, or queue state owns any payload.

“Owner-selected order” does not mean arbitrary frame execution. The owner chooses among eligible
executions. Each selected execution resumes only at its one saved continuation and then follows
ordinary program control. Concurrent resume, resume while running, and resume after completion or
destruction are invalid.

### Static representation selection

The compiler selects representation from exact specialized reachability of sealed suspension
operations, not from Effect syntax, runtime fiber count, or library declaration spelling:

```text
ordinary run + no suspension       -> direct function and Effect lowering
ordinary run + nested suspension   -> current cheaper LIFO suspension lowering
explicit Execution                 -> owned erased body, exact lifecycle metadata,
                                      and one caller-funded combined package
explicit Execution + park          -> combined package also contains the fixed endpoint,
                                      wake cell, and independently owned dormant continuation
```

Before complete specialization, an unresolved call summary is conservatively assumed to include
every suspension mode still permitted by its static contract. Each reachable complete application
then has one statically selected hidden implementation and suspension mode under [generics, interfaces, and specialization](../../docs/language/generics-interfaces-and-specialization.md); no
runtime dispatch survives merely to choose among direct, nested, and independently owned lowering.
Later `drive` still invokes the exact erased body and endpoint through purpose-bound hidden metadata.
Execution construction is a propagation delimiter. Functions inside the closed, owned Effect may
require that lowering; the ordinary source Scheduler or Coroutine code calling the sealed drive
boundary does not become parkable merely because the owned execution can park.

Suspension reachability and explicit execution ownership are separate axes. Constructing an
`Execution` from a non-suspending body still creates an owned, never-driven-droppable executable
container and crosses the selected sealed representation-erasure boundary. It needs exact hidden
invoke and cleanup metadata even though it needs no continuation or wake state:

| Ownership | Reachable suspension | Minimum machinery |
| --- | --- | --- |
| ordinary `run` | none | direct lowering |
| ordinary `run` | nested only | existing LIFO frames and private nested driver |
| explicit `Execution` | none | one combined Allocation containing the owned erased body and exact invoke/drop metadata; a zero-sized no-op endpoint adds no readiness storage |
| explicit `Execution` | nested only | the same combined package plus LIFO frames during a drive |
| explicit `Execution` | external park | combined package containing the fixed endpoint and wake-control state plus independently owned dormant continuation |

Whole-program optimization may remove an execution wrapper when it proves the owner and lifecycle
unobservable, but that is not the semantic cost promise.

### Execution storage and exhaustion

The direct path allocates no continuation storage. The nested path keeps using the existing
compiler/runtime suspension-frame substrate. An independently resumable execution extends that
substrate with storage whose lifetime belongs to the affine `Execution` rather than to one
thread-local LIFO drive. That storage must remain valid between drive calls and while the execution
is dormant.

One target-specific Layout query exposes the complete construction package required by exact `F`,
`O`, and `R`.
The combined layout covers the opaque owner record, body environment, invoke/drop metadata, fixed
endpoint, stable wake-control storage when external parking is reachable, and any initial continuation
segment optionally required by `F`'s static suspension summary. Ordinary source obtains one
Allocation through its selected allocator or pool before the initializing intrinsic. A safe wrapper
may pool or reuse compatible Allocations, but may not hide procurement or failure.

The Execution owns its combined Allocation and endpoint for its complete lifetime and reuses the
endpoint across every park. On successful readiness, the runtime ends cell mutation, holds the
execution Notifying, and invokes `R(&O)` under a transient invocation retain. Reentrant Execution
destruction records DestroyPending rather than dropping actively borrowed endpoint state; after
callback return the runtime either makes the still-live execution Eligible or drops `O` and `R` and
releases the combined Allocation. Dormant destruction drops every live body, frame, and endpoint
value but retains the indivisible Allocation as inert cancelled-cell storage until the last external
Wake is consumed or dropped. A forgotten Wake therefore retains more bytes than the former
two-package design; Revision 27 accepts that cost to remove a sealed operation, one allocation, and
one failure branch. The
endpoint does not receive a fresh affine payload per drive and cannot be retargeted during the
Execution lifetime; owners needing indirection keep it in ordinary shared source state.

After caller-funded construction, recursive or otherwise dynamic continuation-stack growth uses the
compiler/runtime execution-stack policy and remains fatal on exhaustion, matching [Effect suspension](../../docs/language/effect-suspension.md) and
Silk's no-unwind rule. Thus a bounded owner can recover if it cannot package a new Execution and
endpoint before insertion, while an already running execution may still terminate if its
logical stack later exceeds target/runtime capacity. Ordinary task identity, result, queue,
Deferred, coroutine payload, and combined execution-package allocations are all visible at
their construction or growth boundary.

This proposal selects no non-source owner and grants no source-created child a special storage
exception. SLP-0003 separately asks whether executable entry adaptation implicitly constructs and
owns a root Execution using target-provisioned resources.

### Required realization seams

The proposal requires semantic separation at four layers:

1. **Continuation foundation** — suspension identities, live-state capture, frame slots, resume
   dispatch, outcome restoration, and exact cleanup. It knows neither Scheduler nor wake policy.
2. **Execution ownership and lifecycle** — affine construction, storage ownership, drive state,
   completion, dormant destruction, stable wake-control lifetime, and prevention of re-entry or
   concurrent resume. It is owner-neutral. Each execution also owns one evaluator logical-stack
   context rooted at its first drive and preserved across later drives and parking.
3. **Suspension policies** — nested child transfer and external-wake parking are distinct target-
   neutral control modes over the foundation. A cooperative library yield may use parking by
   retaining and later consuming its affine Wake. Only parking owns registration and
   dormant eligibility.
4. **Ordinary-source policy** — ready queues, fairness, fibers, Deferred, timers, result storage,
   cancellation conventions, and future Coroutine wrappers remain outside the compiler.

These are semantic seams, not mandatory packages, files, public interfaces, or allocation
strategies. Backends may specialize or fuse layers when behavior and pay-for-use remain provable.
However, no realization is conforming if adding another ordinary-source owner or suspension policy
requires replacing the continuation foundation, or if stack-only suspension inherits independent-
execution costs solely because the shared foundation exists.

## Worked language experience

### Detailed ordinary-source implementation shape

The following shapes are illustrative, not a canonical concurrency API. They deliberately expose
enough state to show where every ownership and wake transition lives.

An ordinary safe wrapper funds one combined sealed package explicitly and fixes one source-owned readiness
endpoint for the Execution lifetime:

```silk,ignore
// execution.silk — declarations are unqualified inside their actor module.
pub effect fn make<
  A,
  F: once Effect<A> + Intrinsic.Detached,
  O: Intrinsic.Detached,
  R: fn(&O) -> () + Intrinsic.Detached + Intrinsic.NonParking
>(
  body: F,
  readyState: O,
  onReady: R
) -> Intrinsic.Execution<A>
! OutOfMemoryError
? &mut Allocator {
  let layout = Intrinsic.executionLayout<A, F, O, R>()
  let allocation = run Allocator.allocate(layout)
  unsafe {
    return Intrinsic.executionFromAllocation<A, F, O, R>(
      move allocation,
      move body,
      move readyState,
      move onReady
    )
  }
}
```

`Intrinsic.Detached` and `Intrinsic.NonParking` are compiler-owned static properties, not runtime
interfaces or source actors. The generic body can therefore prove the initializer's environment and
callback-control preconditions once. A concrete call whose exact `F` captures a caller loan, or whose
exact `R` can reach external parking, fails the corresponding bound at `Execution.make`;
specialization does not add a hidden rejection.

The Scheduler is a service key implemented by a distinct ordinary provider. This direct local
provider separates the affine task store from the shared ready inbox:

```silk,ignore
// scheduler.silk
pub struct TaskOutput {}

service Scheduler {
  effect fn schedule<F: once Effect<TaskOutput> + Intrinsic.Detached>(
    body: F
  ) -> ()
  ! OutOfMemoryError
  ? &Scheduler | &mut Allocator

  effect fn driveOne() -> bool
  ? &Scheduler

  effect fn awaitReady() -> ()
  ! OutOfMemoryError
  ? &Scheduler | &mut Allocator
}

// local_scheduler.silk imports Scheduler and TaskOutput from scheduler.silk.
struct LocalScheduler {
  tasks: Shared<TaskStore>
  ready: Shared<ReadyInbox>
}

pub effect fn make() -> LocalScheduler
! OutOfMemoryError
? &mut Allocator {
  return LocalScheduler {
    tasks: run Shared.make(TaskStore.empty()),
    ready: run Shared.make(ReadyInbox.empty())
  }
}

fn signalReady(
  endpoint: &ReadyEndpoint
) -> () {
  ReadyInbox.signal(&endpoint.ready, endpoint.id)
}

struct ReadyEndpoint {
  ready: Shared<ReadyInbox>
  id: TaskId
}

// Reserves a task record and reusable ReadyInbox slot before any task is published.
effect fn reserveTask(self: &LocalScheduler) -> TaskReservation
! OutOfMemoryError
? &mut Allocator

struct TaskReservation {
  id: TaskId
  taskSlot: ReservedTaskSlot
  readySlot: ReservedReadySlot
}

struct TaskReady { execution: Intrinsic.Execution<TaskOutput> }
struct TaskRunning {}
struct TaskParked { execution: Intrinsic.Execution<TaskOutput> }
struct TaskCompleted {}

struct TaskState {
  value: TaskReady | TaskRunning | TaskParked | TaskCompleted
}

struct DriveBranch {
  tasks: Shared<TaskStore>
  ready: Shared<ReadyInbox>
  id: TaskId
}

fn completeDriven(
  branch: DriveBranch,
  value: TaskOutput
) -> () {
  TaskStore.complete(&branch.tasks, branch.id)
  ReadyInbox.release(&branch.ready, branch.id)
}

fn retainSuspended(
  branch: DriveBranch,
  execution: Intrinsic.Execution<TaskOutput>
) -> () {
  TaskStore.park(&branch.tasks, branch.id, move execution)
}

effect fn schedule<
  F: once Effect<TaskOutput> + Intrinsic.Detached
>(
  self: &LocalScheduler,
  body: F
) -> ()
! OutOfMemoryError
? &mut Allocator {
  let reservation = run LocalScheduler.reserveTask(self)
  let endpoint = ReadyEndpoint {
    ready: Shared.clone(&self.ready),
    id: reservation.id
  }
  let execution = run Execution.make(
    move body,
    move endpoint,
    signalReady
  )
  LocalScheduler.enqueueNew(self, move reservation, move execution)
}

impl Scheduler for LocalScheduler {
  schedule: LocalScheduler.schedule
  driveOne: LocalScheduler.driveOne
  awaitReady: LocalScheduler.awaitReady
}
```

`TaskReservation` is an uncommitted affine rollback guard, not a published task. All fallible task
and ready-slot capacity growth completes before it is returned. While uncommitted it owns both
reserved slots. Dropping it removes the reservations. `LocalScheduler.schedule` then constructs the
fixed `ReadyEndpoint` and funds the combined Execution package before `enqueueNew` publishes
`TaskReady {execution}` plus the initial ReadyTask identity and disarms the rollback guard in one
non-suspending, non-observable commit sequence. This local witness requires no multi-cell atomic
primitive. Failure
in the combined package allocation drops the reservation and inserts no task.

An execution's retained readiness endpoint owns only a clone of `ready` plus its source-owned
`TaskId`; the reusable callback borrows that detached endpoint state on each notification. It never
owns the task store that owns the execution. The witness also accepts a
closed child Effect rather than implicitly inheriting the Scheduler provider. This avoids a strong
cycle from `TaskStore -> Execution -> Scheduler -> TaskStore` and leaves provider inheritance to the
canonical concurrency-library proposal.

`LocalScheduler.driveOne` removes one execution from the task store before driving it. It holds no `Shared` access
across the call:

```silk,ignore
struct DriveStop {}
struct DriveRetry {}
struct DriveSelected {
  execution: Intrinsic.Execution<TaskOutput>
  id: TaskId
}

fn staleSelection(self: &LocalScheduler, id: TaskId) -> DriveRetry {
  ReadyInbox.releaseStale(&self.ready, id)
  return DriveRetry {}
}

fn selectTask(
  self: &LocalScheduler,
  readyTask: ReadyTask
) -> DriveRetry | DriveSelected {
  let id = readyTask.id
  return match TaskStore.takeReady(&self.tasks, id) {
    None {} => staleSelection(self, id)
    Some<Intrinsic.Execution<TaskOutput>> {value} => DriveSelected {
      execution: move value,
      id: id
    }
  }
}

fn selectRunnable(
  self: &LocalScheduler
) -> DriveStop | DriveRetry | DriveSelected {
  return match ReadyInbox.take(&self.ready) {
    None {} => DriveStop {}
    Some<ReadyTask> {value} => selectTask(self, move value)
  }
}

effect fn driveSelected(
  self: &LocalScheduler,
  selected: DriveSelected
) -> bool {
  let DriveSelected {execution, id} = move selected
  let branch = DriveBranch {
    tasks: Shared.clone(&self.tasks),
    ready: Shared.clone(&self.ready),
    id: id
  }
  run Intrinsic.drive(
    move execution,
    move branch,
    completeDriven,
    retainSuspended
  )
  return true
}

effect fn finishDrive(
  self: &LocalScheduler,
  selected: DriveStop | DriveRetry | DriveSelected
) -> bool {
  return match move selected {
    DriveStop stopped => false
    DriveRetry retry => false
    DriveSelected chosen => run driveSelected(self, move chosen)
  }
}

pub effect fn driveOne(self: &LocalScheduler) -> bool {
  loop {
    let selected = selectRunnable(self)
    if let DriveStop stopped = &selected { return false }
    if let DriveRetry retry = &selected { continue }
    return run finishDrive(self, move selected)
  }
}
```

`TaskStore.takeReady` is called only for an identity dequeued from `ReadyInbox`; it accepts
`TaskReady` or `TaskParked`, changes the source state to `TaskRunning`, and returns the execution.
`park` and `complete` use short non-suspending `Shared.withMut` callbacks. Source `TaskParked` may
hold either a semantic Dormant or Eligible execution; the dequeued identity is the eligibility
evidence, not the source union tag.
The one affine `DriveBranch` moves to exactly one outcome callback, so this shape does not require
duplicating an owner lease or allocating shared branch state merely to cover both results.
`ReadyInbox.signal` likewise changes inbox state under access, extracts the optional Wake for the
single explicit driver waiting for the inbox to become nonempty, releases access, and consumes that
Wake afterward. This witness admits at most one inbox waiter, so publishing one task identity and
notifying its one driver are O(1); arbitrary work inside another source-defined endpoint is not an
intrinsic complexity guarantee. No external owner callback runs while a `Shared` cell is accessed.

A typed Fiber stores its outcome in source-owned state while the Scheduler stores only homogeneous
`TaskOutput` executions. The nominal zero-field result avoids the current parser ambiguity around a
unit type nested directly inside an exact Effect bound:

```silk,ignore
struct Fiber<A, E> {
  result: Deferred<Result<A, E>>
}

// fiber.silk — declarations are unqualified inside the actor module.
pub effect fn fork<A, E, F: once Effect<A ! E> + Intrinsic.Detached>(
  body: F
) -> Fiber<A, E>
! OutOfMemoryError
? &Scheduler | &mut Allocator {
  let result = run Deferred.make<Result<A, E>>()
  let publish = Deferred.clone(&result)

  let child = effect {
    let outcome = run Effect.result(move body)
    run Deferred.succeed(move publish, move outcome)
    return TaskOutput {}
  }

  run Scheduler.schedule(move child)
  return Fiber<A, E> { result: move result }
}
```

The exact `F` binder preserves the child's concrete Effect representation until the sealed
`executionFromAllocation` boundary inside the selected Scheduler provider. The Scheduler requirement
belongs to `Fiber.fork`, not to the child body: the borrowed provider constructs and enqueues the
finished `Execution<TaskOutput>` but is never captured by it. The caller must close the child's requirements
with owned values before forking. A canonical Fiber API may later offer scoped inheritance or
library-defined ways to snapshot selected services, but the intrinsic does not generically extend
their loans.

`Deferred.make` allocates the joinable result cell. `Scheduler.schedule` grows TaskStore and
ReadyInbox as needed, registers one reusable ready slot for the new `TaskId`, and allocates the exact
combined Execution/endpoint package before publication. The task's Execution owns that package. The
inbox slot owns only identity and queue links, never an Execution Allocation. Task wake and repeated
suspension therefore do not allocate after scheduling.

If an enqueued execution is destroyed, the task record is removed and the queued slot becomes a
tombstone retained by ReadyInbox until dequeue. Thus a stale identity never traverses freed storage,
and task completion releases an idle slot. A queued slot is released when its tombstone is consumed.
The provider's internal `enqueueNew` consumes an already registered reservation and therefore
neither allocates nor introduces another failure row.

A resultless operation does not construct result state:

```silk,ignore
// Illustrative policy: no handle and no implicit typed-error disposal.
pub effect fn spawn<A, F: once Effect<A> + Intrinsic.Detached>(
  body: F
) -> ()
! OutOfMemoryError
? &Scheduler | &mut Allocator {
  let child = effect {
    drop run body
    return TaskOutput {}
  }
  run Scheduler.schedule(move child)
}
```

This spawn accepts an infallible Effect of any success type and explicitly drops that value because
it returns no handle through which to observe a result or typed failure. Supervision, explicit failure disposal, and daemon lifetime are canonical
concurrency-policy questions. `spawn` only enqueues; it does not start the child inline. If no owner
later drives the Scheduler, shutdown destroys the never-driven execution and cleans its body. If
TaskStore already owns compatible reusable task and execution-package reservations, a corresponding
source operation can reuse that storage. Otherwise one opaque package requires one caller-funded
Allocation. No result allocation is inherent in this witness's resultless spawn.

Every source-policy allocation used by this illustrative `LocalScheduler.make`, `fork`, `spawn`, join,
or `Deferred.make` exposes Allocator and failure channels at the operation that constructs or grows
storage. The combined Execution package uses the same ordinary Allocation path through its exact
intrinsic Layout query. Later compiler continuation-stack growth is a distinct fatal resource.

`Fiber.join` checks its source-owned result, drives other ready tasks, and parks only when no task can
make immediate progress:

```silk,ignore
effect fn raise<E>(error: E) -> never ! E {
  fail move error
}

effect fn resolveResult<A, E>(result: Result<A, E>) -> A ! E {
  return match move result {
    Result<A, E> {value: outcome} => match move outcome {
      Success<A> {value} => move value
      Failure<E> {error} => run raise(move error)
    }
  }
}

pub effect fn join<A, E>(fiber: Fiber<A, E>) -> A
! E | OutOfMemoryError
? &Scheduler | &mut Allocator {
  loop {
    let available = Deferred.tryTake(&fiber.result)
    if let Some<Result<A, E>> {value} = move available {
      return run resolveResult(move value)
    }

    if run Scheduler.driveOne() {
      continue
    }

    run Scheduler.awaitReady()
  }
}
```

`Scheduler.awaitReady` is another ordinary park wrapper. Its registration callback atomically
observes or waits for a nonempty inbox and returns an unlink guard retained privately as `G`:

```silk,ignore
fn registerReadyWaiter(
  wake: Intrinsic.Wake,
  ready: Shared<ReadyInbox>,
  waiter: ReadyWaiter
) -> WaiterGuard {
  return ReadyInbox.register(move ready, move waiter, move wake)
}

pub effect fn awaitReady(self: &LocalScheduler) -> ()
! OutOfMemoryError
? &mut Allocator {
  let ready = Shared.clone(&self.ready)
  let waiter = run ReadyWaiter.make()
  let register = registerReadyWaiter(move ready, move waiter)
  run Intrinsic.park(move register)
  return ()
}
```

Each call allocates one ordinary waiter node. `ReadyInbox.register` consumes the preallocated node
and never allocates while installing Wake; its returned guard releases or unlinks that node. A
canonical Scheduler may instead pre-reserve waiter capacity, but this witness keeps the allocation
and failure visible.

This minimal Shared-only provider deliberately does not support a detached child joining a task
owned by the same Scheduler. Moving `LocalScheduler.clone` into that child would create
`TaskStore -> Execution -> LocalScheduler -> TaskStore`, a strong cycle that SLP-0002 neither breaks
nor collects. Leaving the requirements open is rejected at `Fiber.fork`:

```silk,ignore
let nested = Fiber.join(move inner)
let outer = run Fiber.fork(move nested)
// Rejected: nested still requires Scheduler and Allocator, so its representation
// does not satisfy the closed Intrinsic.Detached contract required by fork.
```

A dependent concurrency-library proposal may add a cycle-safe child-visible scheduling capability,
Weak ownership, scoped children, or a different join architecture. Until then, nested join is a
boundary of this sufficiency witness rather than evidence for hidden provider inheritance.

Parking returns the current explicit execution to the Scheduler invocation that drove it. If no
explicit owner exists—for example, at a complete program entry—this proposal alone supplies no
parking lifecycle; that is the SLP-0003 boundary.

The one-consumer Deferred witness has ordinary source state:

```silk,ignore
struct DeferredPending<A> { waiter: Option<Intrinsic.Wake> }
struct DeferredDone<A> { value: A }
struct DeferredTaken {}

struct DeferredState<A> {
  value: DeferredPending<A> | DeferredDone<A> | DeferredTaken
}

struct Deferred<A> {
  state: Shared<DeferredState<A>>
}
```

`Deferred.await` consumes its designated consumer handle. It first tries to take the value. If still
pending, it parks with a registration callback that installs the affine Wake under a short
`Shared.withMut` access. If completion won the race, registration returns the wake as an action and
invokes it only after access is released. The returned `WaiterGuard` unlinks a still-registered wake
when resumed or when the dormant execution is dropped.

```silk,ignore
// deferred.silk — declarations are unqualified inside the actor module.
fn installWake<A>(
  inner: &mut DeferredState<A>,
  wake: Intrinsic.Wake
) -> DeferredRegistration {
  return DeferredState.register(inner, move wake)
}

fn wakeAlreadyDone(wake: Intrinsic.Wake) -> WaiterGuard {
  Intrinsic.wake(move wake)
  return WaiterGuard.empty()
}

fn registerWaiter<A>(
  wake: Intrinsic.Wake,
  state: Shared<DeferredState<A>>
) -> WaiterGuard {
  let install = installWake<A>(move wake)
  let registration = Shared.withMut(&state, move install)
  return match move registration {
    Linked {id} => WaiterGuard.make(Shared.clone(&state), id)
    AlreadyDone {wake} => wakeAlreadyDone(move wake)
  }
}

pub effect fn await<A>(self: Deferred<A>) -> A {
  loop {
    let available = Deferred.tryTake(&self)
    if let Some<A> {value} = move available {
      return move value
    }

    let state = Shared.clone(&self.state)
    let register = registerWaiter<A>(move state)
    run Intrinsic.park(move register)
  }
}
```

`Deferred.succeed` moves the value into state exactly once, extracts the registered Wake under
access, releases access, and consumes it with `Intrinsic.wake` afterward. Double completion, a second consumer, list
capacity, and allocation policy are ordinary Deferred design choices; this witness needs only one
producer and consumer to prove the compiler boundary.

Together these actors implement the driving program: `fork` creates two `TaskOutput` executions, `join`
drives the waiter until it parks, drives the producer to completion, the producer wakes the waiter,
and a later `driveOne` resumes the waiter. The compiler recognizes only the selected execution and
shared-core intrinsic families across SLP-0001 and SLP-0002, never Scheduler, Fiber, Deferred, a
queue, or a result actor.

### Wake ordering and dormant destruction

The parking protocol has these observable transitions:

| Event order | Required result |
| --- | --- |
| `Intrinsic.wake` called inside `register` | Readiness is latched; the complete `onSuspend` callback returns after storing the execution; only then may the fixed endpoint callback run for that park; the execution still relinquishes once. |
| Wake consumed inside `register`, then `onSuspend` drops the execution | Destruction cancels the latched wake before the callback returns; the endpoint is dropped and never invoked, and the retained continuation is cleaned exactly once. |
| Wake consumed after `register` and after dormancy | The dormant execution becomes Notifying once, its fixed endpoint runs once for that park, and callback return makes it Eligible; it resumes only through a later owner-selected `drive`. |
| safe source attempts to signal the same readiness twice | Rejected because `Intrinsic.wake` consumes affine `Wake`; no second eligibility transition exists. |
| execution dropped before readiness | The wake-control cell is cancelled first, endpoint values and continuation values are cleaned exactly once, and dropping `G` may unlink and release the external Wake. A Wake that remains elsewhere is a safe consuming no-op and keeps the complete inert combined Allocation retained until it is consumed or dropped. |
| Wake consumed, then eligible execution dropped | The consumed Wake cannot fire again; dropping the execution cleans `G` and retained frame values; a queued source identity is stale and the Scheduler discards it through the queued tombstone. |
| endpoint callback reentrantly causes execution destruction | Before invocation, the runtime ends cell mutation but keeps the execution Notifying. Destruction records DestroyPending against the combined package; it cannot drop borrowed endpoint state or release that Allocation until callback return. |

The execution lifecycle includes Initial, Running, Dormant, Notifying, and Eligible. Wake-cell and
package coordination includes Registering, Latched, Cancelled, and DestroyPending behavior; a
realization may fuse tags or flags while preserving these transitions.
After the generation's sole Wake is consumed, a later Eligible drive may reinitialize the same
wake-control storage when it reaches another park. No live Wake crosses that reuse point.
Execution, Wake, and each active register/notification operation contribute reclamation authority.
A state transition may release the cancelled Allocation only after all three categories are gone;
source `G` is not part of that safety count. Live notification ends cell mutation and enters
Notifying before invoking `R(&O)` under an invocation retain. Reentrant destruction changes the
package to DestroyPending; callback return then performs deferred cleanup, or changes a still-live
Notifying execution to Eligible.

Wake-versus-destroy on one local thread is ordered by the next source operation. A true simultaneous
cross-thread race is intentionally deferred to the parallel memory proposal; it must preserve the
same one-shot and cancellation contracts with an atomic realization. The intrinsic never drives an
execution from `wake`. The NonParking endpoint callback borrows only its detached endpoint state and
may publish or enqueue readiness for a later owner Effect. If it indirectly retrieves the Notifying
execution and attempts to drive it before returning, the defined intrinsic-state trap applies.
No compiler scheduler token is required.

### Affine drive lifecycle

Affinity prevents concurrent drive, re-entry, and drive after completion:

```silk,ignore
struct TestReady {}
fn markTestReady(state: &TestReady) -> () { return () }

let execution = run Execution.make(
  effect { return 1 },
  TestReady {},
  markTestReady
)
let branch = OwnerLease.make()
let first = run Intrinsic.drive(move execution, move branch, onComplete, onSuspend)
let second = run Intrinsic.drive(move execution, anotherBranch, onComplete, onSuspend)
// Rejected: execution was moved into the first drive.
```

While an execution is Running, its owner has no `Execution` value to re-enter. Completion invokes a
callback that receives the one affine branch state and `A` but no Execution, so no execution is
returned to drive again. Suspension transfers that same branch state and the one handle through
`onSuspend`; dropping it makes every later source use an ordinary use-after-
move error. Affinity alone does not encode readiness, however. Calling `drive` on a Dormant execution
before its Wake was consumed, or on a Notifying execution before its endpoint callback returns, is a
defined fatal intrinsic-state trap; it does not resume, replace the fixed endpoint, or invoke any
drive callback. Initial and Eligible executions may be driven. The source
Scheduler prevents this trap by returning a `Ready` or `Parked` execution from `takeReady` only for
an identity first dequeued from ReadyInbox; a merely stored Parked execution is not selectable.

A freshly constructed execution already owns the closed Effect body and all of its captures even
before the first drive. Dropping it in that state cleans those values exactly once, invokes none of
`onComplete`, `onSuspend`, or the endpoint callback, and activates no wake state because no park has begun.
Never-driven cleanup uses the same compiler-derived capture cleanup plan as completion and dormant
destruction; construction is not a leak window.

### Values and owned providers retained across parking

```silk,ignore
struct IgnoreReady {}
fn ignoreReady(state: &IgnoreReady) -> () { return () }

effect fn retained(token: Token, logger: Logger) -> i32 {
  let copied = 1
  run Intrinsic.park(registerEvent)
  Logger.write(&logger, "ready")
  drop logger
  drop token
  return copied + 1
}

let execution = run Execution.make(
  retained(Token.make(), Logger.make()),
  IgnoreReady {},
  ignoreReady
)
```

The frame retains the Copy integer, affine token, compiler-owned registration state `G`, and owned
Logger handle used after the park. Each keeps its ordinary access mode and cleanup obligation. On
resumption the runtime drops `G` immediately before continuing to `Logger.write`; dropping the
dormant execution instead cleans `G`, `logger`, and `token` exactly once.

Construction requires the exact Effect representation to satisfy `Intrinsic.Detached`: it retains
no lexical loan, including a borrowed provider, and owns every value needed for later invocation and
drop. The compiler already records exact callable/Effect capture and provider provenance; the new
property exposes the derived fact to generic checking instead of adding an undeclared rejection at
a specialization. The safe generic wrapper declares the bound, so both of these calls are decided at
its ordinary construction boundary:

```silk,ignore
let owned = Token.make()
let valid = effect {
  drop move owned
  return 42
}
let execution = run Execution.make(move valid, IgnoreReady {}, ignoreReady) // accepted

fn borrowed(value: &i32) -> some<F: once Effect<i32>> F {
  return effect { return value }
}

let value = 42
let invalid = run Execution.make(borrowed(&value), IgnoreReady {}, ignoreReady)
// Rejected: F does not satisfy Intrinsic.Detached.
```

Eliminating a service requirement is a separate fact from detaching the provider environment:

```silk,ignore
service Clock {
  effect fn read() -> i32
  ? &Clock
}

effect fn readClock() -> i32
? &Clock {
  return run Clock.read()
}

fn withBorrowedClock(
  clock: &LocalClock
) -> some<F: once Effect<i32>> F {
  return readClock()
    |> Effect.bindRequirement<Clock>(clock)
}

let local = LocalClock.make()
let borrowed = withBorrowedClock(&local)
let invalid = run Execution.make(move borrowed, IgnoreReady {}, ignoreReady)
// Rejected: the requirement row is empty, but F retains the provider loan.

let ownedClock = readClock()
  |> Effect.bindRequirementOwned<Clock>(move local)
let valid = run Execution.make(move ownedClock, IgnoreReady {}, ignoreReady)
// Accepted when LocalClock satisfies Intrinsic.Detached.
```

Moving an Effect value transfers only its already-formed environment. It does not retroactively turn
a shared or exclusive capture into ownership. Values that must cross the delimiter are explicitly
moved into the Effect environment, and owned values may then be borrowed locally inside it. A failed
Detached diagnostic therefore distinguishes a remaining external provider loan from an unsatisfied
requirement row.

The property is a sealed compiler fact, not a source-customizable interface, runtime witness,
transfer permission, or promise that the captured values are immortal. The initializer then erases
one proven representation into uniform `Execution<A>` with exact hidden invoke and cleanup metadata.
It is not an implicit callable/Effect join, general source coercion, or permission for ordinary
nominal fields to omit [generics, interfaces, and specialization](../../docs/language/generics-interfaces-and-specialization.md) representation parameters. It performs no implicit cloning, source
allocation, or lifetime extension. A service such as Scheduler solves inheritance in ordinary
source by cloning an owned `Shared`-backed provider and moving it into the child. A service with no
such owned representation cannot cross an unscoped execution boundary.

Loans created *inside* an execution may remain live across `park` under [Effect suspension](../../docs/language/effect-suspension.md)'s stable-logical-
location rule. Their referents must be owned within the same execution, and target relocation must
preserve reference validity. Dormant cleanup ends each retained loan before cleaning its referent.
A loan into another execution or back across the construction/owner boundary remains rejected. For
example, a view created from an owned Buffer after first drive may cross parking and be used on
resume; a view captured from the constructing caller may not enter `Execution` at all. The initial
model marks Execution and Wake non-thread-transferable, so future cross-thread migration must separately
prove that every retained owner and loan is transferable before changing that classification.

### Execution delimiter and pay-for-use

Calling the `executionFromAllocation` boundary through a safe source wrapper is a static capability delimiter. The ownership/suspension
matrix under Static representation selection defines its minimum machinery. External-wake
reachability in `body` selects dormant continuation and wake state for that owned execution, but it
does not propagate through `Intrinsic.drive` into the ordinary Scheduler implementation itself. A
dynamic branch that does not park still pays the external-park tier when parking remains statically
reachable. Merely importing Fiber modules or constructing a Scheduler selects no execution tier.
Evaluator, native, and Wasm must agree on each tier's source behavior, while binary layout and
storage placement remain target-private.

### Implicit-root boundary

For child evented sleep, library registration parks the explicitly constructed child into Scheduler
ownership. Its wake enqueues the child's source identity; a runnable sibling may complete first. No
Fiber dependency is added to sleep itself.

This SLP deliberately stops when external parking is reached without an explicit Execution owner.
It does not make every ordinary `run` a hidden driver, give the executable entry special storage,
select target waiting or polling, define entry validation, or determine how a final outcome crosses
the entry ABI. SLP-0003 owns that independent root-adapter decision. Its desired source experience
may retain ordinary `run`; separating the adapter contract does not imply a public executor or a
concurrency-specific run operation.

The future Coroutine pressure example in the driving cases exercises the alternate-owner seam: its
source wrapper explicitly allocates shared payload state outside `Execution`, receives the suspended
handle, and later drives it again. It earns no Scheduler-specific compiler path.

## Semantic sketch

1. An ordinary-source owner obtains one exact combined Layout, allocates it through ordinary source
   policy, and initializes an affine execution from that Allocation, one closed
   Effect, detached endpoint state `O`, and reusable NonParking `R: fn(&O) -> ()`. Creation transfers all owned
   environments, fixes the readiness destination for the Execution lifetime, and establishes the
   external-wake propagation delimiter.
2. To drive, an ordinary owner moves an Initial or Eligible execution plus one affine branch state
   `D` into `drive`, supplying unit-returning NonParking completion and suspension callbacks that each accept
   `D`. Exactly one
   branch receives it. The execution becomes Running and proceeds directly until completion, nested
   transfer, or external-wake parking. Driving Dormant or Notifying is a fatal intrinsic-state trap
   before any callback is invoked. First drive roots that execution's own evaluator logical stack; later drives
   restore it. The owner's source frames and compiler-generated drive machinery are not logical
   ancestors of the driven execution, so scheduling order cannot change its `CallDepth` accounting
   or trace ancestry.
3. Nested `Effect.suspend` saves its parent state, immediately starts the known child, and resumes
   only from that child's typed completion. The existing LIFO specialization remains legal.
4. `park` invokes its NonParking ordinary-source registration function with one affine opaque Wake value. The
   returned generic guard and every live value are retained in the execution before it relinquishes.
5. The runtime retains the wake-control cell across the complete suspension callback. That callback
   receives ownership of the execution and returns completely before the fixed endpoint callback
   may run. A readiness signal during registration is remembered as Latched until the callback
   returns, closing the wake-before-dormant race even if the callback destroys the execution.
6. Consuming a live Wake transiently retains the cell and begins one readiness transition. The
   runtime ends cell mutation, holds the execution in non-drivable Notifying, and invokes `R(&O)`
   under an invocation retain. Reentrant execution destruction records DestroyPending and defers
   endpoint cleanup until callback return, so it cannot drop borrowed state or reclaim storage
   beneath `wake`. If the execution remains live, callback return changes Notifying to Eligible.
   The callback publishes readiness so a later owner Effect can select and drive it; indirect drive
   from inside the callback traps while Notifying.
7. Immediately before resumption continues after `park`, the runtime drops retained registration
   state `G`; `park` then returns unit and source rechecks its durable condition.
8. Completion transfers the one branch state and reified outcome to the completion callback and
   does not return an `Execution`; further drive is therefore unrepresentable.
9. Dropping a suspended `Execution` first marks its wake-control cell cancelled, drops `O`, `R`,
   registration state `G`, and every owned affine frame slot exactly once. A retained external Wake
   subsequently consumes as a no-op while keeping the complete inert combined Allocation alive; its
   consume or drop releases that Allocation when no transient
   access remains. Internal loans end before their owned referents during cleanup; no external loan
   may have crossed an `Intrinsic.Detached` boundary. Fatal continuation growth and illegal-state
   traps retain Silk's no-unwind rule.

## Compiler–standard library boundary

### Compiler necessity

Ordinary source cannot discover target live state, detach an active computation from the target call
stack, manufacture safe resume labels, own compiler-private frames, enforce single activation,
destroy dormant state exactly, or close the wake-before-dormant race. Those are the only reasons for
compiler privilege.

### Smallest target-neutral primitive

The selected surface is a hybrid: two opaque affine compiler types, callback-shaped drive outcomes,
and a fixed-layout readiness capability. The spelling remains illustrative, but these semantic
powers are selected:

```silk,ignore
// Opaque, affine, compiler-owned; it has no source fields or constructors.
Intrinsic.Execution<A>

// Opaque, affine, fixed-layout readiness capability; it has no source fields or constructors.
Intrinsic.Wake

// Compiler-owned static property: the exact representation owns its complete environment
// and retains no external lexical or provider loan. Source cannot implement this property.
Intrinsic.Detached

// Compiler-owned static property: the exact callable cannot reach external-wake
// parking after specialization. Direct work and nested-only suspension remain legal.
Intrinsic.NonParking

// Exact target Layout for the erased body, fixed endpoint, wake cell when
// reachable, and initial execution segment when required.
fn Intrinsic.executionLayout<
  A,
  F: once Effect<A> + Intrinsic.Detached,
  O: Intrinsic.Detached,
  R: fn(&O) -> () + Intrinsic.Detached + Intrinsic.NonParking
>() -> Layout

// Transfers one exact caller-funded package, a closed body, and its fixed
// readiness endpoint without running the body.
unsafe fn Intrinsic.executionFromAllocation<
  A,
  F: once Effect<A> + Intrinsic.Detached,
  O: Intrinsic.Detached,
  R: fn(&O) -> () + Intrinsic.Detached + Intrinsic.NonParking
>(
  allocation: Allocation,
  body: F,
  readyState: O,
  onReady: R
) -> Intrinsic.Execution<A>

// Consumes one activation and transfers branchState to exactly one outcome callback.
// Suspension returns Execution ownership through onSuspend; completion does not.
effect fn Intrinsic.drive<
  A,
  D,
  C: once fn(D, A) -> () + Intrinsic.NonParking,
  S: once fn(D, Intrinsic.Execution<A>) -> () + Intrinsic.NonParking
>(
  execution: Intrinsic.Execution<A>,
  branchState: D,
  onComplete: C,
  onSuspend: S
) -> ()

// Consumes one Wake. A live Wake completes one notification; if the execution remains live,
// callback return makes it Eligible. A cancelled Wake is a no-op.
fn Intrinsic.wake(wake: Intrinsic.Wake) -> ()

// The affine Wake supplied to `register` can make the current execution eligible.
// `G` is retained while dormant and dropped immediately before resumption.
effect fn Intrinsic.park<
  G,
  F: once fn(Intrinsic.Wake) -> G + Intrinsic.NonParking
>(
  register: F
) -> ()
```

`Intrinsic.Detached` and `Intrinsic.NonParking` are static compiler-owned properties. `Detached`
holds when an exact executable or value representation owns everything required for later invoke
and drop and retains no external lexical or provider loan. `NonParking` holds for an exact callable
when its specialized transitive reachability cannot reach `Intrinsic.park`; direct work and the
existing nested-only suspension path remain legal. Neither fact carries a runtime witness, can be
implemented by source, or implies Copy or thread transfer. An open generic preserves a fact only
when its declaration states the bound.

Revision 29 explicitly extends [generics, interfaces, and specialization](../../docs/language/generics-interfaces-and-specialization.md)'s exact-representation constraint model: one exact Effect or
callable representation bound may be conjoined with compiler-owned static properties by the shown
`+` spelling. Such a parameter remains an exact representation parameter; the additional conjuncts
do not reinterpret `Effect<...>` or `fn(...)` as interface bounds. Ordinary interface or service
conjuncts are not admitted in that position. This is a type-system admission change, not a runtime
witness or a general intersection-type feature, and it is required for the safe generic wrappers to
type-check before specialization.

`executionLayout` and `executionFromAllocation` follow the same caller-funded pattern as SLP-0002's
shared storage. Ordinary source requests one exact Layout from its selected Allocator and exposes
that allocation's typed failure before the unsafe initializer. The compiler
recognizes Allocation and Layout as low-level ownership values, not `Allocator`,
`OutOfMemoryError`, or a safe wrapper by spelling. A valid initializer consumes the complete package
or returns no Execution.

`executionLayout<A, F, O, R>` includes detached endpoint state, one reusable exact NonParking
callback over that state, and the stable control cell whenever `F` can reach external parking. The
initializer fixes this endpoint for the Execution lifetime. A wrapper for statically non-parking `F` supplies a
zero-sized no-op `O` and function item `R`, allowing the specialized layout to omit readiness state.
The endpoint is not collapsed into one capturing `N: fn() -> ()`. Under Silk's current exact-
callable model, a reusable `fn` can borrow non-Copy state supplied separately, while a partial
application that owns an affine state argument is consuming and therefore `once fn`; capturing a
borrow instead would fail `Intrinsic.Detached`. The `O` plus `R(&O)` split is what lets the package
own affine detached routing state and invoke the same callback after multiple parks without adding
a new closure or callable capability.
`drive` is safe because it has no caller-supplied layout invariant. Moving
the affine execution into it and receiving it back only through `onSuspend` makes concurrent drive,
re-entry, and drive-after-completion unrepresentable in safe source. During endpoint notification,
the execution remains Notifying and any indirect reentrant `drive` is the same defined fatal
intrinsic-state trap as a Dormant drive. Only after the callback returns does the execution become
Eligible for a later owner Effect. Dynamic routing remains expressible by
putting indirection in detached source state, while replacing the endpoint on each drive is not part
of this selected substrate.

Completion may transfer `A` out only when its provenance does not borrow the body environment,
continuation frames, endpoint, or any other storage cleaned at completion. This is the ordinary
loan-escape rule applied across the sealed boundary: the compiler rejects such a result before
construction or drive rather than erasing a dangling loan into `Execution<A>`.

`onSuspend` means external-park relinquishment only. Nested `Effect.suspend` transfers and completes
inside the same drive activation through the existing private nested driver; it never invokes the
owner's suspension callback.

`executionFromAllocation` is the explicit sealed erasure boundary for exact `F`, `O`, and `R`; the
runtime retains their hidden invoke and cleanup metadata. This does not permit implicit joins
between callable representations or make bare callback fields storable in ordinary source. The
nonescaping `onComplete` and `onSuspend` remain ordinary exact specialized callbacks. `drive`
transfers one generic affine branch state to exactly one of them, so an owner may use one non-Clone
lease without allocating shared branch storage.

The Wake value given to `register` is the readiness capability. `Intrinsic.wake` consumes it. A call
during registration records readiness but does not notify the owner until the complete `onSuspend`
callback has returned. A call after suspension makes the execution eligible and invokes `onReady`
at most once for that park; the same fixed callback may run again after a later park. Reaching
`park` relinquishes exactly once even when readiness was signalled during
registration. `Wake` is opaque rather than an ordinary callback because [generics, interfaces, and specialization](../../docs/language/generics-interfaces-and-specialization.md) requires stored
callables to preserve an exact representation parameter; a Deferred created before registration
cannot later change its nominal type to store an unknown callback representation. The fixed-layout
capability gives heterogeneous source state one storable type without adding general callable
boxing, existential source types, or indirect dispatch.

The Wake refers to a stable wake-control cell in the caller-funded combined Allocation, not a raw
pointer into continuation frames. Execution and Wake ownership plus transient Registering and
notification-preparation retains keep that cell alive. Execution drop atomically in the abstract machine—though not
necessarily with a hardware atomic in a local-only runtime—changes it to Cancelled before cleaning
the continuation. Consuming a retained Wake afterward is a safe no-op. Reclamation occurs only after
Execution, Wake, and transient-operation retains are gone. Cancellation retains the whole
indivisible combined Allocation, not a separately reclaimable header. Live notification ends cell
mutation and enters Notifying before borrowing endpoint state for `R(&O)`. An
invocation retain prevents reentrant destruction from dropping the borrowed endpoint or reclaiming
its Allocation; callback return performs any deferred cleanup or makes the execution Eligible.

`G` is ordinary source registration state owned privately by the park activation. The compiler
retains it while dormant and drops it either during dormant destruction or immediately before
resumed source continues after unit-returning `park`. Its cleanup may unregister and release an
external Wake promptly, but correctness of arbitrary `G` cleanup is not trusted for runtime memory
safety. Ordinary affine drop is therefore the execution destroy operation; no explicit
`Intrinsic.destroy` is needed.

The fixed endpoint and two `drive` callbacks have exact cleanup semantics because their state may
own affine values:

| Drive result | Callback ownership |
| --- | --- |
| completion | transfer branch state and `A` to `onComplete`; drop unused `onSuspend`; drop live package values and release the combined Allocation |
| parking | drop unused `onComplete`; transfer branch state and Execution to `onSuspend`; retain `G` and the fixed endpoint in the combined package |
| `onSuspend` destroys execution | cancel the cell; drop `O`, `R`, `G`, body, and frame values; suppress any Latched notification; retain the inert combined Allocation until the external Wake is consumed or dropped |
| dormant destruction | drop `O`, `R`, `G`, body, and frame values; retain inert cancelled-cell storage while a Wake remains |
| readiness | end cell mutation, enter Notifying, invoke `R(&O)` once under an invocation retain, then become Eligible after `R` returns |
| readiness plus reentrant destruction | record DestroyPending; after `R` returns, drop remaining live values and release the combined Allocation instead of becoming Eligible |
| fatal trap | Silk's language-wide no-unwind rule applies; no cleanup guarantee is introduced |

Typed failure also does not enlarge the surface. The standard library reifies it before construction:

```silk,ignore
let execution = run Execution.make(
  Effect.result(move body),
  move readyState,
  move onReady
)
```

The execution then completes with ordinary `Result<A, E>` data. Pending remains visible only to its
owner through the `onSuspend` callback and never enters ordinary Effect success or failure channels.
The intrinsic signatures mention only caller-owned Allocation, never an Allocator service or typed
allocation failure. The ordinary safe `Execution.make` wrapper obtains the exact Layout and
therefore exposes `! OutOfMemoryError ? &mut Allocator`. Later continuation-stack growth remains
fatal and does not enter the Effect's typed channels.

### Standard-library construction

The sufficiency witness must implement ready-queue policy, Fiber result handles, Deferred state and
waiter lists, joins, and provider composition in ordinary Silk. Every Fiber operation in the witness
may require the high-level Scheduler service; park and timer operations do not gain that requirement
merely because they execute inside a Fiber.

The Scheduler need store only homogeneous `Execution<TaskOutput>` values. A typed fork wrapper first
reifies the child outcome with `Effect.result`, publishes `Result<A, E>` into source-owned Fiber
result state, and then returns unit. The typed Fiber handle retains the result state while the
Scheduler sees only the homogeneous `TaskOutput` execution and a source-owned identity.

The detailed witness selects direct pay-as-needed source allocation rather than a hidden Scheduler
arena. `LocalScheduler.make` calls `Shared.make` for the task store and ready inbox and therefore carries
`! OutOfMemoryError ? &mut Allocator`. `Deferred.make` calls `Shared.make` once. A joinable
`Fiber.fork` constructs its typed result Deferred and a task reservation; a resultless infallible
spawn omits only the result state; both paths also construct one combined execution/endpoint
package. ReadyInbox owns the reservation's identity slot, while Execution owns the combined
package, so task notification never allocates; an eligible cancellation keeps a queued
tombstone alive until dequeue. `Fiber.join` allocates its waiter node in this direct witness. Each
construction exposes the Allocator and failure rows shown above. If result allocation fails, `body`
is cleaned. If task reservation or package allocation fails afterward, all earlier Deferred/reservation
state and `body` are cleaned and no task is inserted. No memory is reserved merely to keep those
rows out of application Effects.

Under SLP-0002, Deferred state, Fiber result state, and the ready inbox can be `Shared` values.
One detached endpoint state owns a shared inbox handle and source-owned identity; its reusable
callback borrows that state and enqueues only the identity.
`Deferred.await` and an evented timer store the fixed-layout Wake received from `park`, retain
their own cancellation or unlink guard as `G`, and recheck their source-owned condition after
resumption. A producer updates Deferred state under a non-suspending exclusive access callback and
consumes extracted Wake values only after releasing that access. A buggy or deliberately minimal
source may leave a Wake registered after execution destruction; the hidden cancelled wake cell makes
that memory-safe, while `G` exists for prompt resource cleanup. None of those actors
is known to the compiler.

The task store and ready inbox are distinct ownership actors. The task store owns each dormant
`Execution<TaskOutput>`; the execution's fixed endpoint state owns only a clone of the ready inbox.
This separation prevents the source-level strong cycle that would arise if a callback captured the
same shared state that owns its execution.

This source state is allocated at an explicit ownership boundary such as `LocalScheduler.make`,
`Deferred.make`, `Scheduler.schedule`, `Execution.make`, `ReadyWaiter.make`, or collection growth.
Merely suspending, parking, cloning a shared handle, waking, or returning a value does not acquire an
Allocator requirement. The canonical concurrency-library SLP may select another explicit storage
policy, but that choice does
not enlarge this proposal's compiler surface.

The caller-funded combined Execution package and source actors follow explicit allocation and failure
contracts. Only later dynamic compiler continuation-stack growth is a runtime responsibility whose
exhaustion is fatal.

`executionFromAllocation` consumes an Effect and endpoint state/callback whose exact representations have already proven their
`Intrinsic.Detached` bounds and the callback's `Intrinsic.NonParking` bound from the same
environment-dependency, capture-loan, and reachability facts used by
stored Effects and callables. The sealed operation does not add a specialization-time lifetime
check or erase unproven provenance into `Execution<A>`. Owned provider values remain ordinary
captures and are cleaned with the execution; no runtime requirement-row dictionary, public capture
parameter, or public lifetime parameter is added. A Scheduler provider intended to outlive the
spawning call is therefore a small owned value backed by the independently shared state supplied by
SLP-0002.

A future explicit Coroutine wrapper is a reuse witness, not part of the selected library. It may
store yield and resume payloads in explicitly allocated shared source state and use the same
execution lifecycle. A later proposal owns its public types, allocation API, and difficult rules
such as yielded borrows.

### Privilege audit

A bare `park()` is too small if registration happens separately: notification can be lost and
destroy-time unlinking becomes a convention. A callback-only continuation without an affine
`Execution` is also too small: an ordinary owner cannot store, select, and destroy unfinished
computations without recreating an execution handle under another name. A compiler-known Deferred,
Scheduler, Fiber, timer, or Coroutine is too large because its state and policy are expressible once
execution transfer is safe. A public general continuation is also too large: it exposes target
control representation and permits re-entry powers the driving cases do not require.

The owner-neutral boundary is intentional minimal generality. A Scheduler-only primitive would be
smaller by name but larger in compiler policy and would block source reuse. Conversely, an affine
execution does not earn arbitrary typed yields, multi-shot resume, cloning, or symmetric transfer.
An intrinsic step sum, explicit destroy, per-drive readiness callback, and separate yield are
subtracted: callback-shaped drive outcomes with one affine branch state, affine drop, and
source-owned endpoint state already express those roles. Once task-specific push readiness is
selected, Wake and its consuming operation remain because a Deferred created before registration
cannot name the exact endpoint representation bound to the Execution. They add readiness capability,
not Scheduler or payload policy.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Affected | No new token or canonical library name is selected. Existing `+` bound spelling gains the narrow rule that one exact executable representation bound may retain its identity while conjoined with sealed static properties. The Intrinsic catalog gains `Execution`, `Wake`, `Detached`, `NonParking`, `executionLayout`, `executionFromAllocation`, `drive`, `wake`, and `park`. |
| Types and abstraction | Affected | The sealed seam adds opaque affine, initially non-thread-transferable `Intrinsic.Execution<A>` and `Intrinsic.Wake`, plus compiler-owned no-external-loan and no-external-park generic properties; public Fiber and Coroutine types are deferred. |
| Execution contracts | Affected | An Effect may internally relinquish without completing. Source combined-Execution packaging has explicit allocation failure; Pending and later continuation-stack exhaustion remain absent from ordinary Effect outcomes. |
| Ownership and resources | Affected | Execution ownership, caller-funded packages, rejection of external loans in bodies and readiness endpoints, permitted stable internal loans, callback/cell transient retains, registration guards, affine Wake values, single activation, and exact dormant cleanup are central. |
| Runtime and targets | Affected | Evaluator, native, and Wasm need execution-owned frame storage, fatal growth exhaustion, and resume parity. Each explicit Execution owns a logical evaluator stack rooted at first drive. Implicit entry ownership is deferred to SLP-0003. |
| Compiler | Affected | Exact executable bounds retain identity alongside sealed property conjuncts. Reachability, NonParking proofs, suspension-mode summaries, execution delimiters, liveness, frame layout, resume, destroy, and lowering participate. |
| Standard library | Affected — sufficiency witness only | Source-defined owners and policies prove the substrate usable; canonical concurrency and Coroutine APIs are deferred. |
| Tooling and diagnostics | Affected | Duplicate affine use and failed Detached, NonParking, exact-bound, or target admission need compile-time diagnostics. Dormant or Notifying drive is a defined fatal runtime trap, while consuming a cancelled Wake is valid and silent. |
| Learning and use | Affected | Documentation distinguishes hidden representation from visible relinquishment and nested transfer from external wake. |

## Scope cohesion

This SLP asks one question: should Silk complete its compiler-private suspension machinery into an
owner-neutral independently resumable Effect execution sufficient for closed leaf tasks, Deferred
coordination, and event waits under an explicit source owner? Parking is the decisive new suspension
policy and a minimal local Fiber runtime is the primary witness. Full compositional structured
fibers are not claimed by this substrate witness: nested joins require a dependent concurrency
design for cycle-safe child-visible scheduling, scoped ownership, or another provider architecture.

Initial execution ownership is part of that same lifecycle rather than a separate general erased-
Effect thesis. If the sealed boundary accepted exact `F` only for immediate first activation and
created an opaque handle only after parking, every ordinary-source owner would be forced to run a
child eagerly to its first relinquishment. It could not homogeneously enqueue closed Effects and
choose first-activation order. `Execution<A>` is the one purpose-bound affine container whose Initial,
Running, Dormant, Eligible, completion, and drop states make owner-selected first and later drives
uniform. It does not create a source coercion between callable representations or expose arbitrary
invoke metadata outside this lifecycle. Eager owners remain free to drive immediately.

The selected capability point has three independently subtractable dimensions: an eager-start
execution can omit Initial, an owner-sweep execution can omit push Wake, and runtime-owned storage
can make admission fatal. Alternative parameter values do not by themselves create three
programmer concepts or three useful standalone features. Each dimension changes the same sealed
Execution lifecycle needed by the same driving program; none exposes a distinct public feature or
independently teachable abstraction. This SLP therefore makes one
whole-boundary decision for the Scheduler-grade point: before one task is published, its
heterogeneous body is owned but unstarted, one fixed compiler package is recoverably funded, and one
task-specific readiness endpoint is bound for its lifetime. The first-activation case, large
dormant-set readiness pressure, and allocation rollback expose the dimensions without turning every
alternative coordinate into a separate SLP. Splits remain appropriate for SLP-0002 and SLP-0003
because shared ownership and implicit root adaptation introduce distinct programmer concepts and
independently useful driving cases.

Canonical concurrency policy is a dependent proposal because names, fairness, scopes, cancellation,
and shutdown can vary without changing the substrate. A public Coroutine API is another dependent
proposal because its yield and borrow model is independently observable. Parallel execution is a
third proposal because it adds a memory model, transfer/share rules, atomics, OS threads, and work
stealing. These possible consumers strengthen the separation requirement but do not enlarge this
SLP's public surface.

Allocation-backed local shared ownership is split into SLP-0002 because it answers an independent
question: how ordinary Silk code can give several dormant values local access to one dynamically
lived mutable allocation. It is needed by the source sufficiency witness but is useful without
parking, and it has its own allocation, access-conflict, and last-handle cleanup rules. This SLP
therefore assumes that capability instead of smuggling shared state into Execution or making the
compiler recognize Deferred and Scheduler. Every direct dependency gates this proposal:
[values and types](../../docs/language/values-and-types.md)'s canonical value foundation, [generics, interfaces, and specialization](../../docs/language/generics-interfaces-and-specialization.md)'s generic and exact-representation model, [unsafe code, intrinsics, and targets](../../docs/language/unsafe-intrinsics-and-targets.md)'s
unsafe and sealed-intrinsic authority, [runtime and standard-library boundary](../../docs/language/runtime-and-standard-library.md)'s language/library/runtime layering, [Effect suspension](../../docs/language/effect-suspension.md)'s
nested suspension and execution-stack contract, and SLP-0002's shared ownership must each be
accepted or replaced consistently before an implementation handoff. The linked language foundations
are Confirmed and SLP-0002 reached Accepted direction at revision 6, so this resolution satisfies
the dependency gate. The narrow sealed erasures selected here are an explicit extension point, not evidence that
[generics, interfaces, and specialization](../../docs/language/generics-interfaces-and-specialization.md) already permits general existential callables.

Implicit program-entry ownership is split into SLP-0003 because it answers a second independent
question: whether entry adaptation automatically constructs an Execution, which targets can retain
and wait for it, what resources fund it, and how its final outcome crosses the selected entry ABI.
That decision enables root-only event waits but is unnecessary for an explicit source Scheduler,
Deferred, or future Coroutine owner. SLP-0003 depends on this substrate; this SLP neither assumes nor
prejudges its outcome.

## Complexity and subtraction budget

The author selects two opaque affine concepts, two sealed static properties, and five sealed
operations: `executionLayout`, `executionFromAllocation`, callback-shaped `drive`, consuming `wake`,
and race-free `park`. Dormant cleanup remains
ordinary affine drop backed by compiler-generated frame cleanup. The design reuses the existing
continuation foundation rather than earning a second coroutine implementation. This is not the
absolute smallest substrate that can make a local Fiber eventually progress. It is the smallest
selected surface that simultaneously preserves owner-controlled first activation, task-specific
push readiness suitable for O(1) queues, and explicit recoverable procurement of one fixed combined
package. Binding one reusable endpoint during construction removes per-drive callback
erasure and Allocation handoff. Passing one affine branch state to `drive` removes both a sealed
step-result sum and the need to duplicate owner captures across its outcome callbacks.

`NonParking` is the narrow static fact needed because an ordinary Silk `fn` may run a closed Effect;
callable syntax alone does not exclude external parking. Applying the fact only to runtime-invoked
registration, endpoint, and drive-outcome callbacks prevents recursive relinquishment without
making all ordinary callables effect-polymorphic or making every `wake` conservatively park-capable.

Wake and `wake` are the minimum addition for task-specific push readiness under the exact-callable
representation model. The
alternative would be a broader nameable existential callable representation, general callback
boxing, or a representation parameter that source state created before registration cannot know.
The hidden stable control cell makes the fixed-layout capability safe after execution destruction:
cancellation turns a late Wake into a consuming no-op, and the final execution/Wake reference
reclaims the complete inert combined Allocation.

It reuses existing Layout, Allocation, and ordinary Allocator policy instead of earning a compiler-
known allocator service or new storage-failure type. Packaging failure is visible at source
construction; only later dynamic continuation-stack growth retains fatal runtime policy.

The feature does not earn universal pollability, a public continuation, a compiler scheduler,
implicit globals, compiler-known synchronization actors, or atomic cost in local-only executables.
Programs using ordinary `run` that cannot reach suspension remain direct. Programs using ordinary
`run` that reach only `Effect.suspend` retain the nested/LIFO specialization. Explicit construction
always earns an owned executable container; only external-wake-reachable executions add dormant
continuation and Wake state.

## Surface displacement

No new grammar token or canonical library actor is selected, but exact executable bounds gain the
narrow sealed-property conjunction semantics defined above. The new compiler surface is sealed and
target-neutral. The major representation change is that continuation storage can be owned by an execution
rather than only a thread-local LIFO stack. The source surface displaced from the compiler includes
Fiber, Scheduler, Deferred, timer, queue, Coroutine, and host policy.

The current monolithic “suspendable” classification may need to become a capability summary that
distinguishes nested transfer from external-wake suspension. That refinement is necessary to
preserve both the implementation seams and the cheaper existing path.

## Drawbacks and risks

- Owner-neutrality may expose more sealed lifecycle surface than a Scheduler-specific callback.
- Callback-shaped drive outcomes are less direct to read inside the standard library than a sealed
  step-result sum, although wrappers can immediately translate them into ordinary source types.
- Execution-owned storage and dormant destroy paths enlarge the compiler's memory-safety surface.
- A retained but never consumed or dropped cancelled Wake keeps the complete combined Allocation
  live after all values are cleaned, because Allocation is one indivisible reclaim ticket.
- Fatal continuation-storage exhaustion is not recoverable by ordinary Effect code, so target
  sizing and runtime growth policy remain operational concerns.
- Requiring an owned detachable environment excludes unscoped child executions that inherit
  arbitrary borrowed services; those programs need owned service handles or a later scoped-fiber
  model.
- A child execution cannot retain the ordinary `&mut Allocator` used by its spawning call. Source
  allocations performed by `fork` complete before execution construction; allocation later inside a
  detached child requires an owned allocator capability, explicit ownership transfer, or a future
  scoped model.
- An implementation may share names while still coupling frame layout to Scheduler or parking
  policy; the separation must be proven with alternate-owner pressure.
- Root-only external parking is not enabled until SLP-0003 selects an implicit entry owner; this SLP
  alone proves only explicit owner construction.
- Mechanical destroy must not accidentally promise source-level unwinding or cancellation.
- Conservative pre-specialization suspension summaries may impose independent-execution lowering on
  a generic body until each reachable complete application selects its static target.

## Alternatives and prior art

### Status quo

Keep only direct Effects and nested `Effect.suspend`. This preserves the smallest runtime but cannot
express even closed source-defined leaf fibers, nonblocking Deferred waits, evented sleep, or host
I/O waits.

### Smaller primitive or library solution

Expose no execution substrate and require ordinary source to return `Ready<A> | Pending<State>`.
This can implement scheduling but infects every operation and combinator with a second execution
model and gives up compiler-derived live-state and cleanup facts.

A bare source-callable `park()` is superficially smaller, but it cannot safely separate registration
from relinquishment or give an ordinary owner control of the dormant execution lifecycle.

The strongest smaller counterproposal has one opaque `Execution<A>`, the `Intrinsic.Detached` and
`Intrinsic.NonParking` properties, and three operations: `start(exactBody, onComplete, onRelinquish)`,
`drive(execution, onComplete, onRelinquish)`, and `relinquish(guard)`. It starts each exact child
immediately, creates an Execution only after relinquishment, allows every dormant execution to be
redriven for a durable-condition recheck, and makes the owner sweep dormant tasks after another task
step or same-thread reactor event. Compiler execution-stack exhaustion remains fatal.

That model can make the Deferred and local timer cases eventually progress, but it gives up three
selected guarantees: a Scheduler cannot choose first activation order for heterogeneous tasks;
readiness requires an O(n) sweep rather than publishing one ready identity; and fixed package
failure is fatal rather than exposed through caller-funded Allocation. It also provides no natural
task-specific notification shape for later parallel schedulers. The author therefore rejects it for
this proposal while retaining it as the strongest subtractive alternative, not as an incorrect
implementation.

Those losses are independently selectable rather than an all-or-nothing bundle:

| Axis | Selected contract | Smaller coherent contract | Exact loss |
| --- | --- | --- | --- |
| first activation | Initial Execution is publishable before running | eager `start` creates Execution only on relinquishment | owner cannot choose first activation among heterogeneous bodies or drop one never-started |
| readiness | consuming Wake publishes one task identity | legal dormant redrive plus owner sweeps after task/reactor turns | one ready task may require O(n) dormant rechecks and gives no task-specific future parallel notification |
| fixed storage | one combined package is caller-funded before publication | runtime-owned fatal packaging | bounded owners lose recoverable admission, rollback, pooling, and accounting |

The primary program needs eventual progress under every combination, but the selected Scheduler-grade
contract additionally chooses deterministic first activation, task-specific push readiness, and
recoverable pre-publication procurement. A larger per-drive endpoint model would allow readiness
retargeting and fresh affine notification payloads on every activation; no driving case requires
those powers, so the selected attach-once endpoint deliberately omits them.

### Strongest competing language model

Compile every Effect into a universal public pollable coroutine and make one executor the normal
interpreter. This unifies representation and resembles Future/Poll systems, but exposes pending state
and imposes coroutine machinery unless optimization removes it. The selected direction instead
keeps representation private, makes suspension-mode reachability explicit, and preserves direct and
nested-only paths.

Another competitor is to graft wake and ready-queue behavior directly onto the current thread-local
LIFO driver. That may minimize an initial patch, but it conflates continuation mechanics, ownership,
and Scheduler policy; it cannot resume executions in non-nested order and would require replacement
for future owners or migration.

Relevant evidence includes Effect-style callback/resume runtimes, Rust Future/Waker separation,
current-thread and work-stealing executors, generators, and asymmetric coroutines. Silk adopts none
of their public representations automatically; their value is evidence for separating generated
state, ownership, wake eligibility, and scheduling policy.

## Falsifiers and acceptance blockers

- A wake can be lost before the execution becomes dormant.
- Consuming a Wake after its execution was dropped accesses cleaned continuation state, invokes
  already-dropped `O` or `R`, or performs any operation other than a safe no-op.
- One execution can be resumed concurrently, re-entered while running, or driven after completion or
  destruction.
- Destroying a dormant execution leaks, duplicates, or double-cleans an affine value or owned
  provider.
- An external lexical value or provider loan satisfies `Intrinsic.Detached`, is erased into
  `Execution<A>`, or is retained by its readiness endpoint.
- A callable satisfies `Intrinsic.NonParking` while its specialized reachability can reach
  `Intrinsic.park`, or `wake` must conservatively inherit external-parking capability from erased
  endpoint code.
- Conjoining a sealed static property with one exact Effect or callable bound causes the generic to
  lose its exact representation identity or treats that executable bound as an interface.
- A valid loan created inside an execution cannot retain a stable logical referent across parking,
  or dormant cleanup destroys the referent before ending the loan.
- Completion transfers an `A` that borrows body, frame, endpoint, or package storage which completion
  then cleans.
- Ordinary Effect combinators must expose Pending or recognize concurrency actors.
- Execution construction fails to delimit propagation, making a source Scheduler parkable merely
  because the owned Effect can park.
- The compiler recognizes Fiber, Scheduler, Deferred, timer, or Coroutine declarations by spelling.
- Continuation layout, liveness, resume, or cleanup is coupled to Scheduler or external-wake policy
  such that another ordinary-source owner requires a second implementation.
- A program using only direct Effects links suspension machinery.
- A program using only `Effect.suspend` gains independent execution, wake, scheduler, or atomic
  machinery.
- An explicitly constructed non-suspending execution is claimed to be identical in representation
  cost to ordinary direct `run`, despite owning an erased never-driven-droppable body.
- The owner or wake capability can be cloned or used to create two simultaneous activations.
- Execution or Wake becomes thread-transferable before a parallel proposal defines and proves its retained
  capture and memory rules.
- Driving a Dormant or Notifying execution progresses it, replaces its fixed endpoint, or has undefined
  behavior instead of the selected fatal intrinsic-state trap.
- Supported evaluator, native, and Wasm execution owners disagree on typed outcomes, eligibility, or
  cleanup.
- Source Execution packaging uses more than one Allocation, allocates without a caller-funded exact
  Allocation, hides allocation failure, or fails to retain or release the complete package on a
  selected lifecycle path.
- Reentrant destruction during endpoint notification drops borrowed endpoint state, releases its
  Allocation before callback return, or restores the endpoint after DestroyPending cleanup.
- `drive` requires an owner to clone or share one affine branch lease merely to cover completion and
  suspension outcomes instead of transferring it to exactly one callback.
- Resumed source receives registration state `G`, or the runtime fails to drop `G` exactly once
  immediately before resumption or during dormant destruction.
- Later dynamic continuation-stack growth adds a recoverable Effect failure instead of retaining
  [Effect suspension](../../docs/language/effect-suspension.md)'s fatal execution-stack policy.
- This proposal claims that a park-capable complete entry has an implicit owner or defined final
  outcome without depending on SLP-0003.
- After using SLP-0002 for local shared state, the non-normative source witness still cannot
  implement fork, wait, wake, and resume without additional compiler-known library policy.
- The source implementation shape moves a strong Scheduler provider into an Execution stored by that
  same provider's TaskStore, creating an uncollectable SLP-0002 cycle; nested joins remain a declared
  boundary until dependent concurrency policy supplies a cycle-safe capability.
- The sufficiency witness requires allocation during Wake notification or hides any task, join,
  result, execution, or readiness allocation merely to remove Allocator and failure rows. Whether a
  canonical resultless spawn or join allocates visible policy state remains a dependent library choice.

## Open realization questions

These questions may refine compiler facts, target storage, and diagnostics but may not change
affine non-thread-transferable ownership, the explicit `Intrinsic.Detached` and
`Intrinsic.NonParking` admission contracts and their exact-bound conjunction rule,
one caller-funded combined package, the narrow sealed erasure boundaries, callback-shaped
drive, opaque Wake, race-free park, whole-allocation cancelled wake lifetime, fatal
post-construction stack growth, or the absence of compiler-known scheduling actors:

- The exact diagnostic and internal semantic-fact representation used to derive and report a failed
  `Intrinsic.Detached` bound for a concrete Effect or readiness endpoint representation. The proof
  is over the exact executable environment and owned-provider captures, not the Effect's success or
  failure payload types; realization evidence must cover borrowed and owned providers, nested
  nominal captures containing a loan, and an opaque producer result.
- The diagnostic presentation and cached reachability fact for a failed `Intrinsic.NonParking`
  bound. Its semantics are fixed here as absence of transitive external-wake parking after
  specialization; only the internal summary representation remains open.
- How OpenSpec scenarios encode observational all-or-nothing task publication, every row of the
  wake-ordering table, and every completion, suspension, dormant-destroy, cancelled-Wake,
  notification, and DestroyPending cleanup path across evaluator, native, and Wasm.
- How exact executable identity plus sealed property conjuncts survive substitution, caching, and
  serialization without becoming nominal conformance or a general intersection type.
- Which target-specific post-construction growth increments and pooling strategy should realize the
  fatal continuation-stack contract after caller-funded packaging.
- How a safe source wrapper pools the exact combined Layout without changing visible procurement
  failure, whole-package ownership, or the zero-sized endpoint path for statically non-parking `F`.
- How evaluator, native, and Wasm represent one execution-relative stack root, segment/growth state,
  logical depth, and destroy root while preserving the cheaper current global/LIFO path for nested-
  only suspension. Evidence must alternate two explicit executions in non-LIFO order.
- Which same-thread native and Wasm reactor/poll paths can deliver system-event Wake values without
  making Wake transferable or adding atomic machinery.

## Future directions

A dependent concurrency-library SLP may define canonical Scheduler and Fiber APIs, structured
lifetimes, joins, cancellation, fairness, task storage, synchronization actors, timers, reactors,
daemon fibers, concurrent Stream operators, and whether source state uses direct allocation or a
Scheduler-owned arena.

A separate Coroutine SLP may expose an asymmetric user API over the same execution substrate and
define typed yields, resume inputs, yielded-borrow lifetimes, and symmetric or multi-shot exclusions.
Its implementation should reuse continuation, ownership, drive, resume, and destroy mechanisms.

A timer and system-event proposal may select blocking and evented sleep providers while preserving
execution-relative parking. SLP-0003 owns implicit synchronous root adaptation; a later
host-integration proposal may separately define asynchronous outcome delivery, cancellation, and
exported ABI for hosts whose calls return before the root completes.

A later parallel-execution SLP may define transfer/share derivation, safe cross-thread borrows,
atomics and memory ordering, OS thread providers, migratable executions, and work stealing.
It also owns the atomic realization that permits an affine Wake to cross threads; consuming that
Wake must still never drive an execution inline.

## OpenSpec realization map

The accepted direction is handed off through these capability slices:

1. target-neutral suspension-mode summaries and the owner-neutral execution lifecycle;
2. caller-funded combined Execution packaging, later fatal stack growth, drive, resume, and exact destroy;
3. fixed-layout affine Wake, transient cell ownership, and race-free external-wake parking;
4. evaluator, native, and Wasm parity for explicit executions; and
5. static pay-for-use and alternate-owner separation evidence.

Every slice must trace the fixed publication ordering, wake transition table, cleanup matrix,
`Detached` and `NonParking` derivation, exact-executable-plus-sealed-property admission, Notifying and
DestroyPending behavior, execution-local roots, and evaluator/native/Wasm parity into executable
requirements and scenarios. Those realization choices may refine representation and diagnostics but
may not reverse the accepted capability point.

Canonical concurrency and Coroutine APIs require dependent SLP decisions before their own OpenSpec
handoffs.

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-21 | Seeded the Draft from a Deferred.await blocker; preserved [Effect suspension](../../docs/language/effect-suspension.md) stack suspension, proposed independently owned execution and race-free parking, and assigned scheduling policy to ordinary source. |
| 2 | 2026-08-21 | Selected deterministic local concurrency with future-compatible execution ownership while leaving parallel transfer and memory rules to a dependent proposal. |
| 3 | 2026-08-21 | Removed LocalExecutor and concurrency-specific run; ordinary run executes after ordinary Scheduler provider elimination. |
| 4 | 2026-08-21 | Made Scheduler the sole illustrative high-level service and clarified that Deferred is a source sufficiency witness, not a primitive. |
| 5 | 2026-08-21 | Added the current-capability ladder and identified independent ownership plus runtime parking as the wall above existing continuation machinery. |
| 6 | 2026-08-21 | Narrowed scope from defining structured fibers to enabling source-defined fibers; canonical concurrency APIs and parallel execution moved to dependent proposals. |
| 7 | 2026-08-21 | Added root-versus-child timer pressure, selected execution-relative rather than Fiber-relative parking, and tied pay-for-use to static parking reachability. |
| 8 | 2026-08-21 | Reframed parking as an extension of compiler-managed suspension: Effect.suspend is nested child-completion policy, while parking adds independent ownership and external-wake non-nested resumption. |
| 9 | 2026-08-21 | Rewrote the Draft around owner-neutral independently resumable Effect executions. Selected explicit semantic seams between continuation construction, execution lifecycle, suspension policies, and source policy; added execution construction as a propagation delimiter, future Coroutine reuse pressure, and direct/nested/independent pay-for-use tiers. |
| 10 | 2026-08-21 | Selected one opaque affine `Execution<A>` plus `execution`, callback-shaped `drive`, and callback-shaped `park`. Subtracted compiler-owned Wake and step types, explicit wake and destroy operations, and a separate yield; fixed ownership handoff before readiness notification and left requirement-capture typing as the principal realization gate. |
| 11 | 2026-08-21 | Split allocation-backed local shared ownership into SLP-0002 after the source sufficiency witness proved that Deferred state and ready-inbox callbacks require dynamic shared lifetime. Normalized Scheduler tasks to `Execution<()>` through source-owned typed result state, reused ordinary hidden capture-loan tracking for provider bindings, and kept allocation at explicit shared or task construction boundaries rather than suspension itself. |
| 12 | 2026-08-21 | Adopted SLP-0002's `Shared<T>` public actor name while retaining its non-transferable, non-atomic local execution semantics. |
| 13 | 2026-08-21 | Selected compiler/runtime-owned continuation storage for `Execution`, with no public Allocator requirement or recoverable allocation channel. Storage exhaustion is fatal like existing suspension-frame exhaustion; placement, growth, pooling, and future migration remain target/runtime realization details. |
| 14 | 2026-08-21 | Completed the ordinary-source Scheduler/Fiber/Deferred sufficiency witness and the remaining pressure cases. Separated affine task ownership from the shared ready inbox to avoid a source strong cycle; specified join-driven progress without a special run method, register-before-park ordering, wake and dormant-drop behavior, affine drive invalid states, retained value and provider rules, execution-delimited pay-for-use tiers, and root versus child host-event ordering. |
| 15 | 2026-08-21 | Author explicitly promoted the completed direction to Candidate. This revision changes lifecycle metadata only and freezes the dossier for fixed-revision adversarial review. |
| 16 | 2026-08-21 | Selected owned detachable executions. `Intrinsic.execution` now consumes only a closed Effect whose exact representation retains no lexical loans; Scheduler inheritance clones an owned `Shared`-backed provider into the child before construction. Arbitrary borrowed child environments are deferred to a scoped-fiber proposal rather than erased into homogeneous `Execution<A>` storage. |
| 17 | 2026-08-21 | Kept callback-shaped readiness and selected a hidden stable wake-control cell. Execution destruction cancels the cell before cleanup, drops `onReady`, and makes any externally retained late wake a safe no-op; `G` may unlink resources promptly but is not trusted for memory safety. Also clarified that the complete `onSuspend` callback returns before readiness notification. |
| 18 | 2026-08-21 | Selected a direct pay-as-needed allocation path for the source witness. Scheduler, Deferred, task, and result-state construction expose ordinary Allocator and allocation-failure rows instead of hiding them behind a pre-reserved arena. Joinable fork allocates observable result state; resultless infallible spawn does not, and reusable ready-link storage prevents wake-time allocation. |
| 19 | 2026-08-21 | Removed browser completion from scope: it had been an owner-neutrality pressure example, not a proposed embedding feature. Park-capable roots require a compatible target owner, while asynchronous exported-root ABI, eventual outcome delivery, and host cancellation are delegated to a separate host-integration proposal. |
| 20 | 2026-08-21 | Completed the r001 coherence pass: stale ready identities are discarded, never-driven execution cleanup is explicit, and the illustrative fork accepts a closed child Effect rather than designing automatic Scheduler inheritance. This keeps `TaskStore -> Execution -> Scheduler -> TaskStore` cycles out of the witness and leaves scoped or owned provider inheritance to the concurrency-library proposal. |
| 21 | 2026-08-21 | Resolved fresh review r004. Replaced callback-shaped readiness with fixed-layout affine `Wake` plus consuming `wake`, because [generics, interfaces, and specialization](../../docs/language/generics-interfaces-and-specialization.md) cannot store an unknown callback representation in pre-existing Deferred state. Required retained `onReady` to be closed and loan-free; defined callback cleanup, dormant-drive trapping, queued tombstone lifetime, explicit execution-erasure costs, execution-local evaluator depth, Timer requirements, and Coroutine source allocation. Explicitly defended fatal runtime execution-stack exhaustion while keeping all ordinary source state allocation visible. |
| 22 | 2026-08-22 | Resolved fresh review r005. Made execution and retained-readiness packaging caller-funded through exact Layout and Allocation while preserving fatal post-construction stack growth; completed the synchronous target-root delimiter; permitted stable execution-internal loans while rejecting external loans; added transient Registering/Notifying wake-cell ownership; made nested-join waiter allocation explicit; returned reusable readiness storage through drive callbacks; and removed witness-specific result/join allocation policy from the substrate falsifiers. |
| 23 | 2026-08-22 | Resolved review r006 after the author selected its repeated scope fork. Split implicit program-entry ownership into Draft SLP-0003 while preserving ordinary `run` as that proposal's desired source experience; added the compiler-owned `Intrinsic.Detached` generic admission property; retained the whole readiness Allocation behind a cancelled Wake and completed the unique live-notification handoff; made per-drive readiness packaging explicit even for non-parking Executions; corrected Scheduler as a service with `LocalScheduler` provider and explicit owned provider binding; completed Timer preparation, Coroutine wake, and nested-join allocation examples; and gated every unresolved direct dependency. |
| 24 | 2026-08-22 | Resolved fresh review r007. Made the Timer case explicitly wrap its joining parent in an outer Execution and limited event delivery to a same-thread reactor; removed the cycle-producing nested Scheduler-provider example and made nested join a dependent-library boundary; rewrote anonymous functions, named unions, affine parameter modes, scalar names, discards, Result propagation, and callback partial applications to current Silk source; gave TaskReservation an explicit uncommitted rollback lifecycle; removed impossible immediate-drive claims from non-effectful `onReady`; and defended Initial Execution as the owner-neutral mechanism that lets ordinary source choose first-activation order instead of forcing eager-to-first-park semantics. |
| 25 | 2026-08-22 | Applied the r008 author decision to retain the richer push-based, caller-funded substrate. Reframed its minimality around owner-controlled first activation, task-specific O(1) readiness, and recoverable package procurement; added the eager-start owner-sweep counterproposal; repaired the outer Deferred and Timer execution/failure boundaries; completed the first-activation and readiness-layout witnesses; and narrowed source sufficiency to closed leaf tasks pending a cycle-safe concurrency design. |
| 26 | 2026-08-22 | Resolved fresh four-lens review r009. Bound one reusable detached readiness endpoint to each Execution at recoverable construction instead of erasing a fresh callback and handing off Allocation on every drive; added one affine drive branch state; repaired Detached ownership and provider-provenance examples; made the Timer reactor and cancellation driver explicit; separated the three subtraction axes while defending one Scheduler-grade admission contract; and clarified static selection versus purpose-bound erased dispatch. |
| 27 | 2026-08-22 | Resolved r010 after the author retained one cohesive substrate thesis and tightened materiality to observable blockers or capability-preserving reductions. Collapsed execution and readiness storage into one exact caller-funded package, accepted whole-package retention behind a late cancelled Wake, gave statically non-parking wrappers a zero-sized no-op endpoint, made `park` return unit with compiler-owned `G` cleanup, normalized source/semantic task states, made drive callbacks return unit, and distinguished compile-time ownership diagnostics from runtime traps and valid cancelled-Wake consumption. |
| 28 | 2026-08-22 | Resolved fresh review r011. Removed `drive`'s unused result generic and made the sealed operation unit-returning; repaired the ordinary-source witness to current actor-local declaration, expression-match, literal, and exact-bound syntax; used nominal `TaskOutput` for homogeneous scheduling; inlined the future Coroutine body; corrected Timer Wake identity and local publication/O(1) wording; normalized lifecycle and cohesion terminology; specified wake-cell generation reuse and completion-loan rejection; and retained `O` plus `R(&O)` because current reusable callables cannot own affine detached state without becoming consuming. |
| 29 | 2026-08-22 | Resolved fresh review r012. Added the sealed `Intrinsic.NonParking` fact for runtime-invoked callbacks; selected the narrow exact-executable-plus-sealed-property conjunction rule required by generic wrappers; kept readiness Notifying and non-drivable until endpoint callback return; normalized the direct activation case and prose to `TaskOutput`; added the explicit Timer-driver trace; separated execution and cell/package coordination states; and retained the compiler-evidenced `O` plus `R(&O)` endpoint split. |
| 30 | 2026-08-22 | Resolved bounded four-lens review r013. Repaired the two zero-field `None {}` patterns to current Silk syntax; removed duplicated summary prose; described initial publication observationally rather than implying hardware atomicity; and clarified that a live Wake completes notification while only a still-live execution becomes Eligible. The scope, compiler, and dedicated simplification lenses found no material design blocker. |
| 31 | 2026-08-22 | Completed author resolution. Retained revision 30's Scheduler-grade capability point, accepted the non-blocking review batch, recorded SLP-0002 revision 6 as satisfying the shared-ownership dependency, delegated compiler representation, diagnostics, ordering, cleanup, and cross-engine conformance mechanics to OpenSpec, reconstructed the cross-round finding ledger, and recorded Accepted direction. |
