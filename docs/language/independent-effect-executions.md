# Independently resumable Effect executions

> **Proposal status:** Accepted direction. **Implementation status:** In progress.
>
> This page records the confirmed SLP-0001 language direction while its OpenSpec slices are being
> implemented. A rule's **Confirmed** status means that its intended programmer-visible semantics
> are confirmed; it does not mean that every linked implementation slice has shipped.

An independently resumable Effect execution is an explicit affine owner for one closed Effect.
Ordinary source can construct it without starting its body, drive it until completion or external
parking, retain it while dormant, make it eligible through a one-shot `Intrinsic.Wake`, and drive it
again in owner-selected order.

This capability is an owner-neutral language substrate. `Scheduler`, `Fiber`, `Deferred`, timers,
ready queues, and coroutines remain ordinary-source policy. SLP-0001 does not select canonical
public APIs for those actors.

The selected sealed surface contains two opaque affine types, two compiler-owned static
properties, and five operations:

```text
Intrinsic.Execution<A>
Intrinsic.Wake

Intrinsic.Detached
Intrinsic.NonParking

Intrinsic.executionLayout<A, F, O, R>() -> Layout
Intrinsic.executionFromAllocation<A, F, O, R>(allocation, body, readyState, onReady)
Intrinsic.drive<A, D, C, S>(execution, branchState, onComplete, onSuspend)
Intrinsic.wake(wake)
Intrinsic.park<G, F>(register)
```

`Execution` and `Wake` are initially local and non-thread-transferable. The compiler recognizes no
source owner or policy actor by spelling.

## Terms

- **Execution** — one opaque affine owner for an unstarted, running, dormant, notifying, or eligible
  Effect execution.
- **Initial** — constructed but never driven.
- **Running** — active inside one `drive` call. No source owner has an `Execution` value in this
  state.
- **Dormant** — externally parked and retained by its source owner. It is not legal to drive yet.
- **Notifying** — a live Wake is invoking the fixed readiness endpoint. It is not legal to drive.
- **Eligible** — readiness notification has returned and the owner may select the Execution for a
  later drive.
- **Endpoint** — detached state `O` and one reusable non-parking callback `R: fn(&O) -> ()` fixed at
  construction.
- **Park generation** — one call to `Intrinsic.park`, its registration guard, and its sole Wake.

## Construction and ownership

### IEXEC-001 — Explicit construction creates one owner-neutral affine lifecycle

**Status:** Confirmed

`Intrinsic.Execution<A>` is opaque, affine, non-Copy, and initially non-thread-transferable. A valid
construction produces one Initial Execution and does not run the Effect body. Its ordinary-source
owner chooses the first activation and every later eligible activation.

```silk,ignore
let first = run Execution.make(effect { return 1 }, ReadyState {}, publishReady)
let second = run Execution.make(effect { return 2 }, ReadyState {}, publishReady)

// The owner can drive `second` before `first`.
run Owner.driveOne(move second)
run Owner.driveOne(move first)
```

The body of `first` performs no source operation before its first drive. Dropping it while Initial
cleans its body and endpoint exactly once without invoking the body or a drive callback.

The lifecycle is owner-neutral. A Scheduler-shaped owner and a Coroutine-shaped owner use the same
construction, drive, parking, readiness, completion, and drop rules. Neither name has compiler
meaning.

**Boundary:** An Execution cannot be cloned, re-entered, driven concurrently, driven after
completion, or used after movement. Cross-thread transfer remains unavailable until a separate
parallel-memory contract defines transfer and ordering.

**Diagnostics:** Duplicate use receives the ordinary affine use-after-move diagnostic. An attempted
local-domain transfer receives the canonical local-affinity diagnostic. Stable codes for the new
execution-specific obligations are not yet assigned.

**Evidence:** [SLP-0001 lifecycle](../../proposals/0001-independently-resumable-effect-executions/proposal.md#proposed-language-model),
[independent-execution semantics](../../openspec/changes/establish-independent-execution-semantics/specs/bootstrap-independent-execution-semantics/spec.md),
[ownership requirements](../../openspec/changes/establish-independent-execution-semantics/specs/bootstrap-ownership/spec.md).

### IEXEC-002 — `Intrinsic.Detached` proves ownership of the complete retained environment

**Status:** Confirmed

An exact value, callable, or Effect representation satisfies `Intrinsic.Detached` only when it owns
everything required for later invocation and cleanup. It must retain no caller lexical loan and no
borrowed provider.

```silk,ignore
let owned = Token.make()
let valid = effect {
  drop move owned
  return 42
}

let execution = run Execution.make(move valid, ReadyState {}, publishReady)
```

An empty Effect requirement row does not prove detachment. Requirement elimination and provider
ownership are separate facts:

```silk,ignore
let borrowed = readClock()
  |> Effect.bindRequirement<Clock>(&clock)

let invalid = run Execution.make(move borrowed, ReadyState {}, publishReady)
// Rejected: the Effect is closed but retains the provider loan.
```

Values created after activation may retain internal loans across parking when their referents are
owned by the same Execution and remain at stable logical locations. Cleanup ends each internal loan
before its owned referent.

**Boundary:** `Detached` is a sealed static property, not an interface, runtime witness, lifetime
extension, Copy proof, or thread-transfer permission. Source cannot implement it or acquire it by
declaring a same-named actor.

**Diagnostics:** A failed obligation is reported at the generic bound or application and preserves
the complete capture, nested-field, or provider-loan cause. It remains distinct from an unsatisfied
Effect requirement-row diagnostic. A completion result that borrows package-owned state is rejected
before construction or drive can erase its provenance. Stable codes are not yet assigned.

**Evidence:** [detachment semantics](../../openspec/changes/establish-independent-execution-semantics/specs/bootstrap-semantic-facts/spec.md),
[independent-execution ownership](../../openspec/changes/establish-independent-execution-semantics/specs/bootstrap-ownership/spec.md),
[exact executable property bounds](generics-interfaces-and-specialization.md#rep-007--exact-executable-bounds-may-add-sealed-static-properties).

### IEXEC-003 — Construction consumes one caller-funded exact package

**Status:** Confirmed

For concrete `A`, body representation `F`, endpoint state `O`, and endpoint callback `R`,
`Intrinsic.executionLayout<A, F, O, R>()` returns the exact target `Layout` for one indivisible
Execution package. `Intrinsic.executionFromAllocation` consumes one matching active `Allocation`
plus `F`, `O`, and `R`, runs no body code, and returns one Initial `Execution<A>`.

An ordinary safe wrapper performs allocation before the unsafe initializer:

```silk,ignore
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

The package owns the erased body, exact invoke and cleanup metadata, fixed endpoint, applicable wake
state, and applicable initial continuation storage. A non-parking specialization can supply a
zero-sized endpoint and contains no active wake cell or readiness state.

**Boundary:** `executionFromAllocation` requires allocation provenance that matches the concrete
types, target, size, alignment, layout, and suspension summary. It creates no second allocation and
does not recognize `Allocator` or `OutOfMemoryError`. Those belong to the source wrapper.

**Diagnostics:** Layout overflow reports the canonical layout diagnostic. A mismatched allocation
or unproven unsafe precondition is rejected before publication. Source allocation failure remains
the wrapper's declared typed failure and creates no partial Execution.

**Evidence:** [execution packaging requirements](../../openspec/changes/add-independent-execution-packaging/specs/bootstrap-independent-execution-packaging/spec.md),
[target layout requirements](../../openspec/changes/add-independent-execution-packaging/specs/bootstrap-target-layout/spec.md),
[allocation authority](../../openspec/changes/add-independent-execution-packaging/specs/bootstrap-owned-allocation/spec.md).

## Driving and completion

### IEXEC-004 — `drive` transfers one affine branch state to exactly one outcome

**Status:** Confirmed

`Intrinsic.drive` consumes an Initial or Eligible Execution, one branch state `D`, and two take-once
NonParking callbacks. Completion calls only `onComplete(D, A)`. External parking calls only
`onSuspend(D, Execution<A>)`. The unused callback is cleaned exactly once, and `drive` returns `()`.

```silk,ignore
fn completed(branch: OwnerBranch, value: i32) -> () {
  Owner.complete(move branch, value)
}

fn suspended(
  branch: OwnerBranch,
  execution: Intrinsic.Execution<i32>
) -> () {
  Owner.retain(move branch, move execution)
}

run Intrinsic.drive(
  move execution,
  move branch,
  completed,
  suspended
)
```

The branch state can own a non-Copy lease because exactly one callback receives it. Completion does
not return an Execution, so a later drive is unrepresentable. Suspension returns the same Execution
obligation through `onSuspend`.

Nested `Effect.suspend` remains inside the current activation. A direct child's completion resumes
its parent without invoking the owner's suspension callback. Only external parking relinquishes to
`onSuspend`.

Each Execution owns one logical stack rooted at its first drive. Later drives restore that root;
owner and scheduling frames are not logical ancestors of the driven Effect.

**Boundary:** Driving a Dormant or Notifying Execution is a defined fatal intrinsic-state trap
before either callback runs. A callback used by `drive` cannot satisfy `Intrinsic.NonParking` when
its specialized graph can reach `Intrinsic.park`.

**Diagnostics:** Invalid source reuse reports ordinary ownership diagnostics. A failed NonParking
obligation reports its transitive park path at the failed bound or application. Illegal runtime
state traps under the language-wide no-unwind rule.

**Evidence:** [drive contract](../../openspec/changes/add-independent-execution-packaging/specs/bootstrap-independent-execution-packaging/spec.md),
[packaging ownership matrix](../../openspec/changes/add-independent-execution-packaging/specs/bootstrap-ownership/spec.md),
[logical execution roots](../../openspec/changes/add-independent-execution-engine-parity/specs/bootstrap-independent-execution-engine-parity/spec.md).

## External parking and readiness

### PARK-001 — `park` registers readiness before relinquishment

**Status:** Confirmed

`Intrinsic.park` is valid only while an explicit Execution is Running. It calls one take-once
NonParking registration function with the park generation's sole Wake. The callback stores or
consumes that Wake and returns one ordinary guard `G`.

```silk,ignore
fn register(wake: Intrinsic.Wake) -> WaiterGuard {
  return WaiterList.insert(move wake)
}

effect fn awaitReady() -> () {
  run Intrinsic.park(register)
  return ()
}
```

The runtime retains `G` and all live frame values before `drive` transfers the Dormant Execution to
`onSuspend`. Source after `park` does not continue until a Wake makes the Execution eligible and a
later legal drive resumes it. Immediately before `park` returns `()`, the runtime drops `G` exactly
once so source can recheck its durable condition.

Parking, registration, and relinquishment do not allocate and do not consult scheduler policy.

**Boundary:** `park` outside explicit Execution ownership is invalid. Registration callbacks cannot
park. `park` does not return a pending value, Execution, Wake, payload, or scheduler token.

**Diagnostics:** A park-capable unowned entry receives the missing-execution-owner diagnostic
defined by [ENTRY-006](program-entry.md#entry-006--external-parking-requires-an-explicit-root-owner).
A parking registration callback fails its NonParking obligation before execution. Stable codes are
not yet assigned.

**Evidence:** [parking registration requirements](../../openspec/changes/add-external-wake-parking/specs/bootstrap-external-wake-parking/spec.md),
[parking intrinsic boundary](../../openspec/changes/add-external-wake-parking/specs/bootstrap-intrinsic-boundary/spec.md).

### WAKE-001 — Wake signals readiness once and never resumes inline

**Status:** Confirmed

`Intrinsic.Wake` is opaque, affine, fixed-layout, payload-free, and initially
non-thread-transferable. `Intrinsic.wake` consumes it. A live Wake begins at most one readiness
notification for its park generation; it never runs continuation code or drives the Execution
inline.

```silk,ignore
fn signal(wake: Intrinsic.Wake) -> () {
  Intrinsic.wake(move wake)
}
```

If `wake` runs during registration, readiness becomes Latched. The complete suspension callback
first receives and stores the Execution. Only after that callback returns may the fixed endpoint
run. The Execution relinquishes exactly once even when readiness was already latched.

If `wake` runs after dormancy, the runtime enters Notifying, finishes wake-cell mutation, and calls
the fixed endpoint once. The endpoint can publish ordinary-source readiness. Only after it returns
does a still-live Execution become Eligible for a later owner-selected drive.

**Boundary:** Wake carries no success value, failure value, task identity, callback representation,
timer payload, or scheduler token. Source-owned state carries those values. Moving Wake to another
thread remains invalid in the local model.

**Diagnostics:** A second call is ordinary use-after-move because the first call consumed Wake.
Cross-thread movement receives the local-affinity diagnostic. Indirectly driving the Notifying
Execution traps before progress or callback invocation.

**Evidence:** [wake ordering requirements](../../openspec/changes/add-external-wake-parking/specs/bootstrap-external-wake-parking/spec.md),
[wake ownership](../../openspec/changes/add-external-wake-parking/specs/bootstrap-ownership/spec.md).

### WAKE-002 — Execution destruction cancels readiness before cleanup

**Status:** Confirmed

Dropping a Dormant Execution first marks its current wake generation Cancelled. Cleanup then drops
the registration guard, endpoint state and callback, body, continuation frames, and every retained
owned value exactly once. Internal loans end before their referents.

An external Wake retained after destruction remains valid to consume or drop. It performs no
notification and touches no cleaned value. Because the Execution package is one indivisible
Allocation, that Wake retains the complete package as inert cancelled-cell storage until the last
Wake or transient authority ends.

```silk,ignore
drop execution
Intrinsic.wake(move lateWake) // valid consuming no-op
```

If endpoint code reentrantly destroys a Notifying Execution, destruction records DestroyPending.
It keeps borrowed endpoint state and package storage alive until the endpoint returns, then cleans
instead of making the Execution Eligible.

**Boundary:** The registration guard supports prompt source-resource unlinking but is not trusted
for runtime memory safety. Forgetting a cancelled Wake cleans all package values but retains the
complete inert Allocation. No source-callable intrinsic cancel or destroy operation exists;
ordinary affine drop supplies destruction.

**Diagnostics:** Consuming or dropping a cancelled Wake is valid and silent. Using the Wake again is
ordinary use-after-move. Fatal state traps retain the no-unwind cleanup rule.

**Evidence:** [cancellation and reclamation requirements](../../openspec/changes/add-external-wake-parking/specs/bootstrap-external-wake-parking/spec.md),
[parking ownership](../../openspec/changes/add-external-wake-parking/specs/bootstrap-ownership/spec.md),
[one-allocation authority](../../openspec/changes/add-independent-execution-packaging/specs/bootstrap-owned-allocation/spec.md).

## Static properties, failures, and cost

### IEXEC-005 — `Intrinsic.NonParking` excludes only external parking

**Status:** Confirmed

An exact callable satisfies `Intrinsic.NonParking` when its specialized transitive call graph cannot
reach `Intrinsic.park`. It may perform direct work and may use nested `Effect.suspend`.

Runtime-invoked registration, endpoint, completion, and suspension callbacks require this property.
It prevents a callback from recursively relinquishing the execution while the runtime is completing
another lifecycle transition.

**Boundary:** Callable syntax alone does not establish NonParking because an ordinary callable can
run a closed Effect. The property is sealed, static, and re-evaluated for each concrete
specialization. It is not an Effect channel, interface, runtime marker, or promise that the callback
cannot run nested suspension.

**Diagnostics:** A failed obligation reports one deterministic diagnostic at the bound or
application and retains the specialized transitive path to `Intrinsic.park`. Stable codes are not
yet assigned.

**Evidence:** [NonParking semantic facts](../../openspec/changes/establish-independent-execution-semantics/specs/bootstrap-semantic-facts/spec.md),
[exact executable property bounds](generics-interfaces-and-specialization.md#rep-007--exact-executable-bounds-may-add-sealed-static-properties).

### IEXEC-006 — Pending, packaging failure, and stack exhaustion remain distinct outcomes

**Status:** Confirmed

External parking is visible only to the source owner through `onSuspend`. It does not add Pending to
the Effect success type or failure channel. Typed failures remain ordinary Effect failures unless
source reifies them before construction:

```silk,ignore
let execution = run Execution.make(
  Effect.result(move body),
  move readyState,
  move onReady
)
```

This Execution completes with ordinary `Result<A, E>` data. The intrinsic execution lifecycle has
no typed failure channel.

The safe source wrapper exposes allocation failure and the Allocator requirement while procuring
the initial exact package. After construction, dynamic continuation-stack growth remains private
runtime policy. Exhaustion is a fatal trap and cannot be recovered with Effect failure handlers.

**Boundary:** Parking does not allocate, waking does not allocate, and neither operation adds an
Allocator requirement. Source task state, result state, queues, waiter nodes, and owner policy have
their own ordinary allocation contracts.

**Diagnostics:** A wrapper's procurement failure uses its declared typed error. Post-construction
execution-stack exhaustion reports a fatal runtime trap. No Pending, allocation error, or
continuation-growth failure is inferred into the body Effect.

**Evidence:** [execution storage contract](../../proposals/0001-independently-resumable-effect-executions/proposal.md#execution-storage-and-exhaustion),
[package cleanup and growth](../../openspec/changes/add-independent-execution-packaging/specs/bootstrap-independent-execution-packaging/spec.md).

### IEXEC-007 — Static reachability selects direct, nested, and independent execution costs

**Status:** Confirmed

Every complete specialization has one deterministic suspension summary. The compiler selects
machinery from exact reachable behavior, not from Effect syntax or source actor names.

| Ownership and reachable suspension | Required machinery |
| --- | --- |
| ordinary `run`; none | direct lowering |
| ordinary `run`; nested `Effect.suspend` only | existing nested/LIFO suspension |
| explicit Execution; none | owned erased body and exact invoke/cleanup metadata |
| explicit Execution; nested only | owned package plus nested frames during a drive |
| explicit Execution; external `park` | owned package, fixed endpoint, wake control, and dormant continuation |

Execution construction is a propagation delimiter. External parking reachable inside the owned body
selects that Execution's independent tier. Calling `drive` does not make the ordinary source owner
park-capable merely because its body can park.

Before complete specialization, an open generic conservatively retains every suspension mode its
declared contract permits. Each reachable concrete specialization then receives one static summary.

**Boundary:** A runtime branch that does not park still pays the external-parking tier when parking
remains statically reachable. Importing or naming a Scheduler, Fiber, Deferred, timer, or Coroutine
selects no machinery. An explicit non-suspending Execution still owns an erased droppable body and
is not representation-equivalent to ordinary direct `run`.

**Diagnostics:** Static tier selection produces no source diagnostic. Artifact verification checks
that unreachable suspension, Wake, scheduler, and atomic machinery is absent. Unsupported reachable
intrinsics receive the ordinary target-availability diagnostic.

**Evidence:** [static suspension modes](../../openspec/changes/establish-independent-execution-semantics/specs/bootstrap-independent-execution-semantics/spec.md),
[pay-for-use pressure requirements](../../openspec/changes/prove-independent-execution-separation/specs/bootstrap-independent-execution-pressure/spec.md),
[runtime pay-for-use](runtime-and-standard-library.md#runtime-006--suspension-and-execution-machinery-is-selected-statically).

## Deliberate boundaries

SLP-0001 does not define implicit ownership for a park-capable program entry. See
[ENTRY-006](program-entry.md#entry-006--external-parking-requires-an-explicit-root-owner).

It also does not select canonical Scheduler, Fiber, Deferred, timer, Coroutine, structured
concurrency, cancellation, fairness, parallel execution, or host-event-loop APIs. Those abstractions
remain ordinary source above this substrate and require separate language or library decisions for
their programmer-visible contracts.
