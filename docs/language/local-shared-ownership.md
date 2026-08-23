# Local shared ownership

Silk provides explicitly allocated, single-threaded shared ownership through the ordinary
standard-library `Shared<T>` actor. Several affine handles may keep one value and control-block
allocation alive. Access remains callback-scoped, and the handle that removes the last strong
obligation cleans the value and releases the allocation.

> **Proposal status:** Accepted direction. **Implementation status:** Complete.
>
> All six SLP-0002 OpenSpec slices are implemented and archived. The rules below describe the
> completed programmer-visible surface rather than the still-in-progress SLP-0001 execution work
> that motivated its first pressure case.

`Shared<T>` is local shared ownership, not thread-safe shared ownership. It adds no atomics, locks,
garbage collector, weak handles, cycle collector, Scheduler, Deferred, or implicit runtime.

## Terms

- **Strong handle** — one affine `Shared<T>` value that contributes one obligation to keep the
  control block alive.
- **Control block** — one stable caller-funded allocation containing the strong count, access
  state, reclaim authority, and stored `T`.
- **Local execution affinity** — the static fact that a value may move within one same-thread
  execution domain but is not transferable to another OS thread.
- **Access callback** — the ordinary take-once function passed to `Shared.with` or `Shared.withMut`.
  Its borrow exists only during that invocation.
- **Conflict** — an access attempt made while the same control block already has an active access
  callback.

## Public actor and construction

### LSHARED-001 — `silk.shared` provides the ordinary-source `Shared<T>` actor

**Status:** Confirmed

The canonical module `silk.shared` exports an affine `Shared<T>` handle and four operations with
these exact contracts:

```silk,ignore
pub effect fn make<T>(value: T) -> Shared<T>
! OutOfMemoryError
? &mut Allocator

pub fn clone<T>(self: &Shared<T>) -> Shared<T>
pub fn with<T, A>(self: &Shared<T>, use: once fn(&T) -> A) -> A
pub fn withMut<T, A>(self: &Shared<T>, use: once fn(&mut T) -> A) -> A
```

`Shared<T>` is an ordinary shipped-source actor over a sealed control-block primitive. The compiler
does not recognize `Shared`, Deferred, Scheduler, a ready inbox, or another policy actor by source
spelling. An equivalent safely written wrapper receives the same ownership behavior under another
name.

The handle remains affine for every `T`, including when `T` is Copy. Moving a handle transfers one
strong obligation; an ordinary read never duplicates it.

**Boundary:** `Shared<T>` does not make `T` Copy, make every capture implicitly shared, or replace
ordinary `&T` and `&mut T`. Source must construct and explicitly clone each independently owned
handle.

**Diagnostics:** An attempted non-consuming read that would duplicate a handle reports `OWN0003`.
Using a handle after a consuming move reports `OWN0001`. An attempted `impl Copy` for `Shared<T>` or
a nominal containing it reports `SEM0083`.

**Evidence:** [canonical Shared specification](../../openspec/specs/bootstrap-silk-stdlib/spec.md#requirement-shared-is-canonical-ordinary-silk-source),
[local shared ownership specification](../../openspec/specs/bootstrap-ownership/spec.md#requirement-every-local-shared-core-handle-is-one-affine-obligation),
[shipped Shared source](../../packages/compiler/stdlib/silk/shared.silk),
[Shared standard-library tests](../../packages/compiler/test/SharedStdlib.test.ts).

### LSHARED-002 — Construction allocates once through the selected `Allocator`

**Status:** Confirmed

`Shared.make` has this exact public contract:

```silk,ignore
effect fn Shared.make<T>(value: T) -> Shared<T>
! OutOfMemoryError
? &mut Allocator
```

Construction requests the exact target layout, allocates one control block through the explicitly
provided allocator, and transfers `value` into that block only after allocation succeeds. The
allocator loan ends before the returned handle escapes. The handle retains self-contained reclaim
authority rather than an allocator requirement. Successful initialization starts with strong count
one and available callback access.

The control-block header still requires distinct nonzero storage when `T` is zero-sized. Zero-sized
payload representation does not erase the strong count, access state, or reclaim authority.

If allocation fails, no handle or control block exists and ordinary typed-failure cleanup cleans
the still-owned `value` exactly once.

**Boundary:** Construction is the only storage boundary introduced by `Shared`. Cloning, access,
movement, suspension of an Effect that owns a handle, and dropping a non-last handle do not allocate
and acquire no `Allocator` requirement. A callback may independently perform operations that have
their own allocation contract.

**Diagnostics:** Exhaustion is the typed failure `OutOfMemoryError`, not a compiler diagnostic or
fatal trap. An unrepresentable control-block layout reports `SEM0093` at the reached layout
specialization before allocation, MIR, or runtime execution.

**Evidence:** [Shared construction contract](../../openspec/specs/bootstrap-silk-stdlib/spec.md#requirement-shared-is-canonical-ordinary-silk-source),
[caller-funded control blocks](../../openspec/specs/bootstrap-owned-allocation/spec.md#requirement-local-shared-control-blocks-use-exact-caller-funded-allocation),
[allocation acceptance tests](../../packages/compiler/test/OwnedAllocationAcceptance.test.ts).

## Cloning and cleanup

### LSHARED-003 — Explicit clone adds one strong obligation without touching `T`

**Status:** Confirmed

`Shared.clone(&handle)` synchronously creates one additional affine handle to the same control block.
It allocates nothing, invokes no user code, and does not read, copy, move, or clean `T`.

```silk,ignore
let first = run Shared.make(Token.make())
let second = Shared.clone(&first)

drop first
Token.observeStillLive()
drop second
```

The strong count and callback-access state are independent. Cloning through another live alias, or
dropping a non-last alias, is valid while an access callback is active because neither operation
forms another reference to `T` or changes the active access state.

**Boundary:** Clone is infallible and has no Effect, failure, or requirement channel. If the target's
bounded strong count is already at its maximum, cloning traps before mutating the count and before
producing a partial handle. The count never wraps or saturates.

**Diagnostics:** Valid clone has no diagnostic. Count exhaustion is a defined fatal trap and cannot
be recovered through typed Effect failure handling.

**Evidence:** [Shared clone contract](../../openspec/specs/bootstrap-silk-stdlib/spec.md#requirement-shared-is-canonical-ordinary-silk-source),
[local shared lifecycle primitive](../../openspec/specs/bootstrap-intrinsic-boundary/spec.md#requirement-two-sealed-primitives-govern-local-shared-lifecycle),
[Shared standard-library tests](../../packages/compiler/test/SharedStdlib.test.ts).

### LSHARED-004 — The last structured drop cleans `T` and releases storage exactly once

**Status:** Confirmed

Dropping a non-last handle decrements the strong count and preserves both `T` and the control-block
allocation. The structured drop that changes the count from one to zero gains the sole cleanup
authority, cleans `T` exactly once, and then releases the allocation exactly once.

```silk,ignore
let first = run Shared.make(Token.make())
let second = Shared.clone(&first)

drop first  // Preserves Token and the allocation.
drop second // Cleans Token, then releases the allocation.
```

This remains true when handles occupy different aggregates, callable or Effect environments, or
typed-failure frames. Each frame drops only its own handle. Whichever drop reaches zero performs the
one payload cleanup without replacing the propagated failure.

The canonical `Shared<T>` actor declares no source `Drop` hook. Ordinary recursive cleanup of its
sole opaque `SharedCore<T>` field performs the decrement-or-last-cleanup transition.

**Boundary:** A fatal trap retains Silk's general no-unwind rule. The language does not promise that
live handles, payloads, or allocations are cleaned after a trap. Last-handle cleanup is guaranteed
for ordinary structured success and typed-failure cleanup paths.

**Diagnostics:** Structured handle cleanup produces no diagnostic. Duplicate use or movement uses
ordinary affine ownership diagnostics.

**Evidence:** [dynamic cleanup authority](../../openspec/specs/bootstrap-ownership/spec.md#requirement-strong-handle-transitions-preserve-one-dynamic-cleanup-authority),
[typed-failure cleanup](typed-failures.md#fail-006--typed-failure-applies-ordinary-cleanup-and-preserves-diagnostic-context),
[local shared pressure tests](../../packages/compiler/test/LocalSharedPressure.test.ts).

## Callback-scoped access

### LSHARED-005 — `with` and `withMut` confine borrows to one ordinary callback

**Status:** Confirmed

`Shared.with` invokes a take-once ordinary callback with `&T`. `Shared.withMut` invokes one with
`&mut T`. Both return only after the callback ends and its borrow has ended:

```silk,ignore
let counter = run Shared.make(Counter.make(1))

let before = Shared.with(&counter, fn(value) {
  return Counter.value(value)
})

Shared.withMut(&counter, fn(value) {
  Counter.increment(value)
  return ()
})
```

The callback may return owned data and may use ordinary operations to move affine values into or
out of state behind `&mut T`. Access does not make those values Copy.

The callback borrow cannot be returned directly, hidden inside a generic result or aggregate, used
as a failure value, captured by an Effect or stored callable, or retained across suspension.

**Boundary:** The callbacks are ordinary functions, not Effect-returning functions. Returning an
Effect that captures the borrowed parameter is invalid even when the Effect is never run.

**Diagnostics:** Every direct, recursive, executable, or suspension escape reports `OWN0016`. The
diagnostic identifies both the attempted escape or suspension and the local shared access boundary.

**Evidence:** [callback-scoped access specification](../../openspec/specs/bootstrap-ownership/spec.md#requirement-local-shared-access-borrows-are-callback-scoped-and-non-escaping),
[public wrapper specification](../../openspec/specs/bootstrap-silk-stdlib/spec.md#requirement-shared-is-canonical-ordinary-silk-source),
[access escape tests](../../packages/compiler/test/SharedStdlib.test.ts).

### LSHARED-006 — Every reentrant access combination conflicts

**Status:** Confirmed

The first version has one exclusive runtime access state. `Shared.with` narrows the exclusive borrow
to `&T`; it does not create a separate reader state. Reentrant access through any alias therefore
follows one matrix:

| Active access | Nested access | Result |
| --- | --- | --- |
| `with` | `with` | fatal conflict trap |
| `with` | `withMut` | fatal conflict trap |
| `withMut` | `with` | fatal conflict trap |
| `withMut` | `withMut` | fatal conflict trap |

The nested operation detects conflict before forming another reference or invoking its access
callback. Observing a conflict does not change or release the outer active state.

The sealed access operation invokes exactly one of its two take-once callbacks. After the selected
callback returns normally, the unselected callback environment receives ordinary cleanup exactly
once. On successful access, the callback borrow ends and availability is restored before that
cleanup and before the operation returns. A normally returning conflict callback leaves the outer
active access unchanged while the unselected access callback is cleaned.

Ordinary stateful actors should extract callbacks or work items during a short `withMut` call, let
that call return, and only then invoke external code.

**Boundary:** This rule intentionally rejects nested read-only access. `Shared<T>` provides neither
reader counts nor a `RefCell`-style recoverable borrow error. The sealed primitive exposes conflict
through a callback, but the canonical public wrapper selects a fatal trap as ordinary source policy.

**Diagnostics:** Reentrant access is a runtime trap, not a static diagnostic or typed failure. No
post-trap cleanup is promised.

**Evidence:** [public reentrancy contract](../../openspec/specs/bootstrap-silk-stdlib/spec.md#requirement-shared-is-canonical-ordinary-silk-source),
[lifecycle operation specification](../../openspec/specs/bootstrap-intrinsic-boundary/spec.md#requirement-two-sealed-primitives-govern-local-shared-lifecycle),
[cross-engine parity specification](../../openspec/specs/bootstrap-backend/spec.md#requirement-native-and-wasm-realize-local-shared-ownership-identically).

## Locality, cycles, and runtime boundary

### LSHARED-007 — Shared handles are local to one same-thread execution domain

**Status:** Confirmed

`Shared<T>` and every aggregate or executable that contains it have `LocalExecution` affinity. A
handle may move between ordinary frames, be captured by a callable or Effect, survive ordinary
Effect suspension, and be retained by dormant unrun local work.

```silk,ignore
struct Observer {
  state: Shared.Shared<Counter>
}

let state = run Shared.make(Counter.make(0))
let observer = Observer { state: Shared.clone(&state) }
```

SLP-0002 also establishes the compatibility rule that parking, resumption, or movement between
independently resumable frames in one same-thread execution domain does not create or discharge a
strong obligation. This is a guarantee consumed by SLP-0001; the public independent-execution and
parking surface remains in progress under that proposal. After construction, only explicit clone
and handle drop change the strong count.

**Boundary:** A local handle, a reference rooted in it, or an executable that captures it cannot be
transferred to another OS thread. SLP-0002 defines no thread-transfer syntax, atomic reference count,
lock, memory ordering, data-race model, or thread-safe conversion. A future parallel-memory model
must consume the existing affinity fact without adding atomic cost to local programs.

**Diagnostics:** The current language publishes local affinity before any cross-thread transfer
surface exists, so this rule introduces no standalone transfer diagnostic. A future
parallel-transfer proposal may consume this fact and must define its own rejection behavior.

**Evidence:** [local execution affinity](../../openspec/specs/bootstrap-semantic-facts/spec.md#requirement-semantic-facts-retain-local-shared-ownership-and-execution-affinity),
[affine handle ownership](../../openspec/specs/bootstrap-ownership/spec.md#requirement-every-local-shared-core-handle-is-one-affine-obligation),
[semantic and standard-library tests](../../packages/compiler/test/SharedStdlib.test.ts).

### LSHARED-008 — Strong cycles leak and no collector runs implicitly

**Status:** Confirmed

Strong-reference cycles are retained when their external handles are dropped because no strong count
reaches zero. Their payloads are not cleaned and their control-block allocations are not released.
This leak is specified behavior.

```silk,ignore
let left = run Shared.make(Node.empty())
let right = run Shared.make(Node.empty())
Node.link(&left, Shared.clone(&right))
Node.link(&right, Shared.clone(&left))
drop left
drop right // The two internal strong handles keep the cycle allocated.
```

Every acyclic graph whose handles leave through structured paths still reaches exact last-handle
cleanup. Evaluator, native, and direct WebAssembly agree on successful access, conflict selection,
strong-count transitions, payload cleanup, and allocation release order even when their physical
control-block layouts differ.

**Boundary:** `Shared<T>` supplies no Weak handle, tracing garbage collector, cycle collector,
implicit cycle detection, identity operation, background work, atomics, locks, or scheduler. Source
must avoid strong cycles or break them explicitly using its own state model.

**Diagnostics and audit:** Constructing a cycle is valid and emits no diagnostic. Runtime and
artifact verification reject hidden allocation during clone or access and unrelated collector,
scheduler, lock, atomic, or background support.

**Evidence:** [dynamic cleanup authority](../../openspec/specs/bootstrap-ownership/spec.md#requirement-strong-handle-transitions-preserve-one-dynamic-cleanup-authority),
[engine parity](../../openspec/specs/bootstrap-backend/spec.md#requirement-native-and-wasm-realize-local-shared-ownership-identically),
[local shared runtime evidence](../../packages/compiler/test/LocalSharedPressure.test.ts),
[accepted SLP-0002](../../proposals/0002-allocation-backed-local-shared-ownership/proposal.md).

## SLP-0001 shared-state sufficiency boundary

### LSHARED-009 — Local shared ownership removes the shared-state wall, not the execution wall

**Status:** Confirmed

Ordinary Silk can use cloned `Shared<T>` handles to build the shared-state portion of a ready inbox
or Deferred-style actor. Stored callbacks and dormant unrun Effects may each own a handle without
retaining one exclusive lexical borrow for their complete lifetime:

```silk,ignore
let state = run Shared.make(DeferredState.pending<Payload>())
let waiterState = Shared.clone(&state)

let dormant = effect {
  return Shared.with(&waiterState, fn(current) {
    return DeferredState.observe(current)
  })
}
```

Registration, publication, and enqueue operations use short `Shared.withMut` calls. Publication
moves the offered affine value into shared state once, extracts readiness callbacks under the
mutation callback, lets access restore, and only then invokes those callbacks. Dropping a dormant
Effect or stored callback decrements its captured handle without releasing state retained by other
handles. Dropping the last unpublished state cleans its affine payload and unconsumed callbacks
exactly once.

Construction failure at any exercised `Shared.make` allocation creates no partial inbox or state,
cleans still-owned constructor inputs, balances every earlier acquisition and release, and preserves
the ordinary `OutOfMemoryError` failure.

**Boundary:** This sufficiency result does not define a canonical Deferred, ready inbox, producer,
waiter, callback registry, Scheduler, or execution actor. It proves no execution transfer, parking,
wake selection or ordering, cancellation, fairness, or scheduler integration. Those remain SLP-0001
work.

**Diagnostics:** The witness requires no additional language diagnostic or compiler-known actor.
Its operations use the ordinary allocation, ownership, callback-escape, and Effect diagnostics
defined by the preceding rules.

**Evidence:** [local shared pressure specification](../../openspec/specs/bootstrap-language-pressure-programs/spec.md#requirement-local-shared-ownership-removes-the-slp-0001-shared-state-wall),
[pressure program](../../examples/language-pressure/local-shared-slp1/README.md),
[pressure findings](../../examples/language-pressure/local-shared-slp1/findings.md),
[pressure tests](../../packages/compiler/test/LocalSharedPressure.test.ts).

## Sealed implementation boundary

The irreducible compiler surface is one opaque `Intrinsic.SharedCore<T>` and four operations:

```silk,ignore
fn Intrinsic.sharedLayout<T>() -> Layout

unsafe fn Intrinsic.sharedFromAllocation<T>(
  allocation: Allocation,
  value: T
) -> Intrinsic.SharedCore<T>

fn Intrinsic.sharedClone<T>(
  self: &Intrinsic.SharedCore<T>
) -> Intrinsic.SharedCore<T>

fn Intrinsic.sharedWithMut<T, A>(
  self: &Intrinsic.SharedCore<T>,
  use: once fn(&mut T) -> A,
  onConflict: once fn() -> A
) -> A
```

Only `sharedFromAllocation` is unsafe because source must supply a live allocation with exact layout
provenance and transfer `value` exactly once. The public actor keeps allocation policy, failure
types, conflict policy, shared-borrow narrowing, and reusable state abstractions in ordinary Silk.
`sharedWithMut` invokes exactly one callback. After normal return it cleans the unselected callback
environment exactly once; successful access restores availability before that cleanup and return.
See [INTR-006](unsafe-intrinsics-and-targets.md#intr-006--local-shared-privilege-stops-at-control-block-mechanics).
