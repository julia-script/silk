# SLP-0002: Allocation-backed local shared ownership

SLP: 0002
Status: Candidate
Revision: 5
Author: Julia Ortiz
Created: 2026-08-21
Updated: 2026-08-21
Discussion: —
Review record: [r001](reviews/r001.md)
Depends on: [values and types](../../docs/language/values-and-types.md), [generics, interfaces, and specialization](../../docs/language/generics-interfaces-and-specialization.md), [unsafe code, intrinsics, and targets](../../docs/language/unsafe-intrinsics-and-targets.md), [runtime and standard-library boundary](../../docs/language/runtime-and-standard-library.md)
Split from: SLP-0001
Split into: —
Supersedes: —
Superseded by: —
Revisit when: —
Resolution: —
OpenSpec handoff: —

## Summary

Silk should support explicitly allocated, single-threaded shared ownership through an ordinary
standard-library `Shared<T>` actor over the smallest sealed target-neutral control-block
primitive. Cloning a handle should share one stable allocation without reallocating; dropping the
last handle should clean `T` and release that allocation exactly once. `Shared.with` and
`Shared.withMut` should confine borrowed access to ordinary non-Effect callbacks so no borrow can
escape or remain held across suspension. The first version should realize both through one exclusive
access primitive and reject every reentrant access conflict. Strong-count overflow is a fatal trap
before mutation; an infallible clone can neither wrap the count nor return a partial handle.

Construction is an honest allocation boundary and therefore may carry the selected allocator's
failure and requirement channels. Merely suspending an Effect, returning a value, cloning an existing
handle, or accessing its value does not acquire an allocator dependency. The local handle is not
transferable between threads and earns no atomics, locks, garbage collector, weak reference, cycle
collector, implicit global runtime, or compiler-known Deferred or Scheduler.

This capability is independently useful, but SLP-0001 supplies its first decisive pressure case:
source-defined Deferred state and ready-inbox callbacks need several dormant computations to retain
access to the same mutable state without retaining one exclusive lexical borrow. SLP-0001 owns
execution transfer and parking; this proposal owns only the local shared-lifetime and access model.

## Problem and evidence

Silk currently has affine owners, shared lexical borrows, and exclusive lexical borrows. These rules
prevent aliasing correctly:

```silk,ignore
let mut inbox = ReadyInbox.make()
let first = effect { ReadyInbox.push(&mut inbox, 1) }
let second = effect { ReadyInbox.push(&mut inbox, 2) }
// Rejected: two escaping Effect environments cannot retain the same exclusive borrow.
```

Changing the captures to `&inbox` does not help because ordinary shared access cannot mutate the
inbox. Moving it into one Effect prevents the other Effect from using it. The fact that a future
Scheduler runs these Effects one at a time is not a static ownership proof: both dormant
environments coexist for an unknown duration.

SLP-0001 exposes the same wall after the control-flow problem is solved. A parked execution stores a
one-shot readiness callback. Calling that callback must notify a ready inbox shared with the owner,
while Deferred producers and waiters must share one value-and-waiter state. Neither operation
requires parallel execution, but both require several independently owned handles to one mutable
allocation.

The current allocation vocabulary cannot build that actor in ordinary source. `Allocation` and
`RawBuffer<T>` are affine. Mutation and `Slot<T>` projection require `&mut RawBuffer<T>`, and no
operation can derive independently owned aliases, count them, or give the last one sole cleanup
authority. Historical allocation design explicitly records that Silk has not designed safe shared
interior mutation.

## Driving examples: current and desired

### Case: Share one ready inbox between dormant callbacks

#### Intent

Create two independently retained callbacks that can enqueue identifiers into the same local inbox
without retaining `&mut ReadyInbox` for their complete lifetimes.

#### Current Silk

```silk,ignore
let mut inbox = ReadyInbox.make()

let first = effect {
  ReadyInbox.push(&mut inbox, 1)
}
let second = effect {
  ReadyInbox.push(&mut inbox, 2)
}
```

The second capture conflicts with the exclusive borrow already retained by `first`. A shared borrow
would be read-only.

#### Desired Silk

```silk,ignore
// The fixed-capacity inbox performs no allocation in `withMut`.
let inbox = run Shared.make(ReadyInbox.make())
let firstInbox = Shared.clone(&inbox)
let secondInbox = Shared.clone(&inbox)

let first = effect {
  return Shared.withMut(&firstInbox, fn(state) {
    ReadyInbox.pushNoGrow(state, 1)
    return ()
  })
}

let second = effect {
  return Shared.withMut(&secondInbox, fn(state) {
    ReadyInbox.pushNoGrow(state, 2)
    return ()
  })
}
```

`make` allocates one stable control block. Each explicit clone increments its local ownership count
without allocating. Each `withMut` obtains exclusive access only for the ordinary callback call and
restores availability before returning.

#### Observable result

Running `first` and `second` sequentially leaves identifiers `1` and `2` in the same inbox. Dropping
either handle early does not release the inbox while another handle remains.

#### Boundary case

```silk,ignore
Shared.withMut(&inbox, fn(state) {
  return effect { ReadyInbox.pushNoGrow(state, 3) }
})
```

Mutation may not escape into an Effect or another stored executable. The callback must finish before
`withMut` returns; suspension while access is held is rejected.

### Case: Share Deferred-style state without making it compiler-known

#### Intent

Let one ordinary-source actor retain readiness callbacks while another handle publishes a value.

#### Current Silk

An affine state owner can be moved into the waiter list or producer, but not both. A lexical
`&mut State<A>` cannot be retained by several independently stored Effects. Existing raw allocation
operations do not change that ownership fact.

#### Desired Silk

```silk,ignore
// Illustrative source implementation shape, not a canonical Deferred API.
struct Deferred<A> {
  state: Shared<State<A>>
}

effect fn make<A>() -> Deferred<A>
! OutOfMemoryError
? &mut Allocator {
  let state = run Shared.make(State.pending<A>())
  return Deferred<A> { state: move state }
}

fn clone<A>(self: &Deferred<A>) -> Deferred<A> {
  return Deferred<A> { state: Shared.clone(&self.state) }
}
```

Await registration and completion use short `Shared.withMut` calls. They remove readiness
callbacks from the state before invoking them, so no callback re-enters the same cell while its
exclusive access is active.

#### Observable result

Every Deferred handle observes one source-owned state. Publishing a value once can extract and call
registered readiness callbacks after the mutation callback has returned. Dropping the last handle
cleans an unpublished affine value and every unconsumed callback exactly once.

#### Boundary case

The local shared handle cannot cross an OS-thread boundary. A future thread-safe Deferred requires a
separate transferable shared handle whose reference count, state transitions, and memory ordering
are defined by the parallel-memory model.

### Case: Release one affine value at the last handle

#### Intent

Prove that shared ownership does not create a second cleanup authority or turn affine `T` into Copy.

#### Current Silk

Ordinary copying of an affine allocation or value is rejected. Manually copying an allocation
address would lose the language's knowledge of which alias performs cleanup.

#### Desired Silk

```silk,ignore
let first = run Shared.make(Token.make())
let second = Shared.clone(&first)

drop first
Token.observeStillLive()
drop second
```

#### Observable result

The first drop decrements local ownership only. The second drop exclusively cleans `Token` and
releases the control-block allocation. `Token` is never copied, and cloning the handle does not make
`Token: Copy`.

#### Boundary case

Strong-reference cycles are not collected. A cycle of local shared handles leaks until a later Weak
or cycle-management proposal supplies an explicit way to break it.

## Goals and non-goals

### Goals

- Provide independently owned local handles to one stable allocation and value.
- Make allocation explicit at construction and allocation-free at clone and access.
- Preserve exact cleanup of affine `T` at the last strong handle.
- Trap before strong-count overflow without changing the count or manufacturing a handle.
- Confine inspection and mutation to ordinary non-suspending callbacks.
- Prevent references obtained during access from escaping their callback.
- Define deterministic single-threaded reference-count and access behavior across evaluator, native,
  and Wasm.
- Keep the public actor and its validation policy in ordinary Silk.
- Supply the shared-state prerequisite needed by source-defined ready inboxes and Deferred actors.

### Non-goals

- Define fibers, Scheduler, parking, timers, or concurrency APIs.
- Permit a local handle, retained reference, or callback to cross threads.
- Define atomics, locks, memory ordering, transfer/share derivation, or data-race behavior.
- Add garbage collection, tracing, weak handles, cycle collection, or implicit cycle detection.
- Make every value shared, implicitly allocate captures, or change ordinary `&T` and `&mut T`.
- Hide allocation failure or add `Allocator` to operations other than construction or explicit growth
  performed by the user's callback.
- Allow an Effect, suspended computation, or escaping callable to retain access acquired by
  `with` or `withMut`.

## Current language model

Silk values have one cleanup owner unless admitted as Copy. Shared borrows permit concurrent reads
within their lexical loans; exclusive borrows permit mutation while excluding every competing
access. Stored Effects and callables retain their captures and loans until last safe use or drop.

Owned allocations preserve the same model. `Allocation` owns reclaim authority, `RawBuffer<T>` owns
one typed view over it, and a lexical `Slot<T>` transition requires exclusive access to that buffer.
These operations are sufficient for Box and Vector because those actors retain one affine owner.
They intentionally cannot manufacture aliases that each believe they keep an allocation alive.

There is therefore no current source value corresponding to `Rc<RefCell<T>>`, a JavaScript object
shared by several closures, or a local runtime handle. Target-side code may use such structures
internally, but ordinary Silk cannot express their ownership and cleanup contract.

## Proposed language model

The selected public name is `Shared<T>`: an explicitly cloned, non-thread-transferable
strong handle to one stable control block containing `T`, local strong-count state, and scoped-access
state.

```text
Shared<T> handle ----+
Shared<T> handle ----+--> local control block { strong count, access state, T }
Shared<T> handle ----+
```

The handle itself is not Copy. `Shared.clone(&handle)` is the only safe duplication operation
and has observable ownership meaning even though it does not run user code or allocate. Moving a
handle transfers one count obligation; dropping it discharges that obligation. The last drop gains
exclusive cleanup authority for `T` and the allocation.

Construction is effectful because it allocates:

```silk,ignore
effect fn Shared.make<T>(value: T) -> Shared<T>
! OutOfMemoryError
? &mut Allocator
```

The selected access API is callback-shaped:

```silk,ignore
fn Shared.with<T, A>(
  self: &Shared<T>,
  use: once fn(&T) -> A
) -> A

fn Shared.withMut<T, A>(
  self: &Shared<T>,
  use: once fn(&mut T) -> A
) -> A
```

Both callbacks are ordinary functions, not Effect-returning functions. Their parameter borrows are
scoped to the invocation and cannot be stored, returned, captured by an escaping executable, or held
across suspension. Both operations initially acquire the control block's one exclusive access;
`with` merely narrows the resulting `&mut T` to `&T` before invoking user code. A reentrant `with` or
`withMut` through any alias therefore conflicts and traps through ordinary source policy. The sealed
primitive reports conflict through a callback so a future source-defined `tryWithMut` can instead
return ordinary result data without changing the compiler boundary.

`Shared<T>` has one local execution affinity. Moving it between frames of the same independently
resumable execution or between fibers owned by one local Scheduler is allowed. Transferring it to
another OS thread is rejected before lowering. A later parallel proposal may introduce a distinct
thread-safe actor or a proven conversion; it must not silently add atomic cost to `Shared`.

## Worked language experience

### Ordinary access and the reentrancy matrix

```silk,ignore
let cell = run Shared.make(Counter.make(1))

let before = Shared.with(&cell, fn(value) {
  return Counter.value(value)
})

Shared.withMut(&cell, fn(value) {
  Counter.increment(value)
  return ()
})

let after = Shared.with(&cell, fn(value) {
  return Counter.value(value)
})
```

The observable pair is `(1, 2)`. Each callback returns before access becomes available again. The
initial implementation uses one exclusive access state for both public operations, so every nested
access through an alias follows the same matrix:

| Outer call | Nested call | Result |
| --- | --- | --- |
| `with` | `with` | conflict trap |
| `with` | `withMut` | conflict trap |
| `withMut` | `with` | conflict trap |
| `withMut` | `withMut` | conflict trap |

```silk,ignore
let alias = Shared.clone(&cell)

Shared.with(&cell, fn(_) {
  // Traps through Shared's ordinary source `onConflict` policy.
  return Shared.with(&alias, fn(value) { return Counter.value(value) })
})
```

No nested case obtains a second reference. Cloning a handle or dropping a non-last alias during an
access callback does not itself access `T` and remains legal; the borrowed receiver ensures that the
active callback cannot drop the last handle.

### Move affine values through ordinary state operations

```silk,ignore
// Mailbox is an ordinary source actor whose put/take operations preserve initialization state.
let mailbox = run Shared.make(Mailbox.empty<Token>())
let token = Token.make(7)

Shared.withMut(&mailbox, fn(state) {
  Mailbox.put(state, move token)
  return ()
})

let returned = Shared.withMut(&mailbox, fn(state) {
  return Mailbox.take(state)
})
```

The token moves into the control-block value and later moves out exactly once. `withMut` does not
make `Token` Copy and grants no privilege beyond the ordinary operations available through
`&mut Mailbox<Token>`.

Returning the borrow itself remains invalid:

```silk,ignore
let escaped = Shared.with(&mailbox, fn(value) {
  return value
})
// Rejected: the callback-scoped borrow cannot appear in the result.
```

Returning an Effect or stored callable that captures `value` is rejected for the same escape reason,
even if that executable is never run.

### Construction failure and last-handle cleanup

```silk,ignore
effect fn allocateToken() -> Shared<Token>
! OutOfMemoryError
? &mut Allocator {
  return run Shared.make(Token.make(9))
}
```

If allocation fails, no `Shared<Token>` exists. The still-owned argument is cleaned exactly once by
ordinary typed-failure cleanup; `sharedFromAllocation` is never called. Once construction succeeds,
the following paths all have one cleanup authority:

```silk,ignore
let first = run Shared.make(Token.make(10))
let second = Shared.clone(&first)

drop first            // count 2 -> 1; Token remains live
drop second           // count 1 -> 0; clean Token, then release allocation
```

If a later Effect fails while both handles are live, ordinary frame cleanup drops both handles and
only the count transition to zero cleans `Token`. A fatal trap does not promise unwinding or cleanup,
consistent with Silk's general rule.

`Shared.clone` is allocation-free and has no typed failure. If incrementing the target's local
strong-count representation would overflow, it traps before changing the count and before producing
a new handle. Wrapping the count, saturating it, or leaking a phantom obligation is invalid.

### Retention in ordinary values and executions

```silk,ignore
struct Observer {
  state: Shared<Counter>
}

let state = run Shared.make(Counter.make(0))
let observer = Observer { state: Shared.clone(&state) }
let action = effect {
  return Shared.with(&state, fn(value) { return Counter.value(value) })
}
```

A handle may be moved into a nominal value, ordinary callable, Effect, or SLP-0001 `Execution`, and
each explicit clone contributes one strong obligation. Parking such an execution does not change
the count. Dropping the dormant execution drops its captured handle normally.

These are local execution handles, not thread-transferable values. Two fibers owned by one local
Scheduler may retain clones because they run on the same thread. A future attempt to send either
`observer` or `action` to another OS thread is rejected through the parallel proposal's transfer
rules; this SLP does not predeclare that future syntax.

### Cycles and target parity

```silk,ignore
// Illustrative: each Node stores a strong handle to the other.
let left = run Shared.make(Node.empty())
let right = run Shared.make(Node.empty())
Node.link(&left, Shared.clone(&right))
Node.link(&right, Shared.clone(&left))
drop left
drop right
```

The two-node strong cycle leaks: neither count reaches zero. This is specified behavior, not a
collector request. Ordinary source must break the cycle explicitly; a future Weak proposal may make
that practical without changing acyclic cleanup.

For every acyclic program above, evaluator, native, and Wasm must agree on successful access,
conflict selection, strong-count transitions, `T` cleanup, and allocation release order. The
control-block byte layout may differ by target; observable ownership transitions may not.

## Semantic sketch

1. `make` obtains one allocation through the selected ordinary Allocator provider.
2. `sharedFromAllocation` consumes that allocation and `T`, initializes one local control block, and
   returns one strong handle. Failure before the call leaves cleanup with ordinary source; a valid
   call cannot return partial state.
3. `clone` increments the local strong count and returns one new non-Copy handle. It does not clone
   `T` or allocate. If increment would overflow, it traps before changing the count or returning a
   handle.
4. `with` and `withMut` both establish exclusive control-block access for exactly one ordinary
   callback invocation. `with` narrows the callback argument to a shared borrow.
5. The callback's lexical borrow ends before the operation restores the access state and returns its
   result. It cannot suspend or escape the borrow.
6. A conflicting reentrant access invokes the sealed operation's `onConflict` callback while the
   existing access remains active. The selected public wrappers trap; another ordinary wrapper may
   translate the same outcome into data. No conflict creates overlapping references.
7. Dropping a non-last handle decrements the local strong count and performs no `T` cleanup.
8. Dropping the last handle obtains exclusive control-block ownership, cleans `T` exactly once, and
   releases the allocation exactly once. Fatal traps retain Silk's general no-unwind rule.

## Compiler–standard library boundary

### Compiler necessity

Ordinary Silk cannot turn one affine Allocation into several independently owned handles, attach one
last-drop obligation to their dynamic count, mutate through shared handle access without violating
ordinary borrow rules, or guarantee that an escaping handle never crosses a thread. Raw address
forging would abandon the ownership, initialization, and cleanup facts the language is designed to
preserve.

### Smallest target-neutral primitive

The selected semantic capability is a non-transferable local shared control block with explicit
strong cloning, one callback-scoped exclusive access operation, and last-handle cleanup. The sealed
surface is one opaque type and four callable operations:

```silk,ignore
Intrinsic.SharedCore<T>

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

Dropping the opaque core performs decrement-or-last-cleanup. `sharedLayout` lets ordinary source
request storage from an ordinary Allocator; the compiler does not recognize Allocator,
OutOfMemoryError, or the `Shared` wrapper by spelling. `sharedFromAllocation` is unsafe because the
caller must supply a live allocation with the exact requested layout and transfer `value` exactly
once. `sharedWithMut` invokes exactly one callback: `use` after acquiring exclusive access, or
`onConflict` when another access is active. The callback-shaped conflict outcome avoids a
compiler-known error type or trap policy.

`sharedClone` is total only while its bounded target count can be incremented. Count exhaustion is a
fatal intrinsic trap, like other representation exhaustion that cannot be expressed through this
infallible operation. The transition checks before mutation, so no wrapped count, partial handle, or
additional cleanup obligation becomes observable.

There is no separate sealed inspection operation. `Shared.with` is ordinary source over
`sharedWithMut` and narrows the exclusive callback borrow to `&T`. This intentionally rejects nested
read-only access in the first version; the driving Scheduler and Deferred cases need no overlapping
callback access, and a later intrinsic can add true shared access if real programs justify its count
and conflict state.

### Standard-library construction

The ordinary `Shared<T>` nominal stores one opaque core. `make` requests `sharedLayout` through
`Allocator.allocate`, passes the successful Allocation and value to `sharedFromAllocation`, and
returns the safe wrapper. `clone` delegates the count transition. `withMut` delegates exclusive
access and traps from its ordinary `onConflict` callback; `with` delegates through `withMut` while
narrowing the borrow. Field cleanup of the opaque core supplies Drop behavior.

Ready inboxes, Deferred, memo cells, observer registries, and local caches remain ordinary actors
containing `Shared<State>`. Their state machines invoke external callbacks only after a `withMut`
call has restored access, preventing accidental reentrant mutation of the same cell.

### Privilege audit

A borrowed `LocalCell<T>` is smaller but does not solve dynamically owned lifetimes: every user must
remain inside one lexical owner, and a Scheduler cannot freely retain or remove tasks referring to a
shorter-lived local. A raw pointer plus source reference count is superficially lower level but
exposes address provenance, last-drop authority, and aliasing without enough facts for safe cleanup.

A separate sealed read-only access is also unnecessary initially: single-threaded, non-suspending
callbacks do not overlap except by reentrancy, and the source witnesses can extract work before
calling external code. Adding reader counts now would privilege behavior the driving cases do not
need.

A compiler-known Scheduler, Deferred, Cell, queue, or callback registry is too large. Once local
shared ownership is safe, all of those policies are ordinary source state machines. Atomic reference
counts, locks, and cross-thread wakeups are also too large for this proposal because the driving cases
are local and deterministic.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Not affected | The selected public actor and every sealed operation use existing declarations and calls. |
| Types and abstraction | Affected | One opaque non-Copy, non-transferable generic core and one ordinary `Shared<T>` nominal participate in specialization. |
| Execution contracts | Affected | Construction carries explicit allocation channels; access callbacks are ordinary and cannot suspend. |
| Ownership and resources | Affected | Strong-count obligations, moves, clones, scoped borrows, reentrancy, last-drop cleanup, and cycles are central. |
| Runtime and targets | Affected | Evaluator, native, and Wasm need equivalent stable control blocks, local counts, access state, and release behavior. |
| Compiler | Affected | Intrinsic typing, ownership classification, cleanup, callback-borrow escape, target lowering, and future thread-transfer rejection participate. |
| Standard library | Affected | `Shared` and safe stateful actors are ordinary source over the sealed core. |
| Tooling and diagnostics | Affected | Allocation, escaping access, conflicting reentrancy, cycle guidance, and thread-affinity diagnostics need stable categories. |
| Learning and use | Affected | Documentation must distinguish lexical borrows, local shared ownership, and future thread-safe sharing. |

## Scope cohesion

This SLP asks one question: should Silk admit explicitly allocated, dynamically lived local shared
ownership without weakening ordinary lexical borrowing? Strong counts, scoped interior access, last-
handle cleanup, allocation, and local execution affinity are one ownership model.

SLP-0001 consumes this capability for Deferred and ready-inbox state but owns execution transfer,
parking, and wake ordering. Neither SLP requires the other's public names. Parallel shared ownership
is a later proposal because transfer derivation, atomic counts, memory ordering, locks, and data races
can change independently and would impose costs absent from the local case.

## Complexity and subtraction budget

The feature earns one opaque local shared control-block concept, explicit clone, one callback-scoped
exclusive access operation, callback-shaped conflict selection, and last-handle cleanup.
Construction reuses the existing ordinary Allocator and Allocation model instead of earning hidden
storage policy. Read-only public access is derived by borrow narrowing.

It does not earn implicit sharing, universal heap allocation, Copy for handles or contents, atomic
operations, thread transfer, garbage collection, weak handles, cycle collection, Effect callbacks,
or compiler-known stateful library actors.

## Surface displacement

The public standard library gains an explicitly imported ownership actor rather than changing `&T`,
`&mut T`, nominal fields, or Effect capture implicitly. The compiler gains a sealed control-block
boundary only because ordinary source cannot preserve alias provenance and dynamic last-drop
authority through RawBuffer operations.

Existing Box, Vector, Allocation, RawBuffer, and Slot retain their affine contracts. Code pays no
shared-control-block cost unless it constructs a local shared value.

## Drawbacks and risks

- Every independently lived shared value requires stable indirect storage, normally an allocation.
- Dynamic access-conflict checks introduce runtime state and a misuse mode absent from lexical borrows.
- Strong cycles leak without Weak or explicit cycle breaking.
- Reference counting adds clone and drop work even on one thread.
- Strong-count exhaustion is fatal because clone has no recoverable failure channel.
- Because `with` initially uses exclusive control-block access, nested read-only `with` calls trap
  even though their underlying borrows could theoretically coexist.
- A too-powerful intrinsic could become an untyped pointer or general aliasing escape hatch.
- A too-narrow intrinsic could force the compiler to recognize the public wrapper or make correct
  cleanup impossible in ordinary source.
- Non-transferability must remain visible enough that future multithreaded fibers do not accidentally
  accept local handles.
- Fatal traps do not unwind, so a trap during a mutation callback does not promise restoration or
  cleanup; the whole process or instance remains failed under Silk's general rule.

## Alternatives and prior art

### Status quo

Keep only affine owners and lexical borrows. This preserves the strongest static model but cannot
express independently retained local callbacks, Deferred state, observer registries, or scheduler
inboxes without making each one a privileged runtime actor.

### Smaller primitive or library solution

Use `&LocalCell<T>` over stack or uniquely owned storage. This avoids reference counting and may
allocate nothing, but every handle remains a borrow. It is useful for statically scoped activity and
may be added independently, yet it cannot represent the dynamic lifetime required by the driving
cases.

Expose raw shared addresses and implement reference counting entirely in unsafe source. This appears
minimal but discards provenance, non-transferability, last-drop authority, and scoped alias facts.
Proving the wrapper sound would require reconstructing compiler-owned ownership rules manually.

Replace callback-scoped `&mut T` with a sealed take-and-replace transaction. This can make affine
movement explicit, but it forces every access to move or reconstruct all of `T`, complicates
panic-free restoration, and exposes more state-transition policy than the driving cases require.
The selected ordinary `&mut T` callback already permits sound source actors such as Mailbox to move
their initialized fields under existing ownership rules.

### Strongest competing language model

Adopt tracing garbage collection or make all reference-like values implicitly shared heap objects.
That removes explicit last-handle management and naturally handles many object graphs, but it changes
the language-wide allocation, latency, finalization, FFI, and cleanup model. The selected direction
keeps sharing explicit, pay-for-use, deterministic for acyclic graphs, and compatible with affine
ownership.

Rust `Rc<RefCell<T>>`, Swift reference types with exclusivity enforcement, and single-threaded runtime
handles provide relevant evidence. Silk does not inherit their exact public APIs or conflict
behavior; the useful separation is stable shared lifetime versus scoped access to the value.

## Falsifiers and acceptance blockers

- Cloning a handle copies, moves, or cleans `T` instead of sharing it.
- Dropping handles can leak an acyclic value, release the allocation early, or clean `T` twice.
- Strong-count overflow wraps, saturates, mutates the count before trapping, or returns a partial
  handle.
- Conflicting access can create two mutable references or one mutable and competing shared reference.
- A callback borrow can escape, be stored, or remain live across Effect suspension.
- Construction hides allocation or adds Allocator channels to clone, access, suspension, or unrelated
  Effect results.
- A local handle can cross threads without a parallel memory model.
- The compiler recognizes `Shared`, Deferred, Scheduler, or another library declaration by
  spelling.
- Direct and Wasm engines disagree on clone/drop order, conflict behavior, or affine cleanup.
- A complete source wrapper requires raw address privileges broad enough to bypass ordinary ownership
  outside this control-block use case.
- SLP-0001 still requires a compiler-known queue or Deferred after using this capability.

## Open realization questions

These questions may refine representation and diagnostics but may not change explicit allocation,
infallible clone with fatal overflow, exclusive callback access, non-transferability, or last-handle
cleanup:

- How control-block size, alignment, header fields, and target layout remain target-neutral while
  ordinary source requests allocation.
- Which compiler semantic fact records non-transferability before a parallel type model consumes it.
- How callback-borrow escape and conflict diagnostics reuse ordinary ownership categories while
  identifying the `Shared` access boundary precisely.

## Future directions

A later proposal may add borrowed allocation-free `LocalCell<T>` for statically scoped sharing.

A parallel-memory proposal may define transferable shared ownership, atomic reference counts,
memory ordering, mutexes, condition variables, cross-thread readiness, and conversion rules from
local state. It must preserve the zero-atomic-cost local actor.

A Weak-handle proposal may add non-owning observation and explicit cycle breaking after real source
graphs demonstrate the need.

A later library proposal may add identity comparison if source algorithms demonstrate that shared
allocation identity, rather than value access, belongs in the public actor.

## OpenSpec realization map

No OpenSpec handoff exists while this proposal remains a Candidate. If the direction is later
accepted, likely capability slices are:

1. local shared ownership and execution-affinity semantic facts;
2. caller-funded control-block allocation and from-allocation initialization;
3. clone, scoped access, conflict, and last-handle cleanup;
4. ordinary-source `Shared<T>` construction and safe wrappers;
5. evaluator, native, and Wasm parity; and
6. SLP-0001 ready-inbox and Deferred sufficiency evidence.

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-21 | Split the shared-state prerequisite from SLP-0001. Seeded explicit allocation, allocation-free clone and access, non-Copy local handles, ordinary non-suspending scoped access, last-handle cleanup, no thread transfer or atomics, Deferred and ready-inbox pressure, and an honest unresolved primitive and access-conflict frontier. |
| 2 | 2026-08-21 | Selected `Shared<T>` as the ordinary public actor name. Thread transfer remains a type property rather than a name prefix; the Draft continues to describe the initial implementation as local, non-atomic shared ownership. Renamed the provisional sealed core consistently while leaving its operation-collapse audit open. |
| 3 | 2026-08-21 | Selected one opaque `SharedCore<T>` and four operations: `sharedLayout`, `sharedFromAllocation`, `sharedClone`, and callback-shaped `sharedWithMut`. Derived public `Shared.with` by narrowing the exclusive borrow, selected public `Shared.withMut`, rejected a separate initial read-access primitive, and kept conflict policy in ordinary source through `onConflict`; the initial wrappers trap on every reentrant conflict. |
| 4 | 2026-08-21 | Completed the ownership pressure cases: all initial reentrant access combinations conflict, callback borrows cannot escape, affine values move only through ordinary state operations, allocation failure creates no partial handle, acyclic last-handle cleanup is exact, strong cycles leak, local handles may be retained by Effects and executions but not transferred across threads, and strong-count overflow traps before mutation without adding a clone failure channel. |
| 5 | 2026-08-21 | Author explicitly promoted the completed direction to Candidate. This revision changes lifecycle metadata only and freezes the dossier for fixed-revision adversarial review. |
