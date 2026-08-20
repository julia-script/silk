# SLP-0009: Explicit stack-safe Effect suspension

SLP: 0009
Status: Candidate
Revision: 8
Author: Julia Ortiz
Created: 2026-08-19
Updated: 2026-08-19
Discussion: —
Review record: —
Depends on: SLP-0003, SLP-0006, SLP-0007, SLP-0008
Split from: —
Split into: —
Supersedes: —
Superseded by: —
Revisit when: runtime parking, async execution, or structured concurrency becomes a priority
Resolution: Accepted direction: SUSP-001–020
OpenSpec handoff: `align-effect-suspension-coroutine-storage`

## Summary

`Effect.suspend` is one explicit stack-safety boundary for Effect execution. It
accepts a deferred child Effect and transfers its execution through compiler-owned continuation
machinery, allowing terminating recursive Effect cycles that cross the boundary to use bounded
native and WebAssembly machine stack. It is not a scheduler yield, a parked task, an observable
pending state, or a general guarantee for ordinary recursion. The public function remains ordinary
Silk over one sealed target-neutral intrinsic, and ordinary Effect combinators compose with it
without learning a second Effect representation. Each concrete suspendable invocation has one
compiler-shaped coroutine frame whose maximum layout is known statically and whose states reuse the
same storage. `Effect.suspend` therefore preserves the child's failure and requirement channels; it
does not add `OutOfMemoryError` or `Allocator`. Dynamic recursion still consumes finite execution storage,
but that storage belongs to the compiler-owned execution stack and exhaustion is a fatal trap rather
than a typed Effect failure.

## Problem and evidence

Deep recursive Effects previously consumed one native or host-Wasm call frame per logical
invocation. A terminating program could therefore end in `SIGSEGV` or a host `RangeError` instead
of producing its declared typed outcome. The evaluator happened not to fail the same way because it
already represented activations on the heap, creating a cross-engine semantic mismatch rather than
a language guarantee.

The historical discussion considered whether the same machinery might eventually support streams,
queues, latches, semaphores, and async execution. That is useful architectural pressure, but it does
not establish their semantics. Stack-safe recursion needs only a deferred transfer and later resume;
a waiting stream consumer additionally needs registration, parking, wakeup, scheduling, lost-wakeup
prevention, cancellation, and ownership rules for values held by shared synchronization actors.
Conflating those capabilities would make today's small explicit operation accidentally define a
runtime model Silk has not chosen.

The current compiler and standard library implement the narrower feature across evaluation, native,
and direct Wasm. This proposal exists because implementation coverage is evidence, not authority:
the language contract still needs to be understandable, independently reviewed, and corrected where
the historical design made an unnecessary choice.

## Driving examples: current and desired

### Case: Make non-tail recursive Effect execution stack safe

#### Intent

Count through a depth that may exceed the machine call stack while retaining work after the
recursive result returns.

#### Current Silk

```silk,ignore
effect fn count(value: i32) -> i32 ! OutOfMemoryError ? &mut Allocator {
  if value == 0 { return 0 }

  let inner = run Effect.suspend(count(value - 1))
  return inner + 1
}
```

#### Desired Silk

```silk,ignore
effect fn count(value: i32) -> i32 {
  if value == 0 { return 0 }

  let inner = run Effect.suspend(count(value - 1))
  return inner + 1
}
```

The spelling is retained while the allocator requirement and storage failure disappear. The
important rule is the execution boundary, not the fact that the example decrements an integer.

#### Observable result

Every recursive cycle crosses `Effect.suspend`, so native and WebAssembly machine-stack consumption
is bounded independently of `value`. The function still returns the same `i32`, propagates the same
typed outcomes, and performs `inner + 1` once per logical invocation.

#### Boundary case

```silk,ignore
effect fn count(value: i32) -> i32 {
  if value == 0 { return 0 }
  return 1 + run count(value - 1)
}
```

This is ordinary recursive Effect execution. It receives no bounded-stack guarantee merely because
the declaration is an `effect fn`.

### Case: Preserve an ordinary combinator across suspension

#### Intent

Transform the result of a potentially suspending Effect without exposing continuation mechanics to
`Effect.map` or to user-defined equivalents.

#### Current Silk

```silk,ignore
fn increment(value: i32) -> i32 { return value + 1 }

effect fn program() -> i32 ! OutOfMemoryError ? &mut Allocator {
  let deferred = Effect.suspend(effect { return 41 })
  return run (move deferred |> Effect.map(increment))
}
```

#### Desired Silk

```silk,ignore
fn increment(value: i32) -> i32 { return value + 1 }

effect fn program() -> i32 {
  let deferred = Effect.suspend(effect { return 41 })
  return run (move deferred |> Effect.map(increment))
}
```

The source-level composition is retained while its compiler-storage channels disappear.

#### Observable result

`increment` runs once after the deferred child succeeds. Its ordinary source definition does not
inspect a pending result, continuation frame, scheduler, or compiler token.

#### Boundary case

If the child already fails with `ProblemError` or requires `&Clock`, those channels remain visible
unchanged. Suspension introduces neither an allocator requirement nor a recoverable storage failure.
Exhausting the finite compiler-owned execution stack traps outside the typed failure channel.

### Case: Do not mistake suspension for parking

#### Intent

Distinguish today's stack transfer from a future Effect waiting for another execution context to
publish a stream value.

#### Current Silk

```silk,ignore
let value = run Effect.suspend(readNext())
```

This may move `readNext` through the stack-safe runner, but it does not let another task run, register
a waiter, or arrange a later wakeup. If `readNext` cannot complete synchronously under today's
language, `Effect.suspend` does not supply the missing concurrency model.

#### Desired Silk

No parking syntax is proposed here. A future proposal may reuse private continuation machinery while
adding a separate public contract for park/wake, scheduling, structured lifetime, and interruption.

#### Observable result

Programs using only `Effect.suspend` remain deterministic single-execution compositions and acquire
no scheduler or concurrency runtime.

## Goals and non-goals

### Goals

- Define exactly which Effect recursion becomes stack safe.
- Preserve one ordinary public `Effect<A ! E ? R>` abstraction across synchronous and suspended
  execution.
- Keep coroutine-frame storage outside public failure and requirement channels without introducing
  a source-visible ambient allocator.
- Preserve ownership, borrows, providers, typed outcomes, and cleanup across continuation frames.
- Require equivalent behavior from evaluation, native, and direct WebAssembly.
- Preserve pay-for-use behavior for call graphs that cannot reach suspension.
- Give programmers useful guidance for missing suspension boundaries and ordinary ownership
  diagnostics without making recursion inference magical.

### Non-goals

- Make ordinary function recursion, every recursive Effect, or recursive `Drop` stack safe.
- Add tail-call optimization or promise any target's tail-call ABI.
- Design generators, tasks, fibers, executors, queues, streams, latches, semaphores, cancellation, or
  async cleanup.
- Expose a public continuation type, pending result, callback ABI, or scheduler handle.
- Require source-defined Effect combinators to recognize suspension.
- Standardize the compiler's native, Wasm, or evaluator continuation representation.

## Current language model

`effect { ... }` and calls to `effect fn` already construct lazy Effect values. `run` executes one
Effect layer. `Effect.suspend` therefore does not create a fundamentally different kind of laziness:
its distinctive purpose is to force the deferred child through the stack-safe execution boundary.

The current standard library widens the contract:

```silk,ignore
effect fn suspend<A, !E, ?R>(
  deferred: once Effect<A ! E ? R>
) -> A ! E | OutOfMemoryError ? R | &mut Allocator
```

The current compiler builds separately allocated continuation records for active relays and routes
their storage through the selected source allocator. This is implemented behavior, but it is the
design drift this proposal corrects.

## Proposed language model

The stable function instead has this contract:

```silk,ignore
effect fn suspend<A, E, ?R>(
  deferred: once Effect<A ! E ? R>
) -> A ! E ? R
```

The `once` parameter transfers one Effect into the operation. Reaching the intrinsic yields the
current invocation to the private execution driver after saving the state required to resume it.
Each concrete suspendable invocation has a fixed maximum frame layout derived from all of its
suspension states; reaching another suspension point reuses that invocation's existing frame.

Concrete suspendability is propagated through specialized call edges. Consequently, an ordinary
source-defined combinator may compile through a suspendable private path when its selected child can
suspend, even though its source signature and implementation remain unchanged.

The public concept is an **explicit stack-safe Effect boundary**:

- **Deferred child** means the owned Effect value passed to `Effect.suspend`. It does not start until
  the transfer is accepted and any required continuation state is ready.
- **Suspension origin** means the explicit `Effect.suspend` boundary that requests transfer. The
  origin records one resume state in its invocation frame; it does not allocate a separate origin
  continuation record.
- **Suspendable caller** means a concrete Effect execution path that can reach a suspension origin.
  It may still complete synchronously on a branch that never reaches one.
- **Continuation** means compiler-private state sufficient to resume one logical caller after its
  deferred child completes. One invocation's possible continuation states share one statically
  shaped coroutine frame. It is not a source value or public ABI.
- **Relay** means propagation of the transfer from a child through an ordinary caller toward the
  private driver. A stateful relay retains the exact values and control point needed after resume.
- **Resume** means continuing the logical caller with the child's typed success or failure. It does
  not imply another thread, task, or scheduler.
- **Execution stack** means compiler-owned storage for the dynamically active coroutine frames.
  Individual frame layouts are statically known; total recursive depth is not. Exhaustion traps in
  the same class as machine-stack exhaustion and never becomes an Effect failure.
- **Parking** means an unfinished execution becoming dormant until an external wakeup condition is
  met. Parking is not provided by `Effect.suspend`.

A terminating self- or mutually-recursive Effect cycle has the bounded-machine-stack guarantee only
when every possible recursive cycle crosses an explicit suspension origin. Suspension on an
unrelated branch or before an execution path that later enters a non-suspended recursive cycle is
not enough.

## Accepted rule catalog

These IDs preserve stable review references. The author has accepted SUSP-001–020; realization
planning remains a separate OpenSpec step.

### Public contract and recursion

- **SUSP-001 — Explicit boundary:** `Effect.suspend` is an explicit stack-safe Effect execution
  boundary. It is not inserted automatically.
- **SUSP-002 — Ordinary Effect result:** Running `Effect.suspend(child)` produces the child's one
  success value or failure; it does not expose a nested Effect, pending result, or continuation.
- **SUSP-003 — Covered cycles:** A terminating self- or mutually-recursive Effect cycle uses bounded
  native and Wasm machine stack when every recursive cycle crosses `Effect.suspend`.
- **SUSP-004 — Deliberate exclusions:** Ordinary function recursion, Effect recursion whose cycle
  does not cross the boundary, recursive `Drop`, and arbitrary recursive data traversal receive no
  stack-safety guarantee from this feature.
- **SUSP-005 — Channel preservation:** `Effect.suspend<A, E, ?R>` preserves exactly `A`, `E`, and
  `R`. It adds no storage failure or allocator service to the public Effect contract.
- **SUSP-006 — Execution-resource exhaustion:** One invocation's coroutine-frame size is known
  statically, but dynamic recursion depth is not. Exhausting the compiler-owned execution stack is a
  fatal trap outside the typed failure channel, like exhausting the machine stack.

### Execution and composition

- **SUSP-007 — Deferred child:** The owned `once Effect<A ! E ? R>` child does not start merely
  because `Effect.suspend` is constructed. Once run, transfer preparation completes before the child
  begins.
- **SUSP-008 — Suspension-transparent combinators:** Ordinary source-defined combinators compose
  with suspendable children without exposing or inspecting pending state or changing their public
  signatures.
- **SUSP-009 — Reused invocation frame:** Repeated suspension by one invocation reuses its coroutine
  frame rather than allocating a continuation record per yield. A non-suspendable invocation keeps
  the ordinary direct representation.
- **SUSP-010 — Static maximum frame:** Each concrete suspendable invocation has one statically known
  maximum frame layout representing its possible resume points and the exact values live in each
  state. Dynamic recursion creates more invocations, not a larger recursive type.

### Ownership and lifecycle

- **SUSP-011 — Ordinary ownership across states:** Copy values may copy; affine values move into one
  field of the invocation frame; borrows and provider access retain their ordinary exclusivity and
  lifetime rules while the logical caller is suspended. A referent that remains borrowed across
  suspension has a stable logical location for the borrow's lifetime; no public `Pin` mechanism is
  required merely because the compiler selected a coroutine representation.
- **SUSP-012 — Complete state transition:** The current invocation finishes moving every live value
  into its next valid coroutine state before the driver starts the deferred child. No source value
  remains simultaneously owned by the active state and its suspended representation.
- **SUSP-013 — Exact cleanup:** Success and typed failure move or clean every retained value exactly
  once and release completed execution frames according to their owner. A fatal trap retains the
  language-wide no-unwind rule.
- **SUSP-014 — Execution-owned placement:** Frame placement, growth, and release belong to the
  compiler-owned execution stack, not to a source `Allocator` service selected by `Effect.suspend`.
  Their exact allocation events are not source-observable. Relocation, segmentation, or growth must
  preserve every live reference; a backend cannot invalidate a source-valid borrow by moving a
  private frame.
- **SUSP-015 — No recursive allocator rule:** Because suspension does not select a source allocator,
  it creates no special restriction on ordinary `Allocator` implementations. Any later fallible
  task or fiber constructor must define its own storage and bootstrap contract.

### Engines, limits, and tooling

- **SUSP-016 — Logical evaluation depth:** A suspended source invocation still counts toward the
  evaluator's source-logical `CallDepth`; compiler-generated driver and resume helpers do not.
- **SUSP-017 — Cross-engine parity:** Evaluation, native, and direct Wasm preserve the same typed
  outcome, retained ownership, and cleanup order. Native and Wasm additionally guarantee bounded
  machine stack for covered cycles.
- **SUSP-018 — Static pay-for-use:** A closed call graph that cannot reach the suspension intrinsic
  contains no coroutine-frame transformation, private suspension driver, pending branch, scheduler,
  or widened channels because suspension exists elsewhere.
- **SUSP-019 — Tooling, not semantic rejection:** An uncovered recursive Effect cycle remains valid
  Silk. The LSP may warn that its stack depth is unbounded and offer to insert an explicit boundary;
  the compiler does not reject potentially shallow intentional recursion.
- **SUSP-020 — No async promise:** Suspension provides no parking, wakeup, fairness, parallelism,
  interruption, or scheduler semantics. A future capability must specify those separately even if
  it reuses private continuation machinery.

## Worked language experience

### Laziness and one-layer execution

```silk,ignore
fn delayed(value: i32) -> Effect<i32> {
  return Effect.suspend(effect { return value })
}
```

Calling `delayed(42)` constructs an Effect. It does not run the inner body. Running the returned
Effect executes one layer and eventually produces `42`; it does not return a nested `Effect<i32>`.

### Typed failure propagation

```silk,ignore
struct ProblemError { code: i32 }

effect fn failing() -> i32 ! ProblemError {
  fail ProblemError { code: 42 }
}

effect fn protected()
  -> i32 ! ProblemError {
  return run Effect.suspend(failing())
}
```

`ProblemError` passes through unchanged. Suspension adds no failure or requirement member. Exhausting
private execution storage is a fatal trap and cannot be recovered with `Effect.catch`.

### Dynamic non-suspending branch

An Effect classified as suspendable may take a branch that completes before reaching
`Effect.suspend`. That invocation performs no suspension transfer or nested execution-stack growth.
Whether the backend removes unused frame fields or dispatch on that path is an unobservable
optimization.

### Mutual recursion

It is sufficient for every cycle—not every call edge—to cross the boundary. If `even` calls `odd`
directly and `odd` returns through `Effect.suspend(even(...))`, the mutual cycle is covered. A cycle
analysis diagnostic may explain an uncovered path, but the language does not insert a boundary.

### Expected diagnostics

- `Effect.suspend` produces no allocator-provision or storage-failure diagnostic because neither is
  part of its public contract.
- Invalid movement, duplication, borrowing, or cleanup across the boundary reports the ordinary
  ownership diagnostic at the source operation responsible.
- The compiler does not promise to prove that a recursive program will overflow. The LSP may warn
  when it finds a recursive Effect cycle with no suspension boundary and offer an explicit edit.

## Semantic sketch

1. Build the deferred child Effect without executing it.
2. Execute ordinary code directly until an explicit suspension origin is reached.
3. Move the current invocation's live post-child state into its next coroutine-frame state and
   return transfer to the iterative driver.
4. Grow or reuse the compiler-owned execution stack for a dynamically nested child. Exhaustion is a
   fatal trap; it is not delivered to the Effect as a typed failure.
5. The private iterative driver starts the child.
6. On success or typed failure, resume logical callers in order with the exact outcome and retained
   state.
7. Move or clean every retained value exactly once, then release completed execution frames.
8. A fatal trap keeps Silk's general no-unwind rule; suspension does not introduce trap cleanup.

The evaluator counts a suspended logical invocation as active source-logical call depth even though
it no longer occupies JavaScript stack. Compiler-generated driver and resume helpers do not add
source-logical depth. This preserves deterministic evaluation limits and honest logical traces.

## Compiler–standard library boundary

### Compiler necessity

Ordinary Silk cannot replace active machine call frames, derive exact live state after lowering,
resume target control flow, or guarantee bounded target stack through a library function alone.

### Smallest target-neutral primitive

One sealed operation accepts an owned deferred Effect and requests a stack-safe child transfer while
preserving its success, failure, and requirement channels exactly. It exposes no continuation,
pending token, execution-stack allocator, scheduler, fiber, or target ABI.

### Standard-library construction

The canonical `Effect.suspend` declaration is ordinary Silk over that intrinsic. Generic channel
lifting, actor naming, imports, documentation, and composition remain library responsibilities.
User code may define functions with the same spelling without acquiring intrinsic privilege.

### Privilege audit

The compiler may know that an intrinsic execution edge can transfer and may generate private target
continuations. It must not recognize `Effect.suspend`, `Effect.map`, an allocator implementation, or
another public declaration by name. It must not add scheduler machinery or universal Effect
interpretation to call graphs that cannot reach the intrinsic.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Not affected | The canonical operation is an explicitly imported ordinary function; no new keyword is required. |
| Types and abstraction | Affected | The child preserves `A`, `E`, and `R` exactly; private execution storage never widens either channel. |
| Execution contracts | Affected | `run` may internally transfer and resume while retaining one public Effect abstraction. |
| Ownership and resources | Affected | Live owners, borrows, provider access, state transitions, cleanup, and execution-frame ownership cross private states. |
| Runtime and targets | Affected | Native and Wasm need bounded-stack private drivers; future parking remains separate. |
| Compiler | Affected | Reachability, suspendability, continuation liveness, MIR verification, and target lowering participate. |
| Standard library | Affected | One ordinary wrapper exposes the primitive; existing combinators remain suspension-unaware. |
| Tooling and diagnostics | Affected | LSP recursion warnings and fixes are useful; compiler diagnostics remain contract- and ownership-based. |
| Learning and use | Affected | Users need one simple rule: every recursive Effect cycle that must be stack safe crosses `Effect.suspend`. |

## Scope cohesion

Stack-safe transfer, coroutine state, execution-stack exhaustion, and cleanup form one coherent
feature. Async parking does not: it requires shared progress and wakeup rules absent from recursive
trampolining, so it belongs to a later proposal even if a backend reuses the same frame
representation.

## Complexity and subtraction budget

The feature earns one library operation, one sealed primitive, and a private coroutine
representation for reachable suspendable code. It does not earn public continuation types, a second
Effect type, scheduler services, pending-aware combinators, automatic transformation of all
recursion, or target-specific source APIs. Compiler-owned execution storage is the analogue of the
machine stack, not a source-visible ambient allocator or dependency service.

## Surface displacement

The proposal adds no syntax. It adds a library name and a compiler execution path reachable only
through the sealed primitive. It displaces an implicit assumption—that all Effect calls use ordinary
machine recursion—with an explicit boundary chosen at recursive cycles.

## Drawbacks and risks

- Programmers must recognize recursion cycles and place an explicit boundary.
- Execution-storage exhaustion is fatal rather than recoverable through the Effect failure channel.
- Suspendability analysis and continuation liveness substantially increase compiler complexity even
  though the public surface is small.
- A compiler defect in state transitions or retained ownership can cause leaks, double cleanup, or
  invalid borrows far from the source boundary.
- The word “suspend” can suggest async parking, making the narrow contract easy to misunderstand.
- A future execution/fiber model may expose configurable or fallible execution-stack ownership, but
  it must not retroactively add channels to this primitive.

## Alternatives and prior art

### Status quo

Leave recursion on the target call stack and document implementation limits. This is simple, but a
terminating typed Effect can still abort differently by engine and depth.

### Smaller primitive or library solution

Use a source-level manual loop with an explicit user-defined state enum. This can be allocation-free
and predictable, but it forces every recursive program and combinator stack to expose and interpret
its own continuation state.

### Strongest competing language model

Compile every Effect into a universal resumable state machine. This makes later parking easier, but
imposes frame representation, dispatch, and runtime policy on synchronous Effects. The proposed
model instead transforms only concrete execution paths that can reach suspension and leaves future
scheduling policy undefined.

### Rejected source-allocation model

The implemented design currently allocates separate continuation records through a source-selected
`Allocator` and adds `OutOfMemoryError ? &mut Allocator` to `Effect.suspend`. It makes every allocation
observable and recoverable, but it confuses coroutine-frame placement with Effect business
dependencies, allocates per active continuation rather than reusing one invocation frame, and lets a
compiler lowering decision infect otherwise unrelated failure and requirement rows. This proposal
rejects that model.

The selected model treats the coroutine execution stack like the ordinary machine stack: finite,
compiler-owned, and fatal on exhaustion. A future explicit task or fiber constructor may offer
configurable or fallible storage policy, just as a thread constructor may configure a machine stack,
without changing the type of every function that calls or suspends within it.

## Falsifiers and acceptance blockers

- A recursive cycle that crosses the explicit boundary still grows native or Wasm machine stack
  with logical depth.
- A non-suspending Effect acquires coroutine frames, dispatch, scheduler linkage, or widened channels
  merely because suspension exists elsewhere.
- Ordinary source combinators must expose or inspect a pending representation.
- Typed success or failure changes while crossing a boundary.
- A live affine value can be duplicated, leaked, or cleaned twice across transfer, resume, success,
  or typed failure.
- Target engines disagree about evaluation result, retained ownership, or cleanup order.
- The public contract accidentally promises parking, fairness, concurrency, or cancellation.

## Open realization questions

- Whether the private execution stack uses segmented memory, an arena, target coroutine storage, or
  another strategy; no exact allocation event is source-observable.

## Future directions

A later proposal may define runtime parking and wakeup, executors, structured child tasks,
interruption, queues, deferred values, semaphores, latches, and concurrent streams. It may reuse
target-neutral continuation descriptions and private drivers, but reuse is an implementation choice.
That proposal must independently define registration-before-park, lost-wakeup prevention, ownership
of queued and parked values, cancellation cleanup, provider access while dormant, scheduling policy,
zero scheduler cost when unreachable, and whether an explicit task owner offers configurable or
fallible execution storage.

## OpenSpec realization map

The accepted direction would reconcile the existing suspension requirements across Effect flow,
the sealed intrinsic boundary, target-neutral MIR, ownership and cleanup, deterministic evaluation,
and native/Wasm backends. It would remove source-owned continuation allocation and its widened rows.
Because contradictory requirements and an implementation already exist, the handoff is a corrective
change rather than an assumption that the current artifacts are authoritative.

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-19 | Recovered the recursion-driven intent from the historical discussion and implementation; separated stack-safe transfer from future parking; documented the existing behavior and kept continuation allocation, naming, logical depth, and diagnostics open for review. |
| 2 | 2026-08-19 | Restored the coroutine-storage decision from the original discussion: one statically shaped reusable frame per invocation, compiler-owned dynamic execution stack, fatal exhaustion, and no `OutOfMemoryError` or `Allocator` added by `Effect.suspend`; identified the later explicit-allocation OpenSpec and implementation as drift to correct. |
| 3 | 2026-08-19 | Author confirmed SUSP-001–006, including exact preservation of `A ! E ? R`, no public allocator or allocation failure, one reusable statically shaped frame per invocation, and fatal execution-stack exhaustion. |
| 4 | 2026-08-19 | Author confirmed SUSP-007–010: deferred child start after a complete parent transition, suspension-transparent ordinary combinators, one reusable frame per invocation, and one statically known maximum layout over its possible resume states. |
| 5 | 2026-08-19 | Author confirmed SUSP-011–015: ordinary affine and borrow rules across stable frame states, complete ownership transitions, exact structured cleanup, execution-owned unobservable frame placement, and no allocator-specific suspension restriction. |
| 6 | 2026-08-19 | Author confirmed SUSP-016–020: suspended invocations retain logical `CallDepth`, engines preserve typed and ownership parity, non-suspending graphs pay no coroutine cost, uncovered recursion remains valid with optional LSP guidance, and future runtime parking remains separate. |
| 7 | 2026-08-19 | Promoted the fully reviewed SUSP-001–020 direction to Candidate with the author's explicit acceptance. |
| 8 | 2026-08-19 | Corrected the stable generic spelling to use ordinary failure type parameter `E`; the current widened signature retains obsolete `!E` only as evidence of implementation drift. This aligns suspension with the confirmed Effect contract without changing SUSP-001–020 semantics. |
