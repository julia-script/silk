# Effect suspension and stack-safe recursion

`Effect.suspend` is an explicit stack-safety boundary for Effect execution. It transfers one
deferred child through private execution machinery so a covered recursive Effect cycle can use
bounded native and WebAssembly machine stack.

Nested suspension is not external parking. `Effect.suspend` does not park a task, let another task
run, wait for a wakeup, or introduce a scheduler. Explicit independently owned executions provide
the separate [external-parking boundary](independent-effect-executions.md#external-parking-and-readiness).
Suspension also does not make ordinary recursion stack safe automatically.

The intended public contract is:

```silk,ignore
effect fn suspend<A, E, ?R>(
  deferred: once Effect<A ! E ? R>
) -> A ! E ? R
```

The compiler and standard library preserve that contract exactly. Coroutine frames use private
execution-stack storage; they never select a source `Allocator` or add `OutOfMemoryError` to an Effect.

Rule numbers follow the accepted proposal. SUSP-009, SUSP-010, SUSP-012, and SUSP-014 constrain
private lowering rather than programmer-observable Silk, so their details remain in the proposal
instead of appearing as standalone language-reference rules.

## Terms

- **Deferred child** — the owned Effect passed to `Effect.suspend`. Constructing the suspension does
  not execute the child body.
- **Suspension origin** — the explicit `Effect.suspend` boundary that transfers execution.
- **Suspendable caller** — a concrete Effect execution path that can reach a suspension origin. It
  may still complete along another branch without suspending.
- **Resume** — continue the logical caller with the child's success or typed failure. It does not
  imply a thread, task, or scheduler.
- **Execution stack** — finite compiler-owned storage for logically active suspended calls. It is
  distinct from the source-selected `Allocator` service and from the physical machine stack.
- **Parking** — leave an explicitly owned execution dormant until an external readiness signal
  occurs. `Effect.suspend` does not provide parking; `Intrinsic.park` provides the distinct boundary.

## Public contract and recursion

### SUSP-001 — Suspension is an explicit boundary

**Status:** Confirmed

Silk makes an Effect execution path stack safe only where source explicitly calls
`Effect.suspend`. The compiler does not insert suspension boundaries automatically.

```silk,ignore
effect fn count(value: i32) -> i32 {
  if value == 0 {
    return 0
  }
  let inner = run Effect.suspend(count(value - 1))
  return inner + 1
}
```

**Boundary:** Merely declaring `effect fn` or calling a recursive Effect does not request the
bounded-machine-stack path.

**Diagnostics:** A valid explicit boundary produces no diagnostic. Missing boundaries remain valid
under SUSP-019 rather than becoming a compiler error.

**Evidence:** [sealed intrinsic boundary](unsafe-intrinsics-and-targets.md#intr-001--intrinsic-is-sealed-compiler-identity-available-to-every-source-module),
[suspension implementation contract](../../openspec/changes/archive/2026-08-19-align-effect-suspension-coroutine-storage/proposal.md).

### SUSP-002 — Running a suspended Effect produces the child's ordinary outcome

**Status:** Confirmed

Running `Effect.suspend(child)` eventually produces the child's one success value or typed failure.
It does not expose a nested Effect, pending result, continuation, or resume token.

```silk,ignore
let value = run Effect.suspend(effect { return 42 })
```

`value` is `i32`, not `Effect<i32>` or a continuation handle.

**Boundary:** Suspension does not flatten an Effect returned as the child's declared success value.
If the child succeeds with `Effect<i32>`, one `run` still produces that nested Effect value under the
ordinary one-layer rule.

**Diagnostics:** Incompatible use of the child's success or failure receives the ordinary Effect,
return, or type diagnostic.

**Evidence:** [one-layer execution](effects-and-execution.md#eff-003--run-executes-exactly-one-effect-layer).

### SUSP-003 — Every recursive cycle must cross suspension for bounded machine stack

**Status:** Confirmed

A terminating self-recursive or mutually recursive Effect graph uses bounded native and Wasm
machine stack when every possible recursive cycle crosses an explicit suspension origin.

```silk,ignore
effect fn even(value: i32) -> bool {
  if value == 0 {
    return true
  }
  return run odd(value - 1)
}

effect fn odd(value: i32) -> bool {
  if value == 0 {
    return false
  }
  return run Effect.suspend(even(value - 1))
}
```

The mutual cycle crosses the boundary in `odd`; every individual call edge need not suspend.

**Boundary:** A suspension on an unrelated branch does not cover a recursive cycle that can avoid
that branch.

**Diagnostics:** The compiler does not reject an uncovered cycle. The language service may explain
which cycle lacks a boundary under SUSP-019.

**Evidence:** [suspendability analysis](../../packages/compiler/test/Suspendability.test.ts).

### SUSP-004 — Suspension does not cover unrelated recursion

**Status:** Confirmed

`Effect.suspend` gives no stack-safety guarantee to ordinary function recursion, Effect recursion
whose cycle does not cross the boundary, recursive `Drop`, or arbitrary recursive data traversal.

```silk,ignore
effect fn count(value: i32) -> i32 {
  if value == 0 {
    return 0
  }
  return 1 + run count(value - 1)
}
```

**Boundary:** A target may happen to optimize a particular recursive call. That optimization does
not become a Silk guarantee and cannot be relied on for correctness.

**Diagnostics:** These recursive programs remain valid and receive no mandatory compiler
diagnostic solely because their depth is unbounded.

**Evidence:** [suspendability analysis](../../packages/compiler/test/Suspendability.test.ts).

### SUSP-005 — Suspension preserves success, failure, and requirement channels exactly

**Status:** Confirmed

For `Effect.suspend<A, E, ?R>`, the result keeps exactly `A`, `E`, and `R`. Suspension adds no
allocation error or allocator service to the public contract.

```silk,ignore
effect fn protected() -> i32 ! ProblemError ? &Clock {
  return run Effect.suspend(work())
}
```

If `work()` has `i32 ! ProblemError ? &Clock`, `protected` has the same three channels.

**Boundary:** Exhausting private execution storage is a fatal trap under SUSP-006, not a hidden
member of `E`.

**Diagnostics:** `Effect.suspend` produces no allocator-provision or storage-failure diagnostic.
Existing child-channel mismatches retain their ordinary codes.

**Evidence:** [Effect channels](effect-contracts.md),
[suspension composition tests](../../packages/compiler/test/EffectSuspensionComposition.test.ts).

### SUSP-006 — Execution-stack exhaustion is a fatal trap

**Status:** Confirmed

Exhausting the finite compiler-owned execution stack terminates with a fatal trap outside the typed
failure channel, like exhausting the ordinary machine stack.

**Boundary:** `Effect.catch`, `catchAll`, `result`, or another typed-failure combinator cannot recover
execution-stack exhaustion. Explicit Execution construction has separate caller-funded initial
packaging, but later continuation growth retains this fatal policy. See
[IEXEC-006](independent-effect-executions.md#iexec-006--pending-packaging-failure-and-stack-exhaustion-remain-distinct-outcomes).

**Diagnostics:** No failure member or requirement is inferred. A reached exhaustion reports a fatal
runtime trap according to the program-termination rules.

**Evidence:** [fatal traps](typed-failures.md#fail-007--a-trap-is-fatal-and-remains-outside-effect-outcomes),
[execution-storage requirements](../../openspec/changes/archive/2026-08-19-align-effect-suspension-coroutine-storage/specs/bootstrap-evaluation/spec.md).

## Execution and composition

### SUSP-007 — The child starts only after a complete transfer

**Status:** Confirmed

Constructing `Effect.suspend(child)` does not execute the child. When the suspended Effect is run,
the current invocation first completes the state and ownership transition required to resume later;
only then may the child begin.

```silk,ignore
let deferred = Effect.suspend(effect {
  return observe()
})
```

`observe()` does not run until `deferred` is run.

**Boundary:** The child cannot observe a half-moved parent state or begin before live parent values
have one valid owner for later resumption.

**Diagnostics:** Premature construction causes no runtime work. Invalid captures or transfers
receive their ordinary ownership diagnostic.

**Evidence:** [Effect construction](effects-and-execution.md#eff-001--calling-an-effect-function-constructs-an-effect),
[Effect suspension standard-library tests](../../packages/compiler/test/EffectSuspendStdlib.test.ts).

### SUSP-008 — Ordinary combinators are suspension-transparent

**Status:** Confirmed

Ordinary source-defined Effect combinators compose with suspendable children without inspecting a
pending state or changing their public signatures.

```silk,ignore
fn increment(value: i32) -> i32 {
  return value + 1
}

let deferred = Effect.suspend(effect { return 41 })
let answer = run (move deferred |> Effect.map(increment))
```

`answer` is `42`; `Effect.map` need not expose continuation machinery.

**Boundary:** A combinator still obeys its ordinary ownership, failure, and requirement contract.
Suspension transparency does not authorize it to duplicate or retain a take-once child.

**Diagnostics:** Invalid composition receives ordinary callable, Effect-channel, or ownership
diagnostics rather than a suspension-specific error.

**Evidence:** [Effect composition](effects-and-execution.md),
[suspension composition tests](../../packages/compiler/test/EffectSuspensionComposition.test.ts).

## Ownership and lifecycle

### SUSP-011 — Ordinary ownership and loans continue across suspension

**Status:** Confirmed

Copy values may copy, affine values move into one later-execution owner, and shared or exclusive loans
retain their ordinary access and lifetime rules while the logical caller is suspended. A referent
borrowed across suspension keeps a stable logical location for the loan's lifetime.

```silk,ignore
effect fn inspect(value: &Record) -> i32 {
  return run Effect.suspend(effect { return value.count })
}
```

The shared loan remains a shared loan for the complete suspended call; suspension neither consumes
the referent nor permits overlapping mutation.

**Boundary:** Suspension introduces no public `Pin` type or permission to move a borrowed referent.
The compiler's private representation must adapt to source-valid borrows, not invalidate them.

**Diagnostics:** Invalid duplication, movement, overlapping access, or escaping loans report the
ordinary ownership and borrowing diagnostics at the responsible source operation.

**Evidence:** [ownership rules](ownership-and-borrowing.md),
[suspension ownership tests](../../packages/compiler/test/SuspensionOwnership.test.ts).

### SUSP-013 — Suspension preserves exact structured cleanup

**Status:** Confirmed

On success or typed failure, every value retained across suspension is moved onward or cleaned
exactly once. A fatal trap retains Silk's general no-unwind and no-cleanup guarantee.

```silk,ignore
effect fn useOwned(resource: Resource) -> () ! UseError {
  run Effect.suspend(effect { consume(move resource) })
}
```

`resource` has one owner throughout the transfer and is consumed or cleaned exactly once.

**Boundary:** Suspension does not cause `Drop`, `ensuring`, or other cleanup to run after a trap. It
also cannot skip ordinary structured cleanup merely because execution resumed through a driver.

**Diagnostics:** Invalid source cleanup ownership receives its ordinary diagnostic. No additional
cleanup operation appears in source merely because a function is suspendable.

**Evidence:** [Effect lifecycle](ownership-and-borrowing.md#effect-life-001--effect-execution-cleans-per-run-state-and-preserves-reusable-captures),
[trap cleanup](ownership-and-borrowing.md#trap-001--a-trap-has-no-cleanup-guarantee).

### SUSP-015 — Suspension creates no special allocator implementation rule

**Status:** Confirmed

Because `Effect.suspend` does not select or call a source allocator, it imposes no special
recursion, bootstrap, or self-hosting restriction on ordinary `Allocator` implementations.

```silk,ignore
struct SuspendingAllocator {}

effect fn allocate(
  self: &mut SuspendingAllocator,
  layout: Layout
) -> Allocation ! OutOfMemoryError {
  return run Effect.suspend(effect {
    return run Intrinsic.systemAllocationAcquire(move layout)
  })
}

impl Allocator for SuspendingAllocator {
  allocate: SuspendingAllocator.allocate
}
```

If this implementation satisfies the ordinary `Allocator` contract, its use of suspension adds no
extra conformance rule.

**Boundary:** An allocator operation may independently be recursive or effectful and follows its
own declared contract. An explicit Execution wrapper procures its initial exact package through
ordinary source allocation, but `Effect.suspend` itself retains no Allocator contract.

**Diagnostics:** No allocator-specific conformance or recursion diagnostic applies merely because
an allocator is reachable from suspendable code.

**Evidence:** [service rules](requirements-and-services.md),
[allocator-independence requirements](../../openspec/changes/archive/2026-08-19-align-effect-suspension-coroutine-storage/specs/bootstrap-owned-allocation/spec.md).

## Engines, limits, and tooling

### SUSP-016 — Suspended invocations still count toward logical CallDepth

**Status:** Confirmed

An invocation remains a live logical source call while its child runs through suspension. It
therefore continues to count toward the evaluator's `CallDepth` limit. Compiler-generated helpers
do not add logical calls.

```silk,ignore
let result = evaluate(count(10_000), limits: { CallDepth: 100 })
```

Suspension can keep the physical machine stack bounded without making this logical depth smaller.

**Boundary:** Logical depth is not physical JavaScript, native, or Wasm stack depth. Suspension may
bound the physical stack while the logical depth continues growing honestly.

**Diagnostics:** Reaching an evaluator `CallDepth` limit reports the existing deterministic
evaluation-limit outcome and the active logical source frames, not private helper frames.

**Evidence:** [evaluation limits](../../openspec/specs/bootstrap-evaluation/spec.md),
[suspension evaluation tests](../../packages/compiler/test/EffectSuspensionEvaluation.test.ts).

### SUSP-017 — Evaluation, native, and Wasm preserve the same semantics

**Status:** Confirmed

The evaluator, native execution, and direct Wasm produce the same typed outcome, retained
ownership, and cleanup order for a suspended program. Native and Wasm additionally guarantee
bounded machine stack for cycles covered by SUSP-003.

For the `count` example in SUSP-001, every engine must produce the same integer or typed failure;
engine-specific execution machinery cannot become part of that result.

**Boundary:** Engines may use different private execution representations or storage growth
policies. Those differences cannot change source-visible results or cleanup.

**Diagnostics:** A valid program receives no engine-selection diagnostic. A target that cannot
honor the suspension contract is unavailable for that reachable executable closure.

**Evidence:** [target availability](unsafe-intrinsics-and-targets.md#target-003--target-unavailability-is-a-compile-time-compatibility-error),
[native suspension tests](../../packages/compiler/test/EffectSuspensionNative.test.ts),
[Wasm suspension tests](../../packages/compiler/test/EffectSuspensionWasm.test.ts).

### SUSP-018 — Non-suspending call graphs pay no coroutine cost

**Status:** Confirmed

A closed executable call graph that cannot reach the suspension intrinsic contains no coroutine
frame transformation, private suspension driver, pending branch, scheduler, or widened Effect
channels merely because another program can suspend.

```silk
pub fn main() -> i32 {
  return 42
}
```

This program acquires no suspension machinery.

**Boundary:** A graph that can reach suspension may need a suspendable private path even when one
runtime branch completes before reaching the boundary.

**Diagnostics:** No source diagnostic applies. Artifact inspection and pay-for-use tests verify the
absence of unreachable machinery.

**Evidence:** [pay-for-use runtime rule](runtime-and-standard-library.md#runtime-002--source-closure-and-executable-closure-control-separate-costs),
[suspension MIR tests](../../packages/compiler/test/SuspensionMir.test.ts).

### SUSP-019 — Uncovered recursive Effects remain valid

**Status:** Confirmed

A recursive Effect cycle without `Effect.suspend` is valid Silk. The compiler does not reject it
merely because sufficiently deep execution may exhaust the target machine stack.

```silk,ignore
effect fn count(value: i32) -> i32 {
  if value == 0 { return 0 }
  return 1 + run count(value - 1)
}
```

**Boundary:** The language service may warn when it finds an uncovered cycle and offer to insert an
explicit suspension boundary. That assistance cannot silently edit source or claim every execution
will overflow.

**Diagnostics:** No mandatory compiler diagnostic applies. Any LSP warning is non-blocking and must
identify the uncovered cycle and the explicit nature of the suggested change.

**Evidence:** [suspendability analysis](../../packages/compiler/test/Suspendability.test.ts).

### SUSP-020 — Nested suspension promises no external-parking or scheduler behavior

**Status:** Confirmed

`Effect.suspend` provides no parking, wakeup, fairness, parallelism, interruption, cancellation, or
scheduler semantics. It remains deterministic single-execution Effect composition.

```silk,ignore
let value = run Effect.suspend(readNext())
```

This transfers stack-safe execution of `readNext`; it does not wait for another task to publish a
value unless `readNext` already has some separately defined synchronous way to complete.

**Boundary:** The accepted SLP-0001 direction for independently resumable executions defines
explicit ownership, registration, one-shot Wake readiness, and dormant cleanup; its implementation
is still in progress. It does not change `Effect.suspend` and does not select canonical streams,
queues, tasks, fibers, executors, timers, or async-I/O APIs.

**Diagnostics:** Suspension alone adds no Scheduler, Executor, or concurrency service requirement.
Using a future unavailable async construct receives that construct's own diagnostic rather than
changing `Effect.suspend`.

**Evidence:** [no ambient runtime facilities](runtime-and-standard-library.md#runtime-004--silk-has-no-ambient-runtime-facilities),
[external parking](independent-effect-executions.md),
[separate suspension operations](../../openspec/changes/add-external-wake-parking/specs/bootstrap-intrinsic-boundary/spec.md).

## Private lowering model

These are compiler architecture rules, not additional source obligations:

- One concrete suspendable invocation owns one reusable coroutine frame. Repeated suspension by
  that invocation changes its resume state; it does not allocate another continuation record.
- Every resume state names only the values needed after that transfer. One statically known maximum
  layout covers all mutually exclusive states, including compiler-generated temporaries.
- The parent completes its ownership and state transition before the deferred child begins. A live
  value therefore has one owner throughout transfer, execution, resumption, and cleanup.
- Evaluation keeps frames in its activation machine. Native uses non-moving segmented private
  storage. Direct Wasm uses a private, non-overlapping linear-memory region. Growth failure follows
  SUSP-006 on all three engines.

The complete architecture contract remains in the archived
[suspension implementation design](../../openspec/changes/archive/2026-08-19-align-effect-suspension-coroutine-storage/design.md).
