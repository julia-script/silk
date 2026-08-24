# Independently resumable Effect executions

The `silk.execution` module packages one lazy Effect so an ordinary source owner can select its
first activation, receive it back when it relinquishes control, and drive it again after external
readiness. This is explicit local execution ownership. It does not create a scheduler, fiber,
thread, queue, timer, or implicit program-entry owner.

These rules implement SLP-0001. They extend the nested-transfer model in
[Effect suspension](effect-suspension.md); `Effect.suspend` remains stack-safe composition inside
one activation, while `Execution.park` returns control to an owner outside the body.

## Terms

- An **Execution** is one affine package containing a lazy body, private continuation state, and a
  fixed readiness endpoint.
- An **activation** is one legal call to `Execution.drive`.
- The **branch state** is owner data transferred to exactly one drive-outcome callback.
- A **Wake** is the affine readiness authority for one parked generation.
- The **registration guard** is the value returned by `Execution.park`'s registration callback and
  retained until that generation resumes or is destroyed.

## Public operations

```silk,ignore
import silk.execution as Execution

Execution.make(body, readyState, onReady)
Execution.drive(move execution, move branchState, onComplete, onSuspend)
Execution.park(register)
```

`make` returns `Intrinsic.Execution<A>`. `drive` consumes that value and returns continued
ownership only through `onSuspend`. `park` is called inside the running body.

### EXEC-001 — Construction is lazy, explicit, and caller-funded

**Status:** Confirmed

`Execution.make` transfers the body and fixed readiness endpoint into one combined package obtained
through the selected `Core.Allocator`. It returns the package in the `Initial` state without
starting the body.

```silk,ignore
let creating = Execution.make(body(), readyState, onReady)
  |> Effect.provideMut<Core.Allocator>(&mut allocator)
let execution = run creating
```

Construction failure is `Core.OutOfMemoryError`. It publishes no Execution and leaves every input
under ordinary Effect cleanup. Later growth of the compiler-owned execution stack is a fatal trap,
not an allocator requirement or typed failure.

**Boundary:** Importing `silk.execution` and constructing ordinary Effects allocate no execution
package. Only `Execution.make` selects this storage boundary.

**Diagnostics:** An endpoint that is not detached and non-parking reports `SEM0139` at the failed
property application. A package layout mismatch at the unsafe intrinsic boundary reports
`SEM0142`.

**Evidence:** [ordinary source implementation](../../packages/compiler/stdlib/silk/execution.silk),
[package tests](../../packages/compiler/test/ExecutionPackage.test.ts).

### EXEC-002 — The owner drives only Initial or Eligible executions

**Status:** Confirmed

`Execution.drive` consumes an `Initial` or `Eligible` Execution and transfers the supplied branch
state to exactly one take-once outcome callback:

- completion invokes `onComplete(branchState, result)`;
- relinquishment invokes `onSuspend(branchState, execution)`.

Nested `Effect.suspend` transfer may finish within the same activation. External parking
relinquishes the Execution instead.

**Boundary:** `Running`, `Dormant`, `Notifying`, `DestroyPending`, `Completed`, and `Destroyed`
executions are not driveable. Attempting to drive one is a fatal state trap before either outcome
callback runs. There is no recoverable “not ready” result.

**Diagnostics:** Ordinary use after `drive` consumes an affine Execution reports `OWN0001`.
Lifecycle admission failure occurs at runtime as a fatal trap outside Effect failure channels.

**Evidence:** [external parking tests](../../packages/compiler/test/ExternalWakeParking.test.ts),
[lifecycle model](effect-suspension.md#independent-execution-and-external-parking).

### EXEC-003 — Parking creates one generation-scoped Wake

**Status:** Confirmed

`Execution.park(register)` transfers one Wake to `register`. If the Wake is signaled before
registration returns, readiness is latched until the registration handoff is complete. Otherwise
the Execution becomes `Dormant`. A successful signal makes that generation `Eligible` and invokes
the fixed readiness endpoint at most once.

The registration callback's return value is retained as the generation guard. The guard is cleaned
immediately before the body continues after `park`, or during destruction if the generation never
continues.

**Boundary:** Wake signaling publishes readiness; it never drives the Execution inline. Signaling
or dropping a Wake consumes it. A stale or duplicate Wake cannot publish a second readiness event.

**Diagnostics:** Reusing a consumed Wake reports the ordinary affine `OWN0001` diagnostic. Parking
while an incompatible local-shared access loan is live reports `OWN0016` before lowering.

**Evidence:** [wake transition actor](../../packages/compiler/src/WakeCell.ts),
[external parking tests](../../packages/compiler/test/ExternalWakeParking.test.ts).

### EXEC-004 — Executions are local affine owners

**Status:** Confirmed

`Intrinsic.Execution<A>` and `Intrinsic.Wake` are affine and `LocalExecution`. They may move among
ordinary values, Effects, callbacks, and independently resumable frames in one local execution
domain. They are not Copy and have no thread-transfer operation.

Construction accepts only a detached body and detached endpoint state. A detached executable owns
its captured values but retains no caller lexical or provider loan. Stable package-internal loans
may cross parking; a result cannot return a view into package-owned storage.

**Boundary:** `LocalExecution` permits same-thread suspension, parking, and owner transfer. It does
not imply thread safety, atomics, locking, parallel execution, or eligibility for a future thread
transfer operation.

**Diagnostics:** Unsatisfied `Intrinsic.Detached` or `Intrinsic.NonParking` properties report
`SEM0139` with the retained capture, provider, or parking path. Missing explicit ownership of a
park-capable Effect reports `SEM0140`.

**Evidence:** [ownership reference](ownership-and-borrowing.md),
[execution affinity tests](../../packages/compiler/test/LocalSharedPressure.test.ts).

### EXEC-005 — Cancellation and completion clean exactly once

**Status:** Confirmed

Completion cleans the consumed outcome callback, fixed endpoint, body or continuation-owned values,
and package allocation in their canonical ownership order. Dropping a dormant Execution cancels
its Wake authority and cleans suspended state exactly once.

If external source still owns the cancelled Wake, that Wake retains only the complete inert package
allocation. Signaling or dropping it performs no notification or redrive and releases the final
authority.

**Boundary:** A retained cancelled Wake can extend allocation lifetime. It cannot keep the body,
guard, endpoint, or suspended payload logically active, and it cannot resurrect the Execution.

**Diagnostics:** Cleanup does not add a typed failure. Fatal traps use Silk's non-unwinding trap
contract and do not promise post-trap cleanup.

**Evidence:** [cleanup ownership model](../../packages/compiler/src/SuspensionOwnership.ts),
[separation pressure tests](../../packages/compiler/test/LocalSharedPressure.test.ts).

### EXEC-006 — Scheduling policy remains ordinary source

**Status:** Confirmed

The compiler recognizes only the sealed target-neutral Execution, Wake, and parking primitives.
Schedulers, fibers, deferred values, timers, reactors, ready queues, fairness, cancellation policy,
and structured concurrency are ordinary source concepts built over those primitives and
[`Shared`](local-shared-ownership.md).

**Boundary:** SLP-0001 provides explicit independently resumable local executions. It does not make
a park-capable `main` implicitly owned; that is a separate language decision.

**Diagnostics:** No Scheduler, Fiber, Deferred, Timer, or concurrency service requirement is
inferred by `Execution.make`, `drive`, or `park`.

**Evidence:** [minimal compiler privilege](runtime-and-standard-library.md#stdlib-001--public-standard-library-declarations-receive-no-compiler-privilege),
[actor-neutrality pressure tests](../../packages/compiler/test/LocalSharedPressure.test.ts).
