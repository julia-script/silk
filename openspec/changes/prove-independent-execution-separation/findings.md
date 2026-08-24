# Independent execution separation findings

Status: **pressure evidence only; not a canonical concurrency API**

The fixtures under `examples/language-pressure/independent-execution-separation` demonstrate that
ordinary Silk source can assemble owners around the sealed Execution/Wake substrate. Names such as
Scheduler, Deferred, Timer, and Coroutine describe test pressure shapes only. They grant no
compiler privilege and make no API-selection decision.

## Selected guarantees and pressure evidence

| Guarantee | Evidence | Observation |
| --- | --- | --- |
| Initial ownership | `first-activation.silk`; connected-owner construction quota sweep | Construction transfers two distinct exact Effect representations into homogeneous Initial Execution values without starting either body. The owner selects the second value first, and dropping the never-started first value invokes no callback. Every fallible construction ordinal completes before task publication. |
| Task-specific push readiness | `main.silk`; `selective-ready.silk` | A fixed endpoint publishes only its pre-reserved task identity after Wake consumption and after Shared access ends. Waking task 2 among three parked tasks does not inspect unrelated dormant Executions; an eligible task may be destroyed and its queued identity consumed as a stale tombstone. |
| Recoverable package admission | connected-owner construction quota sweep; `post-publication-failure.silk` | Shared cells, result state, reservations, and exact Execution packages are admitted before publication. Allocation refusal remains an ordinary typed failure with balanced cleanup and publishes no partial task. A later waiter allocation failure begins no park and leaves an already-published Initial task valid. |

These observations select deferred first activation, push readiness, and recoverable package
admission. They reject the smaller eager-start/owner-sweep/fatal-package model: eager start removes
the owner's activation choice, sweeping dormant executions makes readiness an owner policy scan,
and treating package admission as fatal removes the source allocator's typed refusal contract. No
fallback or compatibility implementation of that rejected model remains.

## Privilege inventory

The semantic, HIR, MIR, evaluator, native, and direct-Wasm implementation paths contain no checks
for pressure-actor spellings. External parking is derived from the closed `ExecutionPark` builtin
operation identity in `ExecutableOrigin.ts`; Execution and Wake types are sealed Intrinsic
identities. Internal names such as `CoroutineFrame` describe compiler representation and do not
recognize a source declaration.

The connected Scheduler/Deferred, timer/reactor, bounded alternate-owner, and selective-ready
fixtures were each renamed through the landed local-shared normalization harness. Semantic facts,
normalized MIR, evaluator results, direct-Wasm results, and intrinsic inventory remained equivalent.
The pay-for-use matrix separately demonstrates that Shared capture selects `LocalExecution` but no
independent package, while a statically park-capable body retains its external tier even when its
executed branch is direct.

## Explicitly deferred boundaries

- Concurrent Wake delivery, thread transfer, atomics, and parallel-memory ordering remain future
  language work. Every pressure fixture is single-threaded.
- A canonical Coroutine, Scheduler, Fiber, Deferred, Timer, reactor, queue, or task API remains
  unselected. The fixtures deliberately stay outside the standard library.
- Implicit executable-root ownership belongs to SLP-0003. This layer keeps unowned park-capable
  complete entries rejected and synthesizes no root owner.
- Structured concurrency, interruption propagation, fairness, parallel scheduling, and worker
  memory policy remain separate directions.

