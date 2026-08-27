## 1. Prove the Vertical Root/Fork/Join Path

- [x] 1.1 Extend the existing independent-execution pressure fixture with a test-local `LocalScheduler.execute` that creates a lazy root `Execution<()>` as task zero and verify with `Analysis.evaluate` that no root source operation runs before `execute` begins driving it.
- [x] 1.2 Add a test-local owned `Scheduler` client per task plus the one-slot prepare/park/publication handshake, and verify the parent receives a published Fiber before the child records its first activation.
- [x] 1.3 Drive the fixed root and child slots through root `join` parking, child completion, one Wake notification, and root resumption, and verify the evaluator result, activation trace, and empty terminal fixture state in `LocalSharedPressure.test.ts`.
- [x] 1.4 Add vertical typed-failure and allocation-refusal branches to the fixed fixture and verify the root failure is preserved while every prepared but unpublished value is cleaned.

## 2. Add Allocation-Safe Storage Foundations

- [x] 2.1 Add callback-scoped mutable access to an existing `HashMap` value without growth or an escaping borrow, and verify present/missing/update behavior in `HashedCollections.test.ts`.
- [x] 2.2 Add ownership cases proving the HashMap callback cannot leak its value borrow or park while retaining it, and verify the exact diagnostics in `HashedCollectionOwnership.test.ts`.
- [x] 2.3 Implement monotonic per-execute `TaskId` reservation with no reuse and typed exhaustion, and verify sequential identities, overflow refusal, and fresh-run reset with evaluator assertions.
- [x] 2.4 Implement the split generic completion payload and non-generic completion signal, and verify success, typed failure, cancellation, already-complete observation, and exactly one pending waiter without allocation after construction.
- [x] 2.5 Implement the intrusive FIFO ready queue with one preallocated Shared node per task, and verify append/dequeue order, duplicate suppression, node reuse, and stale-node discard under allocator refusal during notification.
- [x] 2.6 Implement one per-task publication mailbox and response protocol, and verify successful adoption, insertion refusal, pending-submission cleanup, and nested children using distinct mailboxes.
- [x] 2.7 Implement driver-owned TaskEntry/TaskStore transitions for Initial, Running, Dormant, Eligible, and completed tasks, and verify every suspension restores the existing map entry without a fallible reinsertion.
- [x] 2.8 Implement intrusive parent/child/sibling links and the allocation-free iterative cancellation worklist, and verify deep descendant cancellation publishes `Cancelled`, removes every task, and leaves queued stale nodes harmless.

## 3. Publish the Scheduler and Fiber Actors

- [x] 3.1 Add canonical `silk.scheduler` declarations for the Scheduler service, provider-facing pending-publication protocol, `TaskIdExhaustedError`, and required internal task actors, and verify a renamed ordinary-source implementation conforms without compiler registration.
- [x] 3.2 Add canonical `silk.fiber` outcome and affine `Fiber<A, E>` declarations plus consuming `await` and `join`, and verify success, failure, cancellation, immediate completion, and pending parking through evaluator cases.
- [x] 3.3 Add allocation-free `Fiber.yieldNow` over immediate Wake notification and verify a yielding task moves behind already-ready work and resumes itself when no competitor exists.
- [x] 3.4 Add `Fiber.forkChild` over the Scheduler preparation SPI and publication handshake, and verify closed work, nested Fiber work, atomic rejection, and deferred first child activation.
- [x] 3.5 Add the target-neutral `Execution.notifyInitial` transition plus canonical `silk.local_scheduler` construction and generic `execute`, including owned root Scheduler provision, homogeneous `Execution<()>` wrapping, post-publication initial notification, FIFO dispatch, and root outcome extraction; verify exactly-once notification, deferred body activation, root success, and typed failure with generic result types.
- [x] 3.6 Add `LocalScheduler.StalledError` detection and complete typed shutdown, and verify stalled roots, root success/failure with unfinished descendants, dropped Fiber handles, escaped cancelled handles, and reuse of one scheduler value.
- [x] 3.7 Replace the fixed fixture prototypes with the canonical actors and delete superseded Deferred/Fiber/scheduler paths, and verify repository search and compilation find no compatibility alias or separate root-adapter terminology.

## 4. Harden Semantics and Failure Boundaries

- [x] 4.1 Add evaluator cases for FIFO publication order, repeated yield, multiple siblings, nested forks, child typed failure, parent typed failure, and completion-before-join, and verify each claim from one shared analysis snapshot per source program.
- [x] 4.2 Add structured-lifetime cases for parent success, failure, and cancellation with unfinished descendants, and verify descendant cancellation finishes before the next dispatched task observes the parent outcome.
- [x] 4.3 Add allocation-ordinal sweeps for execute setup, completion cells, child mailbox/node/client/Execution preparation, and task-store publication, and verify every refusal returns the documented typed error with zero leaked tasks or double cleanup.
- [x] 4.4 Add retained-cancelled-Wake and stale-ready-node reuse cases, and verify old run state remains inert and cannot enqueue into a later `execute` call.
- [x] 4.5 Verify an unowned park-capable root still reports the existing execution-boundary diagnostic and a trivial program imports no scheduler machinery, using diagnostic codes/structure rather than message text.

## 5. Verify Engines, Documentation, and Packaging

- [x] 5.1 Add the representative root/fork/join, nested-fork, cancellation, and yield programs to the shared native acceptance corpus and verify evaluator/native agreement through `DriverNativeAcceptance.test.ts` rather than per-feature native compilations.
- [x] 5.2 Add direct-Wasm legs only for independent-execution scheduling, wake, cancellation, and cleanup claims, and verify evaluator/Wasm parity without duplicating generic language-semantic assertions.
- [x] 5.3 Add public Silk doc comments for Scheduler, Fiber, LocalScheduler, outcomes, errors, and provider SPI, and verify generated standard-library documentation, hover, and navigation resolve to the canonical `.silk` declarations.
- [x] 5.4 Update the maintained language reference with explicit scheduler entry and structured Fiber semantics, omit deleted SLP workflow references, and verify documentation links and examples compile against the final API.
- [x] 5.5 Regenerate the deterministic standard-library embedding/manifest and update namespace-resolution coverage, and verify the new modules package from canonical source with no compiler-origin branch.
- [x] 5.6 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`, fixing change-caused failures and recording any exact pre-existing failure before handoff.
