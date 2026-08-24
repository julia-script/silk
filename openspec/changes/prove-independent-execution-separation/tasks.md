## 1. Connected Ordinary-Source Owners

- [x] 1.1 Build a companion test-local Scheduler-shaped owner from the landed SLP-0002 local-shared
      pressure actors, with distinct Shared TaskStore and ReadyInbox, pre-reserved task/ready slots,
      fixed endpoint state, and removal-before-drive; keep the prerequisite witness independently
      runnable and verify no Shared access spans drive or external callback invocation and no strong
      ownership cycle exists.
- [x] 1.2 Implement deferred first activation from two distinct exact Effect body representations
      erased into homogeneous `Execution<TaskOutput>` values in one owner store; verify owner-selected
      activation and never-started cleanup without callbacks.
- [x] 1.3 Implement a one-consumer Deferred-shaped waiter/producer program over Shared state and Wake;
      verify waiter park, producer publication, task-specific readiness, later owner drive, final
      value `42`, and exact result/guard/package cleanup across all engines.
- [x] 1.4 Implement the explicit outer timer driver and same-thread reactor with fallible preparation;
      verify sibling progress, local Wake delivery, reified success/failure, and no Scheduler
      requirement added to the timer operation itself.
- [x] 1.5 Add cancellation before timer readiness; verify outer and child wait cleanup, late timer
      Wake no-op behavior, no outer readiness publication, and no legal redrive.
- [x] 1.6 Implement a bounded Coroutine-shaped shared port with two yields, completion, and drop while
      yielded; verify it reuses the same Execution/Wake lifecycle with no Scheduler-specific facts.
- [x] 1.7 Park several homogeneous tasks, wake exactly one, and verify its pre-reserved identity is
      published and selected without inspecting or scanning unrelated dormant executions.
- [x] 1.8 Reject scheduling a nested-join child that retains Scheduler or Allocator requirements;
      verify the closed/Detached diagnostic preserves the provider cause, no inheritance occurs, and
      the acyclic ReadyInbox-only endpoint topology still reaches final release.

## 2. Publication and Allocation Evidence

- [x] 2.1 Implement task reservation rollback so all fallible Shared, result, queue, and exact
      package allocations finish before observational publication; verify success publishes one
      complete Initial task and failure publishes none.
- [x] 2.2 Extend the landed local-shared failure-quota harness to sweep every exercised
      construction-failure ordinal in evaluation and Wasm and native boundary ordinals through the
      designated corpus; verify each prior affine value and Allocation is cleaned once and
      subsequent runs remain deterministic; separately fail post-publication waiter allocation and
      verify no park begins and existing tasks remain valid.
- [x] 2.3 Inspect park, Wake consumption, endpoint notification, and ready-identity publication;
      verify no allocator access or failure edge exists and unknown callbacks run only after Shared
      access ends.
- [x] 2.4 Add queued-ready then eligible-destroy coverage; verify source consumes a stale identity or
      tombstone without accessing freed Execution storage or invoking compiler policy.

## 3. Pay-for-Use and Privilege Gates

- [x] 3.1 Add minimal ordinary-direct, ordinary-nested-only, explicit-direct, explicit-nested-only,
      and explicit-external-park programs plus ordinary direct/nested variants that capture a local
      Shared handle, and inspect normalized MIR/runtime-slice inventories; verify Shared capture
      publishes `LocalExecution` without selecting independent Execution machinery, each
      configuration includes only its static suspension and ownership machinery, and explicit
      nested-only execution completes through one drive with no Wake/notification state.
- [x] 3.2 Verify a dynamically direct path inside a statically park-capable Execution retains the
      external tier while owner-side drive code remains NonParking; assert structural identities,
      not byte, timing, or instruction counts.
- [x] 3.3 Rename every Scheduler-shaped, Deferred-shaped, timer-shaped, Coroutine-shaped, allocator,
      ready-inbox, and safe-wrapper actor in an equivalent fixture; verify semantic facts, MIR,
      engine outcomes, and intrinsic inventories remain equivalent apart from source identities,
      reusing the landed local-shared normalization approach rather than adding a disconnected
      spelling audit.
- [x] 3.4 Inventory semantic, HIR, MIR, evaluation, native, and Wasm branches for source-name checks;
      verify only sealed Intrinsic identities grant execution/wake privilege.
- [x] 3.5 Write a checked-in findings report mapping Initial ownership, task-specific push readiness,
      and recoverable package admission to pressure evidence; record the rejected eager-start/owner-
      sweep/fatal-package model and the deferred concurrency, Coroutine, implicit-root, and parallel-
      memory boundaries without adding a compatibility path or canonical API.
- [x] 3.6 Update language and standard-library documentation to distinguish hidden representation,
      visible relinquishment, nested transfer, external parking, Initial activation, package failure,
      fatal later growth, and whole-package retention behind a forgotten cancelled Wake; verify
      pressure actors are not presented as canonical APIs.

## 4. Boundary and Verification Gates

- [x] 4.1 Add the unowned park-capable complete-entry boundary fixture and verify its stable diagnostic
      code/span is distinct from service requirements and no SLP-0003 implicit owner is synthesized.
- [x] 4.2 Extend the existing local-shared pressure test harness with companion connected
      Execution/Wake programs and add target-specific cases to the designated native differential
      corpus; keep one realized Analysis snapshot per source and verify values,
      activation/readiness order, cleanup/release order, diagnostics, and deterministic artifacts at
      the cheapest required tiers.
- [x] 4.3 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and
      `pnpm release:candidate`; record every exact result and identify pre-existing failures before
      implementation handoff.
