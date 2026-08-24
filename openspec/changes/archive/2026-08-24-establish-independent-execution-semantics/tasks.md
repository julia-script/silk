## 1. Static Suspension and Property Facts

- [x] 1.1 Replace the monolithic suspendability outcome with deterministic direct/nested/external-
      park summaries and verify complete, open-generic, selected-provider, and unavailable call
      graphs preserve the exact permitted modes in semantic inspection.
- [x] 1.2 Add the explicit Execution propagation delimiter to reachability and verify a park-capable
      body selects external mode while the ordinary owner-side `drive` caller does not become
      park-capable.
- [x] 1.3 Derive `Intrinsic.Detached` from complete executable/value environment dependencies and
      the canonical environment-component and borrow-root provenance already used by
      `ExecutionAffinity`; verify owned unrestricted and local-Shared captures plus an opaque
      producer result pass independently of payload spelling, local captures remain
      `LocalExecution`, and lexical loans, borrowed providers, nested nominal loans, and empty-row
      borrowed bindings fail with stable codes, spans, and causal paths.
- [x] 1.4 Derive `Intrinsic.NonParking` from specialized transitive external-park reachability and
      verify direct and nested-only callbacks pass while ordinary-helper and selected-provider paths
      to park fail with stable codes, primary obligation spans, and deterministic causal paths.

## 2. Exact-Bound and Lifecycle Semantics

- [x] 2.1 Admit one exact Effect or callable representation plus sealed static-property conjuncts and
      verify parsing/semantic facts preserve exact identity rather than creating interface,
      service, runtime-witness, or general-intersection behavior.
- [x] 2.2 Preserve exact executable identity and ordered property obligations through forwarding,
      substitution, specialization, caching, and serialization; verify repeated analysis produces
      byte-identical facts for success and failure cases.
- [x] 2.3 Add opaque affine local `Intrinsic.Execution<A>` semantic and ownership identity with
      Initial, Running, Dormant, Notifying, Eligible, Completed, and Destroyed logical states; verify
      every available instance composes through the existing `ExecutionAffinity` lattice as
      `LocalExecution` independent of available `A`, malformed or unavailable `A` stays unavailable,
      no execution-instance identity is published, and actor-name lookalikes receive no privilege.
- [x] 2.4 Track one non-Copy Execution obligation through move, drive ownership, completion, and drop;
      verify duplicate use is rejected and no thread-transfer permission is published.
- [x] 2.5 Preserve execution-internal stable loans across parking and cleanup order while rejecting
      external construction loans and completion results borrowing package/body/frame/endpoint
      storage; verify an owned Shared handle crosses park/resume with one unchanged strong obligation
      while direct and transitive park under active `Shared.with`/`withMut` access receives the
      canonical diagnostic and creates no suspended frame.

## 3. Boundary Diagnostics and Gates

- [x] 3.1 Diagnose a park-capable complete entry with no explicit Execution delimiter independently
      from unsatisfied requirement rows; verify the code and span are stable and no implicit owner or
      final-outcome policy is synthesized.
- [x] 3.2 Add ordinary source declarations named Execution, Wake, Detached, NonParking, Scheduler,
      Fiber, Deferred, Timer, and Coroutine; verify semantic, ownership, and reachability facts grant
      no sealed identity or mode from spelling.
- [x] 3.3 Update canonical inspection/golden encodings for suspension modes, properties, exact bounds,
      lifecycle, affinity, and causes; verify repeated in-process analysis is byte-identical without
      adding a redundant fresh-process determinism test.
- [x] 3.4 Run focused semantic, generic, ownership, diagnostic, and inspection tests, then
      `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and
      `pnpm release:candidate`; record every exact result before handing the semantic prerequisite
      to the packaging slice.
