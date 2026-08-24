## 1. Verified MIR and Inspection

- [x] 1.1 Complete target-neutral MIR operations/edges for initialization, drive, nested transfer,
      park, registration, suspension ownership, wake, notification, eligibility, resume, completion,
      cancellation, DestroyPending, and release; verify every legal state path validates.
- [x] 1.2 Add negative MIR tests for invalid predecessor states, duplicate authorities, mismatched
      package provenance, callback violations, escaping completion loans, premature generation reuse,
      and endpoint cleanup before invocation return; verify each is rejected before lowering.
- [x] 1.3 Encode logical execution identities, roots, generations, transitions, authorities, and cleanup
      edges in deterministic inspection without addresses, offsets, Scheduler identity, or ABI data;
      verify repeated analysis is byte-identical.

## 2. Evaluator Oracle

- [x] 2.1 Model explicit execution records, local stack roots, frame stacks, endpoint generations,
      wake-cell states, and reclaim authorities without JavaScript Promise, call-stack, object-
      identity, GC, or finalizer semantics; verify deterministic transition-unit tests.
- [x] 2.2 Execute one owner-selected activation synchronously until completion or relinquishment and
      model wake as readiness only; verify two executions alternate in non-LIFO order with independent
      CallDepth and trace ancestry.
- [x] 2.3 Emit bounded canonical events for every construction, drive, wake-order, cleanup-matrix,
      cancellation, DestroyPending, and release branch; verify repeated scripted runs produce
      identical results and traces.

## 3. Native and Wasm Realization

- [x] 3.1 Extend native lowering from canonical `SuspensionOwnership` liveness, frame, access-mode,
      affinity, resume-label, outcome-restoration, and cleanup facts under execution-owned roots;
      verify reverse-order resumption and target-private package state agree with evaluation.
- [x] 3.2 Extend direct-Wasm lowering from that same `SuspensionOwnership` authority and
      execution-owned storage; verify reverse-order resumption, cleanup, reclaim, and deterministic
      ownership encoding agree with evaluation and native.
- [x] 3.3 Lower local Wake coordination without mandatory atomics or thread-transfer support and
      implement same-thread reactor delivery; verify native and Wasm artifacts contain no worker,
      work-stealing, or compiler-known timer/scheduler policy.
- [ ] 3.4 Preserve existing nested/LIFO lowering and fatal stack/no-unwind traps; verify nested-only
      corpus behavior is unchanged and illegal drive/exhaustion traps occur before outcome callbacks.
- [ ] 3.5 Repeat native and direct-Wasm emission for equivalent verified plans and verify runtime
      helper selection, resume labels, package-layout references, structural inspection, and final
      artifact bytes are deterministic for each target.

## 4. Differential Acceptance

- [x] 4.1 Add non-LIFO alternating execution programs to the designated native differential corpus;
      verify evaluation, native, and Wasm agree on outcomes, logical depth, readiness order, and
      cleanup without a per-feature native compile test.
- [x] 4.2 Cover wake inside registration, `onSuspend` destroy, wake after dormancy, execution-before-
      wake destroy, eligible drop, reentrant notification destroy, repeated generations, typed
      failure data, and late cancelled Wake; verify evaluation and Wasm at every relevant branch and
      native at target-specific boundary cases.
- [x] 4.3 Cover same-thread timer/reactor delivery and explicit target unavailability; verify no
      fallback blocks, transfers Wake across threads, or adds hidden host scheduling.
- [ ] 4.4 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`; record every
      exact result, then run `pnpm release:candidate`; identify pre-existing failures before the
      separation-evidence slice.
