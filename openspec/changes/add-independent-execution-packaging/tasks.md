## 1. Intrinsic and Layout Contracts

- [ ] 1.1 Add `executionLayout`, unsafe `executionFromAllocation`, and unit-returning `drive` to the
      sealed catalog with exact generic, Detached, NonParking, safety, ownership, effect, failure,
      requirement, and target-availability metadata; verify every phase exposes the same inventory.
- [ ] 1.2 Plan one exact target package keyed by `A`, `F`, `O`, `R`, target, and suspension summary;
      verify direct, nested-only, external-park, zero-sized endpoint, alignment, and overflow cases
      without publishing backend field offsets.
- [ ] 1.3 Carry one canonical package-plan provenance from Layout through semantic facts and MIR;
      verify mismatched type, target, size, alignment, endpoint, and suspension-plan inputs are
      rejected before initialization.

## 2. Construction and Erased Ownership

- [ ] 2.1 Implement one consuming MIR initializer that transfers Allocation, body, endpoint state,
      and endpoint callback into one Initial Execution without running source; verify no partial
      package or second allocation can become visible.
- [ ] 2.2 Retain exact hidden invoke/drop metadata for `F`, `O`, and `R` at the purpose-bound erasure
      seam; verify never-driven drop cleans every affine capture and endpoint value exactly once.
- [ ] 2.3 Add an ordinary-source safe wrapper that queries Layout and procures Allocation through the
      selected Allocator; verify allocation refusal preserves and cleans all inputs, exposes only the
      wrapper's declared failure/requirement rows, and publishes no Execution.
- [ ] 2.4 Verify the statically non-parking zero-sized endpoint specialization owns the erased body
      package while omitting wake-control and readiness storage in target layout and MIR artifacts.

## 3. Drive, Logical Roots, and Cleanup

- [ ] 3.1 Implement verified drive entry for Initial and Eligible states and the fatal pre-callback
      trap for Dormant and Notifying; verify illegal paths make no progress and invoke no outcome
      callback.
- [ ] 3.2 Lower completion and external-suspension branches so exactly one take-once callback receives
      the sole affine branch state; verify the unused callback is cleaned once and completion returns
      no Execution.
- [ ] 3.3 Keep nested `Effect.suspend` transfers inside one drive activation; verify direct-child
      completion resumes its parent without invoking the owner suspension callback.
- [ ] 3.4 Root one evaluator logical-stack context per first drive and persist it across later drives;
      verify alternating two execution owners preserves independent CallDepth and trace ancestry.
- [ ] 3.5 Implement completion, never-driven drop, and dormant/eligible cleanup plans with loans ending
      before referents; verify structured success and reified typed-failure cases clean and release
      the combined Allocation once.
- [ ] 3.6 Preserve fatal post-construction continuation growth and the no-unwind contract; verify
      exhaustion adds no Effect failure member and no source Allocator access.

## 4. Privilege and Verification Gates

- [ ] 4.1 Audit semantic, HIR, MIR, evaluation, and backend code for names of Allocator,
      OutOfMemoryError, safe Execution wrappers, Scheduler, Fiber, Deferred, Timer, ready queues, and
      Coroutine; verify only sealed intrinsic identities select package or drive behavior.
- [ ] 4.2 Run focused intrinsic, layout, allocation-provenance, ownership, cleanup, and logical-root
      tests, then `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`; record exact results
      before the wake/parking slice begins.
