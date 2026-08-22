## 1. Ordinary-Silk Shared-State Witness

- [ ] 1.1 Implement a readable fixed-capacity ready inbox and Deferred-style value/waiter state in
      ordinary pressure-program source; verify neither actor requires a compiler-known nominal,
      intrinsic operation, hidden allocation, or escaping exclusive borrow.
- [ ] 1.2 Retain cloned inbox and state handles in multiple dormant ordinary callables or Effects;
      require at least one stored callable and one dormant unrun Effect, verify two callbacks enqueue
      in execution order, and verify dropping one dormant Effect decrements its captured handle
      without releasing state still held elsewhere.
- [ ] 1.3 Implement registration and the witness's single publication by moving the offered affine
      payload into shared state, moving only callbacks out under short `Shared.withMut` calls, and
      invoking callbacks only afterward; verify every waiter observes the one stored value by shared
      borrow, no unknown executable runs while access is active, and no repeated-publication policy is
      introduced.

## 2. Ownership and Failure Evidence

- [ ] 2.1 Cover publication to several waiters and verify exactly one publication state transition,
      one retained value owner, one callback invocation per waiter, deterministic callback order,
      and no reentrant conflict under the extract-then-invoke pattern.
- [ ] 2.2 Cover last-drop of unpublished affine state and an unconsumed callback; verify each retained
      owner is cleaned exactly once before one control-block release.
- [ ] 2.3 Sweep construction failure at every exercised allocation ordinal in evaluation and Wasm and
      representative native boundary ordinals; verify no partial actor escapes and subsequent runs
      remain deterministic.

## 3. Cross-Engine and Privilege Gates

- [ ] 3.1 Add the connected witness to the designated differential corpus; verify evaluation, native,
      and Wasm agree on inbox contents, callback order, publication result, count transitions,
      payload cleanup, and release order.
- [ ] 3.2 Rename all witness actors in an equivalent fixture and inspect semantic facts, MIR, and
      intrinsic inventory; verify behavior is unchanged and no phase names Shared, queue, Deferred,
      Scheduler, execution, or callback registry as privileged.
- [ ] 3.3 Write the checked-in findings report separating the removed shared-state wall from SLP-0001's
      remaining execution-transfer, parking, and wake-order work; classify every wall as language,
      standard-library, compiler-defect, tooling/ergonomics, or performance/cost, cite evidence,
      record disposition and smallest follow-up, compare repeated findings with the lexer and stack-VM
      reports, choose next work from evidence, cite each acceptance case, and do not promote witness
      actors to canonical APIs.

## 4. Verification

- [ ] 4.1 Run focused pressure-program and differential tests, then `pnpm typecheck`,
      `pnpm exec biome check .`, `pnpm test`, and `pnpm check`; verify every command passes and report
      exact failures before declaring the sufficiency evidence complete.
