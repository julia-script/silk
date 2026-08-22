## 1. Ordinary-Silk Shared-State Witness

- [ ] 1.1 Implement a readable fixed-capacity ready inbox and Deferred-style value/waiter state in
      ordinary pressure-program source; verify neither actor requires a compiler-known nominal,
      intrinsic operation, hidden allocation, or escaping exclusive borrow.
- [ ] 1.2 Retain cloned inbox and state handles in multiple dormant ordinary callables or Effects;
      verify two callbacks enqueue in execution order and dropping one dormant callback decrements
      without releasing state still held elsewhere.
- [ ] 1.3 Implement registration and one-time publication by moving callbacks and any affine payload
      out under short `Shared.withMut` calls and invoking callbacks only afterward; verify no unknown
      executable runs while access is active.

## 2. Ownership and Failure Evidence

- [ ] 2.1 Cover publication to several waiters and verify exactly one publication, deterministic
      callback order, and no reentrant conflict under the extract-then-invoke pattern.
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
      remaining execution-transfer, parking, and wake-order work; verify it cites each acceptance case
      and does not promote the witness actors to canonical APIs.

## 4. Verification

- [ ] 4.1 Run focused pressure-program and differential tests, then `pnpm typecheck`,
      `pnpm exec biome check .`, `pnpm test`, and `pnpm check`; verify every command passes and report
      exact failures before declaring the sufficiency evidence complete.
