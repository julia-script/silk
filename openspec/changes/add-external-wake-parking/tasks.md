## 1. Wake and Park Contracts

- [ ] 1.1 Add opaque affine local `Intrinsic.Wake`, consuming synchronous `wake`, and effectful unit-
      returning `park` with a take-once NonParking registration callback to the sealed catalog;
      spell registration as `once fn`, accept its affine captures, assign Wake the canonical
      `ExecutionAffinity.localExecution` seed, propagate it through aggregate/source environments
      and suspended frames, and verify safety, ownership, reachability, affinity, and target metadata
      agree across every phase.
- [ ] 1.2 Extend external-park package plans with one stable wake-control cell and fixed endpoint only
      when statically reachable; verify park, wake, notification, and generation reuse contain no
      allocator access or failure edge.
- [ ] 1.3 Add verified MIR state/authority transitions for Registering, Latched, Dormant, Notifying,
      Eligible, Cancelled, DestroyPending, and generation reuse by extending canonical
      `SuspensionOwnership` liveness, access, affinity, cleanup, restoration, and deterministic
      encoding; verify malformed predecessors, duplicate Wake authority, and premature reuse are
      rejected without a parallel frame-ownership model.

## 2. Registration and Ordering

- [ ] 2.1 Implement park registration with the generation's sole Wake and retain returned `G` plus
      every live frame value before suspension ownership transfer; verify stored-wake dormancy and
      `G` cleanup immediately before resumed source, one invocation of an affine-capturing `once fn`,
      and rejection of any second invocation.
- [ ] 2.2 Implement wake-during-registration latching and gate notification on complete `onSuspend`
      return; verify the Execution relinquishes once and notification cannot observe Running state.
- [ ] 2.3 Cover `onSuspend` destruction after a latched Wake; verify cancellation suppresses endpoint
      invocation and cleans continuation, endpoint, and guard values exactly once.
- [ ] 2.4 Implement Wake-after-dormant one-shot notification; verify the endpoint runs at most once,
      wake never drives inline, and safe source cannot signal the generation twice.

## 3. Notification, Cancellation, and Reclamation

- [ ] 3.1 End cell mutation, enter Notifying, and take an invocation retain before calling `R(&O)`;
      verify the Execution becomes Eligible only after ordinary callback return.
- [ ] 3.2 Implement the defined fatal trap for indirect drive while Notifying; verify no endpoint
      replacement, continuation progress, or drive callback occurs.
- [ ] 3.3 Implement reentrant destruction as DestroyPending and defer endpoint cleanup/package release
      until callback return; verify borrowed `O` and `R` remain live and the Execution never becomes
      Eligible afterward.
- [ ] 3.4 Cancel before dormant cleanup and retain the indivisible Allocation behind every external
      Wake/transient authority; verify late Wake is a consuming no-op, forgotten Wake retains only
      inert bytes, and final authority releases storage once.
- [ ] 3.5 Reinitialize the stable cell only after the prior generation Wake and transients end; verify
      repeated park/wake/resume cycles reuse storage without aliasing generations.
- [ ] 3.6 Preserve local affinity for Execution and Wake and add same-thread source extraction tests;
      directly verify Shared-held Wake and parked Execution satisfy the canonical post-SLP-0002
      scenarios and no transfer syntax, diagnostic, or mandatory atomic fact is invented before a
      future transfer consumer exists.

## 4. Source Boundary and Verification

- [ ] 4.1 Add ordinary Deferred-shaped and timer-shaped registration fixtures that extract Wake under
      short Shared access and signal only afterward; verify no unknown callback runs while source
      access is active, direct and transitive park with an active `Shared.with`/`withMut` borrow is
      rejected before suspension, an owned Shared handle survives park/resume unchanged, and
      payloads remain outside Wake.
- [ ] 4.2 Audit intrinsic and phase inventories for explicit cancel/destroy, Scheduler tokens, payload
      transport, actor-name privilege, hidden allocation, and root policy; verify none is introduced.
- [ ] 4.3 Run focused wake-order, ownership, cleanup, reentrancy, generation, local-affinity, and
      intrinsic-boundary tests, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`,
      `pnpm check`, and `pnpm release:candidate`; record exact results before engine parity work
      begins.
