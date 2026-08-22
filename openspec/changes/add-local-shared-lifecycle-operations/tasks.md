## 1. Closed Lifecycle Operations

- [ ] 1.1 Add `sharedClone` and callback-shaped `sharedWithMut` to the sealed catalog with the exact
      ordinary take-once callback contracts; verify intrinsic inventory tests find no separate reader,
      weak, atomic, lock, or actor-specific operation.
- [ ] 1.2 Implement a target-bounded strong transition that compares before incrementing and returns
      one new affine handle only on success; verify a reduced private maximum traps without a store or
      partial handle and backend-independent transition tests cover the boundary.
- [ ] 1.3 Implement the `Available | Active` access transition so exactly one callback runs and conflict
      observes without mutation; verify transition tests cover success, conflict, normal restoration,
      and outer access remaining active after a nested conflict.

## 2. Borrow and Escape Checking

- [ ] 2.1 Create one position-restricted exclusive loan for the successful callback and end it before
      restoration and return; verify sequential access succeeds after the first callback completes.
- [ ] 2.2 Reject direct and narrowed returned borrows with a diagnostic that relates the escape to the
      access boundary; verify diagnostic codes and both spans rather than message text.
- [ ] 2.3 Recursively reject the borrow inside generic results, aggregates, failure values, Effects,
      and stored callables; verify one focused case for each container reaches the same ownership rule.
- [ ] 2.4 Reject suspension while the access loan is live and verify no coroutine frame receives that
      loan or an independently owned reference to `T`.

## 3. Dynamic Cleanup Authority

- [ ] 3.1 Lower explicit and structured core drop to one opaque cleanup action: non-last decrement or
      last `T` cleanup followed by reclaim; verify ownership plans never recursively schedule `T` per handle.
- [ ] 3.2 Verify clone and non-last drop during active access change only the strong count and cannot
      make the borrowed receiver's handle become the last live obligation.
- [ ] 3.3 Add ownership fixtures for two-handle drop, two-frame typed-failure cleanup, acyclic nested
      cores, and a strong cycle; verify exact obligations and the specified cycle leak before engine work.

## 4. Verification

- [ ] 4.1 Cover all four public shared/exclusive nested-access shapes at semantic and transition tiers;
      verify every nested operation selects conflict before forming another reference.
- [ ] 4.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and focused ownership/intrinsic tests; verify
      every command passes before the standard-library wrapper is added.
