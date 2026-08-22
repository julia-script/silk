## 1. Closed Lifecycle Operations

- [ ] 1.1 Add `sharedClone` and callback-shaped `sharedWithMut` to the sealed catalog with the exact
      ordinary take-once callback contracts; verify intrinsic inventory tests find no separate reader,
      weak, atomic, lock, count, address, access-state, cleanup-authority, conflict-value, extra
      lifecycle, or actor-specific operation, and prove same-spelled ordinary declarations receive
      no intrinsic identity or behavior.
- [ ] 1.2 Implement a target-bounded strong transition that compares before incrementing and returns
      one new affine handle only on success; verify a reduced private maximum traps without a store or
      partial handle, backend-independent transition tests cover the boundary, and allocation/payload
      probes observe no allocation and no read, copy, move, cleanup, or user operation on `T`.
- [ ] 1.3 Implement the `Available | Active` access transition so exactly one callback runs and conflict
      observes without mutation; verify transition tests cover success, conflict, normal restoration,
      outer access remaining active after a nested conflict, zero allocation, and exactly-once cleanup
      of affine captures owned by the unselected callback after the selected callback returns normally.

## 2. Borrow and Escape Checking

- [ ] 2.1 Create one position-restricted exclusive loan for the successful callback and end it before
      restoration and return; verify sequential access succeeds after the first callback completes.
- [ ] 2.2 Reject direct and narrowed returned borrows with a diagnostic that relates the escape to the
      access boundary; verify diagnostic codes and both spans rather than message text.
- [ ] 2.3 Recursively reject the borrow inside generic results, aggregates, failure values, Effects,
      and stored callables; verify one focused case for each container reaches the same stable
      diagnostic identity and retains both its escape span and the access-boundary span.
- [ ] 2.4 Reject suspension while the access loan is live and verify no coroutine frame receives that
      loan or an independently owned reference to `T`; assert the same stable diagnostic identity,
      suspension span, and access-boundary span.

## 3. Dynamic Cleanup Authority

- [ ] 3.1 Lower explicit and structured core drop to one opaque cleanup action: non-last decrement or
      last `T` cleanup followed by reclaim; verify ownership plans never recursively schedule `T` per handle.
- [ ] 3.2 Verify clone and non-last drop during active access change only the strong count and cannot
      consume the borrowed receiver or trigger last-handle cleanup while its access is active; verify
      dropping the other handle from a two-handle state legally changes `2 -> 1` and leaves the
      borrowed receiver as the sole live obligation until the callback returns.
- [ ] 3.3 Add ownership fixtures for two-handle drop, two-frame typed-failure cleanup, acyclic nested
      cores, and a strong cycle; verify exact obligations and the specified cycle leak before engine work.

## 4. Verification

- [ ] 4.1 Cover all four public shared/exclusive nested-access shapes at semantic and transition tiers;
      verify every nested operation selects conflict before forming another reference.
- [ ] 4.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and focused ownership/intrinsic tests; verify
      every command passes before the standard-library wrapper is added.
