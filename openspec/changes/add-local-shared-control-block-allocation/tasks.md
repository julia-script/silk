## 1. Construction Intrinsic Contracts

- [ ] 1.1 Add `sharedLayout<T>` and unsafe `sharedFromAllocation<T>` to the sealed intrinsic catalog
      with exact generic, safety, access, ownership, failure, and requirement metadata; verify catalog
      and target-availability tests encode only those two construction operations.
- [ ] 1.2 Publish semantic call facts that connect the concrete `T` and target layout provenance across
      both operations; verify mismatched types, targets, sizes, and alignments are rejected with
      stable diagnostic codes and spans.

## 2. Target Layout and Initialization

- [ ] 2.1 Plan one opaque control-block layout covering strong state, access state, private reclaim
      state, padding, and `T`; verify target-layout tests cover alignment, overflow, and a nonzero
      block for zero-sized `T` without exposing private field offsets to source.
- [ ] 2.2 Add one consuming MIR initialization transition that accepts only the matching active
      allocation and one `T`, initializes count one and available access, and publishes one core;
      verify MIR validation rejects reused, unavailable, or mismatched inputs.
- [ ] 2.3 Transfer self-contained reclaim authority into the control block without retaining allocator
      access; verify ownership facts end the allocation and value bindings and retain no provider loan.

## 3. Construction Failure and Boundary Audit

- [ ] 3.1 Add an ordinary-source construction fixture whose allocator rejects the exact layout and
      verify no initializer, core, or release obligation is created and `T` is cleaned exactly once
      by typed-failure cleanup.
- [ ] 3.2 Audit semantic analysis, HIR, MIR, evaluation, and backend dispatch for actor-name checks;
      verify no branch grants privilege to `Shared`, `Allocator`, `OutOfMemoryError`, Deferred,
      Scheduler, or a ready inbox.
- [ ] 3.3 Run `pnpm typecheck`, `pnpm exec biome check .`, and focused layout/allocation tests; verify
      every command passes before lifecycle operations consume the initialized core.
