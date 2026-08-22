## 1. Canonical Shared Actor

- [ ] 1.1 Add canonical ordinary Silk source for `Shared<T>` with exactly one private opaque-core field;
      verify it derives affine cleanup and local affinity without a source Drop hook or compiler special case.
- [ ] 1.2 Implement `Shared.make` by requesting `sharedLayout<T>`, allocating through the selected
      `Allocator`, and invoking the unsafe initializer only after success; verify success transfers
      `T` and exhaustion cleans `T` once with only the declared failure and requirement channels.
- [ ] 1.3 Implement allocation-free `Shared.clone` over `sharedClone`; verify the source contract has
      no Effect, failure, or service row and preserves the fatal overflow behavior.

## 2. Safe Access Wrappers

- [ ] 2.1 Implement `Shared.withMut` over `sharedWithMut` with ordinary-source fatal conflict policy;
      verify sequential mutation works and all nested conflicting access traps before user access.
- [ ] 2.2 Implement `Shared.with` by narrowing the exclusive callback borrow to `&T`; verify inspection
      cannot mutate, escape its borrow, or gain a separate reader state.
- [ ] 2.3 Document the four-way reentrancy matrix, allocation boundary, cycle leak, and local affinity;
      verify public documentation does not promise Weak, identity, thread transfer, or post-trap cleanup.

## 3. Shipped Source and Privilege Gates

- [ ] 3.1 Register the module in the standard-library manifest and regenerate the shipped source table;
      verify public source resolution imports and type-checks `Shared` without vendoring.
- [ ] 3.2 Add an equivalent renamed ordinary-source wrapper fixture and verify it receives the same
      semantic, ownership, and access behavior without a compiler change.
- [ ] 3.3 Audit intrinsic registries and semantic, HIR, MIR, evaluation, and backend branches; verify no
      code recognizes `Shared`, Deferred, Scheduler, ready inbox, or conflict policy by spelling.

## 4. Verification

- [ ] 4.1 Run focused standard-library source, construction-failure, access, diagnostics, and generated
      source-table tests; verify all expected codes, spans, ownership transitions, and exports.
- [ ] 4.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`; verify every command passes
      and run `pnpm release:candidate` because shipped package contents change.
