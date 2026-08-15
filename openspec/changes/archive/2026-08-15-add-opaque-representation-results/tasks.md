## 1. Prerequisite and Syntax

- [x] 1.1 Confirm `introduce-representation-parameters` is complete and its runtime storage fences remain active.
- [x] 1.2 Add fully specialized `typeof(item)` parsing, formatting, syntax correspondence, and damaged recovery.
- [x] 1.3 Add contextual scoped `some<F: Contract> Result` parsing, formatting, and binder resolution.

## 2. Exact Identity Contracts

- [x] 2.1 Resolve exact named callable identities only after overload and generic specialization are complete.
- [x] 2.2 Enforce public-contract visibility and reject locals, sections, Effects, private leaks, and partial generic items.
- [x] 2.3 Add navigation and diagnostics for valid and invalid exact identity contracts.

## 3. Opaque Families and Realizations

- [x] 3.1 Add stable family keys, normalized public signatures, and family instances over enclosing kinded arguments.
- [x] 3.2 Unify every reachable producer return to one realization per opaque binder and specialization.
- [x] 3.3 Reject divergent returns, realization-only recursion, and inline opaque layout cycles.
- [x] 3.4 Publish privacy-preserving compiler-internal realization definitions for cross-module specialization.

## 4. Invalidation

- [x] 4.1 Add separate target/body and layout/access/cleanup/suspendability fingerprints as incremental dependencies.
- [x] 4.2 Implement the bounded opaque invalidation slice for value-only, target-only, capture-shape, suspendability, bound, and binder-order edits.
- [x] 4.3 Add fresh-process equality, privacy, and invalidation assertions for generic opaque producers.

## 5. Verification

- [x] 5.1 Run `pnpm typecheck` and `pnpm exec biome check .`.
- [x] 5.2 Run `pnpm test`, `pnpm check`, and `pnpm release:candidate`; report exact failures.
- [x] 5.3 Verify opaque results introduce no runtime descriptor, allocation, indirect call, or existential join.
