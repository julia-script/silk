## 1. Access Contracts and Prerequisites

- [x] 1.1 Confirm representation parameters and callable field representation infrastructure are complete.
- [x] 1.2 Add shared `Effect`, exclusive `mut Effect`, and consuming `once Effect` bound syntax, formatting, and kind checking.
- [x] 1.3 Add admissibility and negative aggregate-access fixtures for every run mode.

## 2. Stored-Effect Vertical Slice

- [x] 2.1 Extend resolved field representations with runner, concrete arguments, rows, access, environment, cleanup, and suspendability.
- [x] 2.2 Implement one unrun cleanup case and one suspending run case without a standalone structural Effect ABI.
- [x] 2.3 Stop and revise the design if any backend reconstructs runner semantics or requires a parallel field model.

## 3. Ownership, Layout, and MIR

- [x] 3.1 Preserve Effect loans, exact rows, whole-value moves, nesting, and direct-field-extraction rejection.
- [x] 3.2 Plan inline concrete environments and suspendability dependencies in the enclosing build-internal nominal ABI.
- [x] 3.3 Carry lazy construction, run, suspension/resume, typed failure, and cleanup through HIR and MIR.

## 4. Engine Parity and Invalidation

- [ ] 4.1 Execute shared, exclusive, consuming, unrun, failing, and suspending stored Effects in the evaluator.
- [ ] 4.2 Add native LLVM and direct-Wasm parity for results, failures, runner identity, and cleanup traces.
- [ ] 4.3 Add capture-shape, target, access, cleanup, and suspendability invalidation fixtures.
- [ ] 4.4 Narrow the unavailable-Effect-layout fence only for shapes proven by all engines.

## 5. Verification

- [ ] 5.1 Run `pnpm typecheck` and `pnpm exec biome check .`.
- [ ] 5.2 Run `pnpm test`, `pnpm check`, and `pnpm release:candidate`; report exact failures.
- [ ] 5.3 Verify rows create no runtime lanes and direct Wasm introduces no indirect dispatch.
