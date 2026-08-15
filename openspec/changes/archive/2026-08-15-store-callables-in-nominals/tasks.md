## 1. Vertical Slice

- [x] 1.1 Confirm `introduce-representation-parameters` is complete and identify the existing callable storage fences.
- [x] 1.2 Add one `CallableFieldRealization` actor that consumes `RepresentationField` resolutions and enriches them with the static target, concrete target arguments, ordered captures, invocation access, loan dependencies, liveness, and cleanup, with deterministic lookup by complete instance and field identity.
- [x] 1.3 Build one named-callable and one affine-capturing nominal field slice through symbolic plans, resolved field representations, and realizations.
- [x] 1.4 Stop and revise the design if any phase rediscovers callable construction from syntax instead of consuming the shared resolved fact.

## 2. Ownership and Cleanup

- [x] 2.1 Derive shared, exclusive, and consuming invocation access from aggregate receivers.
- [x] 2.2 Carry capture loans, liveness, whole-value moves, and cleanup through nested representation-bearing nominals.
- [x] 2.3 Keep representation-bearing nominals move-only and reject direct owned field extraction with a dedicated diagnostic.
- [x] 2.4 Add uncalled, called, consuming, moved, typed-failure, and scoped-borrow cleanup traces. Plan-level proofs exist; the executed traces need the engines from section 4.

## 3. Layout and MIR

- [x] 3.1 Plan concrete callable capture fields inline from the resolved representation while keeping structural callable contracts unlayoutable.
- [x] 3.2 Extend HIR/MIR aggregate construction, borrow, projection, invocation, movement, and cleanup with static callable facts.
- [x] 3.3 Add deterministic layouts, instance keys, symbols, and MIR text for nested callable storage.

## 4. Engine Parity and Fences

- [x] 4.1 Execute the callable storage matrix in the evaluator.
- [x] 4.2 Lower the same matrix through native LLVM with equal target and cleanup behavior.
- [x] 4.3 Lower the same matrix through direct Wasm and assert no table or `call_indirect`.
- [x] 4.4 Narrow `SEM0103` only for all-engine-proven construction paths and retain it everywhere else.

## 5. Verification

- [x] 5.1 Run `pnpm typecheck` and `pnpm exec biome check .`.
- [x] 5.2 Run `pnpm test`, `pnpm check`, and `pnpm release:candidate`; report exact failures.
- [x] 5.3 Repeat cross-engine callable acceptance in fresh processes and compare deterministic facts and artifacts.
