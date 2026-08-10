## 1. Add contract-row generics

- [x] 1.1 Add red parser, formatter, and syntax-tree cases for channel-kinded generic binders such as `<A, !E, ?R>` and row expressions in Effect contracts.
- [x] 1.2 Index canonical failure-row and requirement-row parameters with kind-correct duplicate, unbound, and misuse diagnostics.
- [x] 1.3 Implement normalized row union and selected-entry-plus-remainder unification for failures and capability-role-access requirements.
- [x] 1.4 Add inference diagnostics for absent members, ambiguous remainder constraints, incompatible requirement access or role, and non-finite specialization.
- [x] 1.5 Carry concrete normalized row arguments through semantic facts, presentation, documentation, deterministic instance keys, and fresh-process encodings without runtime descriptors.

## 2. Make Effect values ordinary higher-order values

- [x] 2.1 Add red public-source cases that pass, return, store, capture, and generically specialize closed Effects while retaining distinct hidden construction identities.
- [x] 2.2 Extend ownership and target layout for Effect parameters and captures across shared, exclusive, and consuming run access.
- [x] 2.3 Lower and execute ordinary calls whose parameters or results are hidden-identity Effect values across evaluator, native LLVM, and direct Wasm.
- [x] 2.4 Prove repeated, exclusive, take-once, and dropped-unrun Effect values preserve every captured owner and loan exactly once across higher-order boundaries.

## 3. Establish the minimal compiler Effect core

- [x] 3.1 Add canonical ordinary Silk `Result<A, E>` data and exhaustive success/failure matching with Copy and affine payload coverage.
- [x] 3.2 Add an effectful core outcome-reification operation that executes one Effect, preserves requirements and run access, returns `Result<A, E>`, and excludes traps from `E`.
- [x] 3.3 Add one generic typed requirement-binding core operation that satisfies a selected capability-role entry, preserves an unknown remainder row, and exposes no runtime requirement record.
- [x] 3.4 Lower outcome reification and requirement binding through backend-neutral MIR with evaluator, LLVM, direct-Wasm, ownership, cleanup, and deterministic verification parity.
- [x] 3.5 Add architecture assertions that closed non-suspending programs link no scheduler or fiber runtime and that no public semantic artifact exposes a runner callback, pending step, or requirement container ABI.

## 4. Move Effect combinators into Silk

- [x] 4.1 Add the canonical visible Effect standard-library module and implement success mapping, failure mapping, `mapBoth`, `map`, and `mapError` through ordinary Result matching.
- [x] 4.2 Implement `flatMap`, `tap`, and `catch` as ordinary effectful channel transformations with generic row and callable contracts.
- [x] 4.3 Implement repeatability-checked `retry` in Silk while preserving reconstruction of execution locals and capture-derived run access.
- [x] 4.4 Implement `provide` over generic requirement binding and `provideWith` over ordinary effectful acquisition, lexical ownership, and cleanup on success or typed failure.
- [x] 4.5 Publish standard-library source locations through hover, documentation, and go-to-definition and verify copied user-defined equivalents compile without semantic privilege.

## 5. Remove the privileged combinator implementation

- [x] 5.1 Differentially compare the source library with existing intrinsics across direct, piped, grouped, stored, Copy, affine, failure, provision, evaluator, native, and direct-Wasm programs.
- [x] 5.2 Switch resolution to canonical Silk declarations and remove Effect-combinator intrinsic rules, dedicated HIR recipes, stored-recipe bookkeeping, specialized MIR operations, and backend branches.
- [x] 5.3 Re-run lexer, stack VM, algorithm, allocation, failure-cleanup, and entrypoint pressure programs with the source-defined library and retain fresh-process determinism.
- [x] 5.4 Update the concurrency direction, Effect pattern corpus, project roadmap, reference documentation, and the dependent synchronous-cost spike with the final compiler-core boundary.
- [x] 5.5 Run focused tests throughout, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, `pnpm release:candidate`, and strict OpenSpec validation.
