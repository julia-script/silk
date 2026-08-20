## 1. Owners places and provenance

- [ ] 1.1 Add stable logical owner/place identities for locals, materialized temporaries, and addressable subplaces.
- [ ] 1.2 Carry provenance through projections, borrow locals, calls, returns, HIR, and MIR.
- [ ] 1.3 Admit lexical shared/exclusive borrow bindings while preserving exclusivity, non-escape, and last-use rules.
- [ ] 1.4 Replace named-whole-array and standalone-borrow exceptions and diagnostics.

## 2. Callable sections and lifetimes

- [ ] 2.1 Represent ordered supplied trailing suffixes and remaining leading-parameter contracts for every `0 < K < N`.
- [ ] 2.2 Implement staged application with exact affine move and borrow behavior.
- [ ] 2.3 Generalize evaluator, LLVM, Wasm, storage, formatting, signature help, and inspectors.
- [ ] 2.4 Share last-use loan calculation between Effect runs and callable invocation while retaining escape/storage barriers.

## 3. Verification

- [ ] 3.1 Add temporary-call borrows, local borrow values, projections, valid returned provenance, and escaping-view failures.
- [ ] 3.2 Add binary and multi-parameter sections, staged application, affine arguments, captures, and last-invocation tests.
- [ ] 3.3 Retire `SEM0079`, update canonical specs/docs/diagnostics, and delete unary-only representations.
- [ ] 3.4 Run typecheck, Biome, evaluator/Wasm tests, native corpus for address-root behavior, full tests, and `pnpm check`.
