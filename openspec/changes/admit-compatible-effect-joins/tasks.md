## 1. Semantic joins

- [ ] 1.1 Replace construction-identity rejection with finite success/failure/requirement/access/ownership contract joining.
- [ ] 1.2 Define deterministic incompatibility diagnostics for non-finite or unsafe joins.
- [ ] 1.3 Preserve laziness and exact selected-branch capture semantics in semantic facts.

## 2. Representation and execution

- [ ] 2.1 Add a closed composite Effect representation to HIR and its deterministic encoding.
- [ ] 2.2 Add MIR selection, ownership, cleanup, and verification for only the active alternative.
- [ ] 2.3 Implement evaluator, LLVM, and Wasm realization without heap allocation.
- [ ] 2.4 Delete `SEM0069` construction-identity paths and obsolete fixtures.

## 3. Verification

- [ ] 3.1 Add branch, match, distinct channel, capture access, affine cleanup, and incompatible-join tests.
- [ ] 3.2 Add cross-engine outcomes and deterministic artifact coverage at the cheapest applicable tiers.
- [ ] 3.3 Update canonical specs, diagnostics, docs, and representation inspectors.
- [ ] 3.4 Run typecheck, Biome, evaluator/Wasm tests, native corpus, full tests, and `pnpm check`.
