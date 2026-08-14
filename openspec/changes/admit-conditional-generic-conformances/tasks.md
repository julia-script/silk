## 1. Syntax and Indexing

- [ ] 1.1 Add bounded `impl<...>` syntax with explicit provider applications and no `where` or implicit `Self`.
- [ ] 1.2 Index complete kinded binders, requirements, provider/interface heads, mapped operations, and unavailable states.
- [ ] 1.3 Add formatter, syntax-correspondence, duplicate, damaged, and wrong-provider fixtures.

## 2. Coherence and Termination

- [ ] 2.1 Implement alpha-normalized conservative may-overlap for ordinary types, normalized rows, and representation bounds.
- [ ] 2.2 Reject overlapping heads at declaration time without proving or comparing their bounds.
- [ ] 2.3 Implement strict provider-subterm, non-increasing-variable, and unchanged-ground-argument termination checks.
- [ ] 2.4 Add accepted `MappedSchema`/`OptionalSchema` and rejected equal/growing provider size fixtures.

## 3. Proof Search and Diagnostics

- [ ] 3.1 Add canonical concrete conformance goals, completed-proof memoization, and finite strict-subterm traversal.
- [ ] 3.2 Preserve active-goal cycle detection as defensive recovery without admitting coinductive proof.
- [ ] 3.3 Emit deterministic missing-base, overlap, termination, and cycle requirement traces.

## 4. HIR and Instance Discovery

- [ ] 4.1 Retain unresolved conditional witness questions in generic HIR.
- [ ] 4.2 Substitute concrete kinded arguments and discover every transitive base and wrapper witness.
- [ ] 4.3 Key one deterministic witness per concrete provider/interface pair and lower only direct targets.
- [ ] 4.4 Assert that no runtime witness dictionary, interface tag, or standard-library actor lookup is emitted.

## 5. Verification

- [ ] 5.1 Run `pnpm typecheck` and `pnpm exec biome check .`.
- [ ] 5.2 Run `pnpm test`, `pnpm check`, and `pnpm release:candidate`; report exact failures.
- [ ] 5.3 Repeat conformance proof and instance artifacts in fresh processes to verify determinism.
