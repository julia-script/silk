## 1. Syntax and Declaration Facts

- [ ] 1.1 Add callable- and Effect-bounded representation parameter syntax with damaged recovery and formatter correspondence.
- [ ] 1.2 Extend declaration indexing to record ordered generic parameter kinds and representation bounds.
- [ ] 1.3 Add positive, duplicate, unbound, and wrong-kind `RepresentationSyntax` fixtures.

## 2. Canonical Representation Algebra

- [ ] 2.1 Introduce canonical representation parameters, exact arguments, open references, required bounds, and admissibility proofs.
- [ ] 2.2 Generalize nominal applications to ordered kinded generic arguments while preserving ordinary type substitutions.
- [ ] 2.3 Implement equality, ordering, hashing, deterministic encoding, and unavailable recovery for every argument kind.
- [ ] 2.4 Add same-identity/different-bound and repeated-`F` unification/mismatch tests.

## 3. Inference and Propagation

- [ ] 3.1 Infer representation arguments from nominal field initializers and diagnose the first conflicting occurrence.
- [ ] 3.2 Propagate representation arguments through nested nominals, parameters, results, borrows, non-owning projection, and joins.
- [ ] 3.3 Preserve open and concrete representation arguments in generic HIR and instance keys.
- [ ] 3.4 Reject every unresolved representation before layout and MIR while retaining existing runtime storage fences.

## 4. Diagnostics and Tooling

- [ ] 4.1 Implement first-divergent-representation join diagnostics with deterministic origin ordering and consumption guidance.
- [ ] 4.2 Expose complete representation-dependent nominal types and navigation in analysis inspectors and tooling facts.
- [ ] 4.3 Add fresh-process determinism fixtures for semantic facts, HIR, instance keys, presentation, and diagnostics.

## 5. Verification

- [ ] 5.1 Run `pnpm typecheck` and repair every type error caused by the generic-argument migration.
- [ ] 5.2 Run `pnpm exec biome check .` and repair formatting or lint failures.
- [ ] 5.3 Run `pnpm test`, `pnpm check`, and `pnpm release:candidate`; record any pre-existing failure separately.
- [ ] 5.4 Validate that `SEM0103` and Effect-layout fences still reject all runtime nominal storage paths.
