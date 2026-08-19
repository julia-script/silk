## 1. Artifact identities

- [ ] 1.1 Define deterministic identity and digest records for compiler, catalog, source modules, intrinsic inventory, providers, and runtime support.
- [ ] 1.2 Make generation exclude paths, timestamps, and other nondeterministic inputs.
- [ ] 1.3 Publish the matched identity graph in compiler/distribution metadata and inspectors.

## 2. Validation boundaries

- [ ] 2.1 Validate compiler, catalog, source, and intrinsic identities before user source resolution.
- [ ] 2.2 Validate selected provider and runtime coverage after reachable intrinsic planning.
- [ ] 2.3 Model missing source, malformed distribution, mismatch, unsupported target, unresolved entry, and operational failure distinctly.
- [ ] 2.4 Remove fallback search and late backend/runtime reclassification paths.

## 3. Interfaces and verification

- [ ] 3.1 Expose structured integrity outcomes consistently through driver, CLI, embeddings, and tooling.
- [ ] 3.2 Add stale catalog/source, mismatched intrinsic, missing promised runtime, unsupported target, and valid subset tests.
- [ ] 3.3 Add fresh-process deterministic digest coverage in the designated global canary rather than per feature.
- [ ] 3.4 Update canonical specs, toolchain/CLI docs, diagnostics, generated artifacts, and release validation.
- [ ] 3.5 Run typecheck, Biome, full tests, release-candidate checks, and `pnpm check`.
