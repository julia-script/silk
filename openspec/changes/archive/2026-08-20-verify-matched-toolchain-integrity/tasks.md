## 1. Artifact identities

- [x] 1.1 Define deterministic identity and digest records for compiler, catalog, source modules, intrinsic inventory, providers, and runtime support.
- [x] 1.2 Make generation exclude paths, timestamps, and other nondeterministic inputs.
- [x] 1.3 Publish the matched identity graph in compiler/distribution metadata and inspectors.

## 2. Validation boundaries

- [x] 2.1 Validate compiler, catalog, source, and intrinsic identities before user source resolution.
- [x] 2.2 Validate selected provider and runtime coverage after reachable intrinsic planning.
- [x] 2.3 Model missing source, malformed distribution, mismatch, unsupported target, unresolved entry, and operational failure distinctly.
- [x] 2.4 Remove fallback search and late backend/runtime reclassification paths.

## 3. Interfaces and verification

- [x] 3.1 Expose structured integrity outcomes consistently through driver, CLI, embeddings, and tooling.
- [x] 3.2 Add stale catalog/source, mismatched intrinsic, missing promised runtime, unsupported target, and valid subset tests.
- [x] 3.3 Add fresh-process deterministic digest coverage in the designated global canary rather than per feature.
- [x] 3.4 Update canonical specs, toolchain/CLI docs, diagnostics, generated artifacts, and release validation.
- [x] 3.5 Run typecheck, Biome, full tests, release-candidate checks, and `pnpm check`.
