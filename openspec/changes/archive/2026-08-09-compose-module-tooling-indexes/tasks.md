## 1. Module Tooling Ownership

- [x] 1.1 Add the immutable module tooling actor owning one module's exact semantic input, occurrence module index, and anonymous-expression entries, with explicit compiler exports and construction tests.
- [x] 1.2 Refactor semantic occurrence construction into module-local construction plus project composition while preserving point/range query ordering and occurrence performance bounds.
- [x] 1.3 Build a current-project declaration-location registry during composition and make occurrence queries replace any predecessor location with the current location.

## 2. Incremental Tooling Composition

- [x] 2.1 Extend frontend tooling construction with an optional prior module-tooling map and reuse only same-module artifacts whose semantic input is the exact current semantic artifact.
- [x] 2.2 Compose project semantic-occurrence and anonymous-expression indexes shallowly from current module artifacts and retain the module-tooling map as the next revision's immutable basis.
- [x] 2.3 Preserve fresh single-root analysis and all project root views on the same tooling construction boundary without mutable caches or predecessor fallbacks.
- [x] 2.4 Add exact module reuse counters to both tooling phase observations while preserving existing input, output, diagnostic, and ordering meanings.

## 3. Correctness and LSP Coverage

- [x] 3.1 Add project fixtures asserting exact tooling artifact, occurrence module-index, and anonymous-expression array identities for unrelated, body-only dependency, contract, missing-prior, and fresh revisions.
- [x] 3.2 Prove definition navigation from a reused importer resolves a moved dependency declaration through the current location registry.
- [x] 3.3 Prove root-order determinism, range-query behavior, diagnostics, and stale LSP revisions remain coherent with composed tooling.

## 4. Verification and Finalization

- [x] 4.1 Run focused compiler and LSP suites, `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`; fix every change-caused failure.
- [x] 4.2 Run `pnpm check`, strict OpenSpec validation, and `pnpm release:candidate` because compiler exports and public types change.
- [x] 4.3 Update task evidence, sync delta specs, archive the completed change, and merge its verified branch to `main` without staging unrelated codebase-memory or architecture-guide artifacts.
