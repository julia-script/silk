## 1. Scope Actor Surface

- [x] 1.1 Characterize `Effect`-style zero-data scope actors beside intrinsic and ordinary module APIs, and verify focused name-resolution tests cover selected imports and qualified operations
- [x] 1.2 Add documented ordinary-source scope actors to each affected nonprimitive module, and verify the compiler accepts their selected imports without changing intrinsic or runtime inventories
- [x] 1.3 Retain only intentional aggregation or concept-renaming aliases, and verify every retained alias addresses a surface that one existing actor cannot preserve

## 2. Documentation Migration

- [x] 2.1 Replace redundant aliases in canonical stdlib doc comments, handwritten docs, labs, fixtures, and shipped examples while preserving every nonprimitive qualifier and verify a repository audit finds no redundant example aliases
- [x] 2.2 Keep primitive modules as plain unaliased namespace imports and verify `bool`, `char`, numeric, and pointer-width examples use their lowercase qualifiers
- [x] 2.3 Update reference rendering to emit selected scope actors for nonprimitives and plain imports for primitives, and verify focused docgen tests cover both forms

## 3. Generated Artifacts and Acceptance

- [x] 3.1 Regenerate Unicode source, standard-library embedding, integrity metadata, reference pages, and affected MIR/hash goldens, and verify generated-content checks are clean
- [x] 3.2 Compile all standard-library doctests and run focused compiler, docgen, and Labs preset tests, resolving every change-related failure
- [x] 3.3 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`, and verify all required suites pass
- [x] 3.4 Run `pnpm check` and `pnpm release:candidate`, audit the final diff against the OpenSpec requirements, and verify package contents contain the regenerated canonical sources
