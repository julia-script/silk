## 1. Canonical acceptance fixture

- [x] 1.1 Add canonical `app/Main`, `compiler/Member`, and `compiler/Coverage` Silk fixture files for the fixed-input remaining-member fold.
- [x] 1.2 Load the fixture through the ordinary multi-module source resolver and assert that every compiler phase is available without new diagnostics or compiler API changes.

## 2. Three-engine and determinism acceptance

- [x] 2.1 Execute the resolved fixture through logical MIR evaluation and pin the completed result `42` with its selected guarded-match trace.
- [x] 2.2 Compile and execute the same resolved fixture with the native LLVM and direct WebAssembly backends and require both results to equal logical evaluation.
- [x] 2.3 Add a fresh-process determinism fixture that compares closure, semantic facts, HIR, ownership, instances, layout, MIR, evaluation trace, native text/bytes/symbols, and WebAssembly text/bytes/symbols.

## 3. Unified workbench acceptance

- [x] 3.1 Add the exact canonical root and module bytes as one browser-local algorithmic acceptance preset in the existing `/labs` catalog.
- [x] 3.2 Verify preset-to-fixture byte identity, coordinated phase availability, result `42`, accessible text, and existing-pane selection without a standalone inspector.

## 4. Evidence and repository gates

- [x] 4.1 Record fixed cardinality as the demonstrated boundary and promote runtime-sized compiler data—not a preselected feature list—to Next in `roadmaps/project.md`.
- [x] 4.2 Run focused compiler and workbench tests, `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and strict OpenSpec validation; fix every in-scope failure.
- [x] 4.3 Inspect the final diff for accidental language or public API changes and record any unrelated pre-existing warning exactly before handoff.
