## 1. Concrete syntax and semantic type vocabulary

- [x] 1.1 Add the `struct` keyword and struct/type-path node vocabulary to tokens, lexing, syntax-tree APIs, and deterministic encoders.
- [x] 1.2 Parse public/default-private struct declarations, sequential public/default-private fields, local and qualified type paths, and empty structs losslessly.
- [x] 1.3 Implement bounded recovery for missing struct names, braces, field names, colons, and types without consuming following fields or top-level declarations.
- [x] 1.4 Add the semantic `Type` actor for built-in and canonical nominal types with equality, ordering, and deterministic encoding, then migrate scalar type consumers.

## 2. Staged declaration and type resolution

- [x] 2.1 Split declaration processing into identity-bearing collected headers and a completed immutable declaration index.
- [x] 2.2 Collect mixed function/struct declarations in concrete order with shared canonical-name duplicate handling and declaration kinds.
- [x] 2.3 Collect ordered struct field headers with identities, visibility, exact type-path syntax, and duplicate-field diagnostics.
- [x] 2.4 Change module-scope bindings to canonical declaration references and make lookups resolve completed facts from the index.
- [x] 2.5 Resolve built-in, local, selected, and namespace-qualified nominal types in fields and function contracts through explicit module scopes.
- [x] 2.6 Diagnose non-type declarations in type position, inaccessible imported types, and public declarations exposing private nominal types with exact provenance.
- [x] 2.7 Build the canonical nominal dependency graph, retain acyclic dependencies, and diagnose direct or mutual inline recursive components deterministically.

## 3. Target-aware nominal layout

- [x] 3.1 Extend layout entries and encoding with aggregate representation, declaration-ordered field offsets, padding, size, and alignment.
- [x] 3.2 Compute an immutable target-selected catalog for every nominal struct, retaining unavailable field and recursion causes without harming unrelated entries.
- [x] 3.3 Derive reachable runtime layout plans from the catalog and existing scalar discovery without recomputing aggregate layout decisions.
- [x] 3.4 Extend layout verification for canonical nominal ordering, field order, offsets, alignment, size, catalog/plan equality, and backend-neutral vocabulary.

## 4. Compiler pipeline and public facade

- [x] 4.1 Thread collected headers, name scopes, completed declaration facts, target selection, nominal layout catalog, and runtime plan through Analysis snapshots.
- [x] 4.2 Thread the staged declaration and catalog flow through Driver phase reporting and keep diagnostics in deterministic phase order.
- [x] 4.3 Add public facade queries and package exports for nominal type, struct field, dependency, catalog, and layout facts.
- [x] 4.4 Keep HIR, ownership, discovery, MIR, evaluation, LLVM, and WebAssembly behavior unchanged for the existing scalar executable surface while accepting nominal declaration facts.

## 5. Inspector surfaces

- [x] 5.1 Extend the declaration-index lab with struct kinds, visibility, ordered fields, resolved type identities, and explicit unavailable states.
- [x] 5.2 Extend the unified `/labs` target-layout pane with nominal catalog entries, field offsets, padding, dependencies, unavailable recursion causes, and the distinct reachable runtime plan.
- [x] 5.3 Add browser-local presets and accessible text equivalents for empty, nested, imported, private, damaged, and recursive structs.
- [x] 5.4 Preserve and extend the automated facade-only import boundary for every new inspector path.

## 6. Determinism and verification

- [x] 6.1 Add lexer/parser tests and corpus fixtures for valid struct syntax, qualified types, all recovery boundaries, and byte-for-byte losslessness.
- [x] 6.2 Add declaration, name-resolution, visibility, dependency-cycle, diagnostic-order, and fresh-process determinism tests.
- [x] 6.3 Add scalar, empty, nested, target-varying, unavailable, catalog-filtering, padding-invariant, verification, and encoding layout tests.
- [x] 6.4 Add Analysis, Driver, docs-lab, facade-boundary, and packed public-export tests for the complete nominal declaration flow.
- [x] 6.5 Run targeted suites during implementation, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`.
- [x] 6.6 Run `pnpm release:candidate`, strict OpenSpec validation, and scenario-by-scenario evidence review before handoff.
