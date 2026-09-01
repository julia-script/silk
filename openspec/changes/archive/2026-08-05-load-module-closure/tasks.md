## 1. Grammar: provisional import declaration

- [x] 1.1 Lexer: recognize `import` as a keyword token kind; keyword-prefix rule tests
- [x] 1.2 Parser: `ImportDeclaration` node kind; parse `import <identifier>` as a top-level
      declaration with trivia and missing-name recovery
- [x] 1.3 Parser/lexer tests: import before functions, multiple imports, missing-name recovery,
      losslessness and determinism stay green

## 2. Module phase in the unified diagnostics

- [x] 2.1 Add the `module` phase to the closed phase union, ranked between parser and semantic
- [x] 2.2 Add `MOD0001` (unknown module) and `MOD0002` (self-import) with structured reasons and
      constructors

## 3. Closure loading

- [x] 3.1 Create `packages/compiler/src/ModuleClosure.ts`: `CompilationRequest`
      (root + sources map), `load` producing the closure of `SyntaxFile` artifacts in canonical
      identity order; missing root rejected as a caller error
- [x] 3.2 Import facts: `Resolved`/`Unknown`/`Self`/`Unavailable` with diagnostic causes on
      `Unknown`/`Self` and cause suppression for `Unavailable`
- [x] 3.3 Cycle facts via strongly connected components, members and list canonically ordered;
      self-imports excluded
- [x] 3.4 Tests: diamond closure, unreachable exclusion, missing root, supply-order determinism,
      unknown/self/unavailable imports, mutual cycle, acyclic closure

## 4. Package surface

- [x] 4.1 Export `ModuleClosure` from the index and exports map; update the release-candidate
      surface assertions

## 5. Inspector lab

- [x] 5.1 Create the direct-link `/docs/labs/module-closure` lab: presets, per-module editable
      sources, modules in canonical order with import facts, cycle marks, closure diagnostics
- [x] 5.2 Lab tests: diamond preset, cycle marking, unknown/self diagnostics

## 6. Verification

- [x] 6.1 Full compiler and docs suites pass; `pnpm check` green
- [x] 6.2 `openspec validate load-module-closure --type change --strict` passes
