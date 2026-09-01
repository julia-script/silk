## 1. Extract header collection

- [x] 1.1 Create `packages/compiler/src/DeclarationIndex.ts`: move `DeclaredName`,
      `DeclaredTypeFact`, `ParameterFact`, `DeclarationFact`, and the header-analysis functions
      (names, parameters, declared types, duplicate detection) out of `SemanticAnalysis.ts`
- [x] 1.2 Add canonical identity states: `Canonical {module, name}` for first present
      occurrences, `Duplicate {original, cause}`, `Unidentified`; populate them during collection
- [x] 1.3 `collectModule(syntax)`: one module's ordered headers plus header diagnostics
      (`SEM0001`, `SEM0003`, `SEM0005`)
- [x] 1.4 Re-export the moved types from `SemanticAnalysis` and consume collected headers there;
      delete the monolith's header half; existing semantic tests stay green

## 2. The closure-wide index

- [x] 2.1 `collect(closure)`: headers in canonical module order (concrete order within a module),
      per-module name lookup (Resolved/Ambiguous/Missing), unified-ordered diagnostics, immutable
- [x] 2.2 Tests: cross-module same-name identities, duplicate-with-cause, unidentified headers,
      header signature resolution, unknown-type diagnostics, canonical ordering, repeat determinism

## 3. Package surface

- [x] 3.1 Export `DeclarationIndex` from the index and exports map; update release-candidate
      surface assertions

## 4. Inspector lab

- [x] 4.1 Create the direct-link `/docs/labs/declaration-index` lab: presets over a closure,
      headers with module, canonical state, and signature; unified diagnostics panel
- [x] 4.2 Lab tests: cross-module headers, duplicate/unidentified marking, header diagnostics

## 5. Verification

- [x] 5.1 Full compiler and docs suites pass; `pnpm check` green
- [x] 5.2 `openspec validate collect-declaration-headers --type change --strict` passes
