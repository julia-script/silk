## 1. Elaboration phase

- [x] 1.1 Create `packages/compiler/src/Elaboration.ts` from the monolith's body half:
      `elaborateModule(syntax)` with the existing fact shapes, header consumption via
      `DeclarationIndex`, and unchanged body diagnostics
- [x] 1.2 Delete `packages/compiler/src/SemanticAnalysis.ts`; update compiler tests to
      `Elaboration` (import swap; assertions unchanged)

## 2. HIR

- [x] 2.1 Create `packages/compiler/src/Hir.ts`: typed core operations (integer literal,
      parameter reference, canonical-target call), explicit unavailable states with causes,
      normalized contracts
- [x] 2.2 Project HIR per function inside elaboration; publish `hir` on the elaboration result
- [x] 2.3 Deterministic textual HIR encoder; committed goldens (accepted multi-function fixture,
      damaged fixture) with byte-identical and repeat-determinism tests

## 3. Downstream migration

- [x] 3.1 Re-point `BootstrapEvaluation` to the elaboration result (mechanical import/type swap)
- [x] 3.2 Re-point the inspector flow model and syntax-inspector semantic views
- [x] 3.3 Update package exports and the release-candidate surface (`./Elaboration`, `./Hir`;
      remove `./SemanticAnalysis`)

## 4. Inspector

- [x] 4.1 Add the HIR panel to the syntax lab: per-function contract and typed operations with
      unavailable states explicit
- [x] 4.2 Type-and-span reveal on hover/focus for HIR expression entries
- [x] 4.3 Inspector tests: HIR view content, hover reveal markup, unavailable marking

## 5. Verification

- [x] 5.1 Full compiler and docs suites pass; `pnpm check` and release-candidate green
- [x] 5.2 `openspec validate elaborate-bodies-to-hir --type change --strict` passes
