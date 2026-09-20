# Gates: JUL-207 TIR completion

OWNS: packages/compiler/src/**, packages/compiler/test/**, packages/compiler/scripts/generate-documentation.mjs, openspec/changes/establish-authored-hir-pipeline/**, GATES.md

Scope: finish OpenSpec tasks 3.3.2 and 3.3.5 so JUL-207 publishes one portable, directly constructed typed TIR body with dense local identities and no fact-tree conversion path

- [x] G0: this ledger states completion outcomes that can fail
      CHECK: node /Users/juliaortiz/.agents/skills/unlazy/scripts/gate-lint.mjs GATES.md
      EXPECT: LINT OK
      EVIDENCE: LINT OK

- [x] G1: the TIR schema, references, presentation, and canonical codec satisfy their focused tests
      CHECK: CI=true ../../node_modules/.bin/vitest run test/Tir.test.ts test/TirCodec.test.ts test/TirPresentation.test.ts
      EXPECT: Test Files 3 passed
      CWD: packages/compiler
      EVIDENCE: 3 files passed, 49 tests passed

- [x] G2: elaboration constructs checked bodies without the legacy fact-tree boundary
      CHECK: CI=true ../../node_modules/.bin/vitest run test/RecordBoundary.test.ts test/Elaboration.test.ts
      EXPECT: Test Files 2 passed
      CWD: packages/compiler
      EVIDENCE: 2 files passed, 8 tests passed

- [x] G3: the compiler package typechecks after the schema and construction rewrite
      CHECK: pnpm --filter @silklang/compiler typecheck && echo JUL207_COMPILER_TYPES_OK
      EXPECT: JUL207_COMPILER_TYPES_OK
      EVIDENCE: JUL207_COMPILER_TYPES_OK

- [x] G4: compiler and OpenSpec edits are formatted and lint-free
      CHECK: node_modules/.bin/oxfmt --check packages/compiler openspec/changes/establish-authored-hir-pipeline && node_modules/.bin/oxlint packages/compiler && echo JUL207_STYLE_OK
      EXPECT: JUL207_STYLE_OK
      EVIDENCE: JUL207_STYLE_OK

- [x] G5: every task in the OpenSpec change is complete
      CHECK: test "$(openspec instructions apply --change establish-authored-hir-pipeline --json | jq -r '.state + ":" + (.progress.remaining | tostring)')" = "all_done:0" && echo JUL207_OPENSPEC_COMPLETE
      EXPECT: JUL207_OPENSPEC_COMPLETE
      EVIDENCE: JUL207_OPENSPEC_COMPLETE

- [ ] G6: required pull-request checks pass on the branch head
      CHECK: gh pr checks --required && echo JUL207_PR_CI_OK
      EXPECT: JUL207_PR_CI_OK
      EVIDENCE: pending
