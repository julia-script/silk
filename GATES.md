# Gates: compiler architecture Milestone B stack

OWNS: openspec/changes/**, openspec/specs/**, packages/compiler/**, apps/docs/**, GATES.md

Scope: implement JUL-210 through JUL-215 as six stacked, coherent compiler architecture changes and prove the final head.

- [x] G0: this ledger states outcome checks that can fail
  CHECK: node /Users/juliaortiz/.agents/skills/unlazy/scripts/gate-lint.mjs GATES.md
  EXPECT: LINT OK
  EVIDENCE: 2026-09-21 `LINT OK`.

- [x] G1: JUL-210 provides the authoritative declaration/header semantic-query boundary and its OpenSpec contract
  CHECK: openspec validate jul-210-semantic-declaration-queries --strict && pnpm --filter @silklang/compiler exec vitest run test/NameResolution.test.ts test/DeclarationIndex.test.ts && echo JUL210_OK
  EXPECT: JUL210_OK
  EVIDENCE: 2026-09-21 strict validation passed; 86 focused tests passed; `JUL210_OK`.

- [x] G2: JUL-211 routes checked bodies and static evaluation through observed semantic queries without losing editor reuse
  CHECK: openspec validate jul-211-semantic-body-evaluation-queries --strict && pnpm --filter @silklang/compiler exec vitest run test/ProjectAnalysis.test.ts test/StaticText.test.ts && echo JUL211_OK
  EXPECT: JUL211_OK
  EVIDENCE: 2026-09-21 strict validation passed; 65 focused tests passed; `JUL211_OK`.

- [x] G3: JUL-212 publishes a detached concrete instance graph through Realization.instantiate
  CHECK: openspec validate jul-212-instance-artifacts --strict && pnpm --filter @silklang/compiler exec vitest run test/Instances.test.ts && echo JUL212_OK
  EXPECT: JUL212_OK
  EVIDENCE: 2026-09-21 strict validation passed; 58 focused tests passed; `JUL212_OK`.

- [x] G4: JUL-213 publishes pre-reachability type layouts and instance-dependent runtime plans through explicit layout APIs
  CHECK: openspec validate jul-213-layout-runtime-plans --strict && pnpm --filter @silklang/compiler exec vitest run test/Layout.test.ts && echo JUL213_OK
  EXPECT: JUL213_OK
  EVIDENCE: 2026-09-21 strict validation passed; 33 focused tests passed after preserving catalog entry identity; `JUL213_OK`.

- [x] G5: JUL-214 lowers explicit instance/layout/runtime inputs to truthfully staged and audited MIR
  CHECK: openspec validate jul-214-explicit-mir-lowering --strict && pnpm --filter @silklang/compiler exec vitest run test/Mir.test.ts test/MirNormalization.test.ts && echo JUL214_OK
  EXPECT: JUL214_OK
  EVIDENCE: 2026-09-21 strict validation passed; 37 focused tests passed; `JUL214_OK`.

- [x] G6: JUL-215 separates backend emission, materialization, support preparation and final linking without semantic re-entry
  CHECK: openspec validate jul-215-emission-linking --strict && pnpm --filter @silklang/compiler exec vitest run test/Backend.test.ts test/NativeToolchain.test.ts && echo JUL215_OK
  EXPECT: JUL215_OK
  EVIDENCE: 2026-09-21 strict validation passed; 69 focused tests passed; `JUL215_OK`.

- [x] G7: the composed final compiler head typechecks with every OpenSpec implementation task complete
  CHECK: pnpm --filter @silklang/llvm build && pnpm --filter @silklang/compiler typecheck && node -e "const fs=require('fs');for(const p of fs.readdirSync('openspec/changes').filter(x=>/^jul-21[0-5]-/.test(x))){const t=fs.readFileSync('openspec/changes/'+p+'/tasks.md','utf8');if(/- \[ \]/.test(t))throw new Error('incomplete '+p)}console.log('FINAL_LOCAL_OK')"
  EXPECT: FINAL_LOCAL_OK
  EVIDENCE: 2026-09-21 LLVM build and compiler typecheck passed; every JUL-210..215 task is checked; `FINAL_LOCAL_OK`.

- [ ] G8: required pull-request CI passes on the exact final stack head
  CHECK: gh pr checks --required && echo FINAL_CI_OK
  EXPECT: FINAL_CI_OK
  EVIDENCE: pending
