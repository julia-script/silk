# Gates: compiler revision validation and persistence stack

OWNS: openspec/changes/jul-217-_/\**, openspec/changes/jul-218-_/**, openspec/changes/jul-219-\*/**, openspec/changes/jul-221-*/**, openspec/specs/**, packages/compiler/**, apps/docs/**, GATES.md

Scope: implement JUL-217, JUL-218, JUL-219, and JUL-221 as four stacked compiler changes. Full-repository CI is a workflow gate checked on the exact final PR head after all repository mutations; it is intentionally not a self-modifying ledger gate.

- [x] G0: this ledger states outcome checks that can fail
      CHECK: node /Users/juliaortiz/.agents/skills/unlazy/scripts/gate-lint.mjs GATES.md
      EXPECT: LINT OK
      EVIDENCE: 2026-09-21 LINT OK.

- [x] G1: JUL-217 owns reconstructible semantic query descriptors, ordered revision validation, result-fingerprint cutoffs, and current diagnostic presentation for header/name/conformance readers
      CHECK: openspec validate jul-217-semantic-revision-validation --strict && pnpm --filter @silklang/compiler exec vitest run test/SemanticInvalidation.test.ts test/NameResolution.test.ts test/DeclarationIndex.test.ts && echo JUL217_OK
      EXPECT: JUL217_OK
      EVIDENCE: 2026-09-21 strict OpenSpec validation passed; 100 focused tests passed; JUL217_OK.

- [x] G2: JUL-218 routes checked units, evaluation, residual construction, and ownership through the shared revision validator while preserving complete products and budget policy
      CHECK: openspec validate jul-218-shared-body-evaluation-validation --strict && pnpm --filter @silklang/compiler exec vitest run test/NameResolution.test.ts test/SemanticInvalidation.test.ts test/ProjectAnalysis.test.ts test/StaticText.test.ts && echo JUL218_OK
      EXPECT: JUL218_OK
      EVIDENCE: 2026-09-21 strict OpenSpec validation passed; 97 focused tests passed; JUL218_OK.

- [x] G3: JUL-219 supplies bounded opaque Storage with memory and atomic-filesystem providers and migrates native cache owners without retaining obsolete byte-store contracts
      CHECK: openspec validate jul-219-compiler-storage --strict && pnpm --filter @silklang/compiler exec vitest run test/Storage.test.ts test/NativeToolchain.test.ts && pnpm --filter @silklang/compiler exec vitest run test/Driver.test.ts -t "admits native final caching|artifact Storage|rejects interface before cache reads" && echo JUL219_OK
      EXPECT: JUL219_OK
      EVIDENCE: 2026-09-21 strict OpenSpec validation passed; 49 Storage/NativeToolchain tests and 3 selected Driver migration tests passed; JUL219_OK.

- [ ] G4: JUL-221 persists and strictly admits complete checked units plus ordered dependency manifests through Storage and the shared validator
      CHECK: openspec validate jul-221-persisted-checked-units --strict && pnpm --filter @silklang/compiler exec vitest run test/TirCodec.test.ts test/SemanticPersistence.test.ts test/SemanticInvalidation.test.ts && echo JUL221_OK
      EXPECT: JUL221_OK
      EVIDENCE: pending

- [ ] G5: the final composed head keeps every new OpenSpec implementation task complete and the compiler type-safe
      CHECK: pnpm --filter @silklang/llvm build && pnpm --filter @silklang/compiler typecheck && node -e "const fs=require('fs');for(const p of fs.readdirSync('openspec/changes').filter(x=>/^jul-(217|218|219|221)-/.test(x))){const t=fs.readFileSync('openspec/changes/'+p+'/tasks.md','utf8');if(/- \[ \]/.test(t))throw new Error('incomplete '+p)}console.log('FINAL_LOCAL_OK')"
      EXPECT: FINAL_LOCAL_OK
      EVIDENCE: pending
