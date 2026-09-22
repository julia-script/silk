# Gates: cache test result records and runner exchange

OWNS: packages/cli/src/TestResult.ts, packages/cli/src/TestExchange.ts, packages/cli/src/Program.ts, packages/cli/src/index.ts, packages/cli/package.json, packages/cli/test/TestResult.test.ts, packages/cli/test/TestExchange.test.ts, packages/cli/test/Program.test.ts, packages/compiler/stdlib/manifest.json, packages/compiler/stdlib/silk/test_exchange.silk, packages/compiler/stdlib/silk/test_runner.silk, packages/compiler/src/Stdlib.generated.ts, packages/compiler/test/DriverNativeAcceptance.test.ts, packages/compiler/test/support/corpus.ts, openspec/changes/cache-test-results/tasks.md

Scope: implement OpenSpec tasks 2.1–2.4 with strict pass records, bounded two-mode exchange, scoped process cleanup, and ordinary Silk runner support.

- [x] G0: this ledger states outcome checks that can fail
  CHECK: node /Users/juliaortiz/.agents/skills/unlazy/scripts/gate-lint.mjs .unlazy/cache-test-results/gates/records.md
  EXPECT: LINT OK
  CWD: ../../..
  EVIDENCE: automatic-evidence=v1; definition-sha256=e35ae7d8d955e9654a0de34288399c6905492cc70e8be9e8d386a28e64357043; exit=0; EXPECT=matched; output-sha256=48630b7361dd44ee870917b12c3d19b9d7bdea738aaca16bb04d4cab83b772d2; output-bytes=8; shell=/bin/sh; cwd=/Users/juliaortiz/intent/workspaces/solitary-marmot/silk; path=530c4a62004b/90 entries

- [x] G1: strict result records, exchange admission, and process cleanup pass focused CLI tests
  CHECK: CI=true ../../node_modules/.bin/vitest run test/TestResult.test.ts test/TestExchange.test.ts test/Program.test.ts --configLoader runner
  EXPECT: Tests  18 passed
  CWD: ../../../packages/cli
  EVIDENCE: automatic-evidence=v1; definition-sha256=4b97941f0668af6a45e56965a2ba59209fdc116b61d92701051c5183ad89c2e9; exit=0; EXPECT=matched; output-sha256=f7bb2f052817e1d5e61a15b95356705e423a44bfa64779296a035b8abadd8224; output-bytes=317; shell=/bin/sh; cwd=/Users/juliaortiz/intent/workspaces/solitary-marmot/silk/packages/cli; path=530c4a62004b/90 entries

- [x] G2: source runner cache and compact-mode behavior passes the shared native acceptance tier
  CHECK: SILK_NATIVE_CORPUS_CASES=test-exchange-binary-framing CI=true ../../node_modules/.bin/vitest run test/DriverNativeAcceptance.test.ts -t "(runs the bundled test runner|runs the native corpus case)" --maxWorkers=1 --configLoader runner
  EXPECT: Tests  2 passed
  CWD: ../../../packages/compiler
  EVIDENCE: automatic-evidence=v1; definition-sha256=52d799902451836dc1c145291aeddeb78619d698133ab078d15ba185e7497e65; exit=0; EXPECT=matched; output-sha256=49b1ef082bfd4d7b753d0ce64eafc9f3dcf76ffad36e360b7c55c6256ae43363; output-bytes=273; shell=/bin/sh; cwd=/Users/juliaortiz/intent/workspaces/solitary-marmot/silk/packages/compiler; path=530c4a62004b/90 entries

- [x] G3: generated standard-library and toolchain artifacts are current
  CHECK: node -e "const {spawnSync}=require('node:child_process');for(const args of [['scripts/generate-stdlib.mjs','--check'],['scripts/generate-toolchain-integrity.mjs','--check']]){const run=spawnSync(process.execPath,args,{stdio:'inherit'});if(run.status!==0)process.exit(run.status??1)}console.log('GENERATED_OK')"
  EXPECT: GENERATED_OK
  CWD: ../../../packages/compiler
  EVIDENCE: automatic-evidence=v1; definition-sha256=352c2744b805a28512e05512ad9b659c28220caa52fd56608b6eb3c2c1d2d7a8; exit=0; EXPECT=matched; output-sha256=063ae4c5ad1ad1712451dd4701490d16e60989fa9fa3c8f3665d3d47fc974073; output-bytes=13; shell=/bin/sh; cwd=/Users/juliaortiz/intent/workspaces/solitary-marmot/silk/packages/compiler; path=530c4a62004b/90 entries

- [x] G4: affected CLI and compiler source and test projects typecheck
  CHECK: node -e "const {spawnSync}=require('node:child_process');for(const [cwd,config] of [['packages/cli','tsconfig.json'],['packages/cli','tsconfig.test.json'],['packages/compiler','tsconfig.json'],['packages/compiler','tsconfig.test.json']]){const run=spawnSync('../../node_modules/.bin/tsc',['-p',config,'--noEmit'],{cwd,stdio:'inherit'});if(run.status!==0)process.exit(run.status??1)}console.log('TYPES_OK')"
  EXPECT: TYPES_OK
  CWD: ../../..
  EVIDENCE: automatic-evidence=v1; definition-sha256=c43ba904e3bfea3942f12a62806861367beb4e413db8a6f5a591ba35682721b5; exit=0; EXPECT=matched; output-sha256=c14599183016d737424e15730fadc0a5981527e632316b121659bc704db32760; output-bytes=9; shell=/bin/sh; cwd=/Users/juliaortiz/intent/workspaces/solitary-marmot/silk; path=530c4a62004b/90 entries

- [x] G5: OpenSpec tasks 2.1 through 2.4 are complete with a whitespace-clean scoped diff
  CHECK: node -e "const fs=require('node:fs');const {spawnSync}=require('node:child_process');const text=fs.readFileSync('openspec/changes/cache-test-results/tasks.md','utf8');for(const id of ['2.1','2.2','2.3','2.4'])if(!text.includes('- [x] '+id+' '))throw new Error('incomplete '+id);const run=spawnSync('git',['diff','--check','--','packages/cli','packages/compiler/stdlib/manifest.json','packages/compiler/stdlib/silk/test_exchange.silk','packages/compiler/stdlib/silk/test_runner.silk','packages/compiler/src/Stdlib.generated.ts','packages/compiler/test/DriverNativeAcceptance.test.ts','packages/compiler/test/support/corpus.ts','openspec/changes/cache-test-results/tasks.md'],{stdio:'inherit'});if(run.status!==0)process.exit(run.status??1);console.log('TASK_SCOPE_OK')"
  EXPECT: TASK_SCOPE_OK
  CWD: ../../..
  EVIDENCE: automatic-evidence=v1; definition-sha256=32bd52b9cb3c52e19702c641d0774977c7e812e40022ac1a7559a1a9bf356e8f; exit=0; EXPECT=matched; output-sha256=2db75f3e316fbbc9287b00cb912edcf1f8ce386328331b0606a51614469a81e8; output-bytes=14; shell=/bin/sh; cwd=/Users/juliaortiz/intent/workspaces/solitary-marmot/silk; path=530c4a62004b/90 entries
