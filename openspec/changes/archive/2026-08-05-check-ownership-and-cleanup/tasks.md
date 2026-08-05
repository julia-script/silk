## 1. Ownership phase

- [x] 1.1 Create `packages/compiler/src/Ownership.ts`: `checkModule(result)` producing per-function
  binding facts (copyable category, live range), closed verdicts (`Satisfied`/`Unavailable` with
  causes), and the per-exit cleanup plan with ordered releases
- [x] 1.2 Deterministic textual encoder for ownership facts and cleanup plans; committed goldens
  (parameterized fixture, damaged fixture) with byte-identical and repeat-determinism tests
- [x] 1.3 Ownership tests: copyable binding facts, unavailable verdicts with causes, empty-release
  return exits, determinism

## 2. Facade

- [x] 2.1 Compute ownership per module in `Analysis.make`; add `ownershipOf(snapshot, module)`
- [x] 2.2 Facade test covering the ownership query

## 3. Package surface

- [x] 3.1 Export `Ownership` from the index and exports map; update release-candidate surface

## 4. Inspector lab

- [x] 4.1 Create the direct-link `/docs/labs/ownership` lab: per-function binding timelines
  (category + live range spans), verdicts with unavailable states, cleanup plan per exit
- [x] 4.2 Lab tests: binding timeline, empty-release plan shown explicitly, unavailable verdict

## 5. Bookkeeping

- [x] 5.1 Mark the roadmap's open question resolved (stay frozen)

## 6. Verification

- [x] 6.1 Full compiler and docs suites pass; `pnpm check` and release-candidate green
- [x] 6.2 `openspec validate check-ownership-and-cleanup --type change --strict` passes
