## 1. Driver

- [x] 1.1 Add `hostLayout()` to `NativeToolchain.ts`; re-point toolchain tests to it
- [x] 1.2 Create `packages/compiler/src/Driver.ts` (deep import only): `compile(request)`
  orchestrating closure → index → elaboration → ownership → discovery → lowering → backend →
  object → shim → link with closed outcomes naming failing stages with provenance
- [x] 1.3 Per-phase report: elapsed time, input/output counts, diagnostic counts, engine-heap
  totals
- [x] 1.4 Driver tests: compile-and-run equals interpreter result, no-entry outcome, failing-stage
  provenance, report shape

## 2. Continuous gates

- [x] 2.1 Differential harness test over the shared corpus: completing programs agree by exit
  status, trap programs terminate abnormally, recursion programs compile; divergence fails naming
  the program
- [x] 2.2 Determinism audit: syntax, HIR, MIR, and bitcode gates all golden- and repeat-checked
  (already present — verify coverage and reference from the driver tests)

## 3. Package surface

- [x] 3.1 Exports map gains `./Driver` (deep only); release-candidate surface updated

## 4. Inspector lab

- [x] 4.1 Create the direct-link `/docs/labs/pipeline` lab: phases in order with status, counts,
  per-phase diagnostics, elapsed snapshot time, links to each phase lab, planned native stages
- [x] 4.2 Lab tests: healthy overview, damaged program per-phase diagnostic counts

## 5. Close the realignment

- [x] 5.1 Lift the roadmap's grammar freeze note and flip the realignment status to complete

## 6. Verification

- [x] 6.1 Full compiler and docs suites pass; `pnpm check` and release-candidate green
- [x] 6.2 `openspec validate accept-end-to-end-pipeline --type change --strict` passes
