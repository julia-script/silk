# Verification

The implementation removes the borrowed-outcome admission gates and retains ordinary semantic
lifetime checks. Physical failure and loan lowering compares erased runtime types. Suspension
plans and cancellation retain sparse initializedness and the associated boolean flag locals.
Ordinary-source Stream and Box witnesses exercise the feature without compiler-known library names.

Focused iteration covered `RuntimeSliceOwnership`, `SuspensionOwnership`, `SuspensionMir`,
`ExecutionPackage`, `ProjectAnalysis`, `Elaboration`, `DeclarationIndex`, and quantified `Type`
contracts. New shared native cases cover borrowed Box elements, shared and affine stream items,
failure propagation through retry/cleanup/recovery/map/flatMap, and exact partial-owner cleanup
after cancellation and resumption. Each new native case passed individually before the full
milestone. Two concurrent local tests initially timed out and passed when rerun with one worker.

The [growth report](../../../packages/compiler/benchmarks/lifetimes-outcomes.md) records the
opt-in sizes 2, 4, and 8, including invalid sources, actual compiler work, retained flags, and
matching debug/release emission verdicts.

- `pnpm typecheck`: passed after removing stale admission assertions and fixing test fixture types.
- `pnpm format:check`: passed.
- `pnpm lint`: passed after removing unused imports and nested ternaries.
- Diagnostic generation and generated-document checks: passed.
- Standard-library documentation policy: 62 modules passed.
- Standard-library doctests: 60 passed.
- Updated borrowed-failure reference example: analyzed without diagnostics.
- `openspec validate add-borrowed-effect-outcomes --strict`: passed.
- Broader `openspec validate --specs --strict`: 132 passed; the unchanged
  `bootstrap-standard-streams` specification fails because its “Native process destinations are
  explicit” requirement has no scenario. The file matches the pre-change base `ed4b8433`.
- Initial `pnpm test`: compiler parallel suite had 2,269 passing tests and one new diagnostic-span
  assertion failure. Corrected the assertion to accommodate leading whitespace in declaration
  spans; the focused completion test then passed. The native corpus was not reached in that run.
- Final `pnpm check`: passed, including production builds, all 2,270 compiler parallel tests
  across 211 files, all 316 native acceptance tests, remaining workspace tests, and 17 script tests.
- Final `pnpm test`: passed with all 22 Turbo tasks cached; no test bodies were rerun.
- `pnpm release:candidate`: passed all 10 package-consumer checks.
