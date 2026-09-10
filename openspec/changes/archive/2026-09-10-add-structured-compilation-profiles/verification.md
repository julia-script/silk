# JUL-120 verification

Implementation revision: `698a73ce`. The completion commit changes only this record and the task checklist.

## Required gates

All commands completed successfully, in the required order:

- `pnpm typecheck`
- `pnpm format:check`
- `pnpm lint`
- `pnpm test`: all 22 workspace tasks passed.
- `pnpm check`: all 33 tasks and 17 repository script tests passed.
- `pnpm release:candidate`: all 10 package/release tests passed.
- `openspec validate add-structured-compilation-profiles --strict`: passed.
- `openspec validate --specs`: all 135 main specifications passed.

The compiler suite passed 2,289 tests, native acceptance passed 318 tests, all 60 standard-library
examples compiled, the CLI passed 89 tests, and the language server passed 155 tests. The new native
case `package-parameter-final-defaults` returned its independently specified result.

## Contract evidence

- `StaticText.test.ts` covers admitted nested values, private/missing/unknown bindings, equal and
  unequal same-tier conflicts, provenance rejection and public translation, imported/forward
  target-dependent defaults, helper/default/schema cycles, overridden cycles, final predicates,
  compileError traces, demanded dependency identity, immutable input snapshots, and distinct
  same-target specialization.
- `Target.test.ts` checks canonical input identity, unsupported logical combinations, all four
  versioned descriptions, and pinned LLVM plus independently compiled header-free C/object facts.
  The witness source digest is checked. Darwin deployment is explicitly pinned in its fixture.
- `Project.test.ts`, CLI `BuildBatch.test.ts`, and LSP `Workspace.test.ts` cover named/default
  selection, full overrides, conflicting modes, explicit target/host selection, profile-selected
  artifacts, and identical compiler/editor profile identities across settings changes.
- Static application keys include complete profile and source identity; Driver/backend and native
  artifact keys include complete profile identity. Bootstrap and runtime coordinators are distinct.
- Generated documentation, diagnostic/intrinsic inventories, editor grammar, MIR goldens, package
  exports, and main OpenSpec contracts are synchronized. No executable ordinal target API remains.

## Failures found and resolved during implementation

These were migration failures introduced by this change, not unresolved baseline failures:

- Typecheck found missing `ParamKeyword` entries in the editor tables; both tables and the generated
  grammar were updated.
- Documentation generation found a lost opaque realization catalog in project views and a missing
  discoverable message template for `SEM0214`; both owners were corrected.
- The first complete compiler run failed five tests: target-text expectations in `Mir.test.ts`
  and `ConditionalConformanceDeterminism.test.ts`, the canonical MIR hash in
  `AlgorithmicAcceptance.test.ts`, and both `RecursionStackBoundary.test.ts` cases. The first
  three were updated for complete target encoding. Package ownership validation now uses the
  existing canonical module rule, including numeric segments, fixing the stack cases.
- The first CI release-candidate run found the eight new public compiler subpaths missing from
  its explicit export inventory. The inventory now includes them and verifies the packaged
  `silk/compilation.silk` module.
- New lint findings were corrected; the final lint command reports no findings.

Aggregate ABI, conditional imports, resolved runtime roots, and physical platform supplies remain
outside JUL-120, as specified by the ticket.
