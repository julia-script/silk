## Why

The review found exported and internal symbols with no callers, test fixtures bundled into the production module, and a per-revision full-tree fingerprint that no production code reads. Under the repo's green-field policy these are deleted rather than preserved for compatibility optics.

## What Changes

- **Delete nine zero-caller `Analysis` facade exports** (`structLiteralsOf`, `appliedLayoutsOf`, `unionLayoutsOf`, `unionCallingShapesOf`, `hirUnionConversionsOf`, `mirUnionConversionsOf`, `declarationLookup`, `parameterLookup`, `unionTraceEventsOf`) and move the test-only `*Of` projections to `test/support/`.
- **Delete dead serializers/guards**: `IntrinsicAvailability.encode`, `Type.isOutOfMemoryError`, the permanently-empty `Type.intrinsicConformances`/`intrinsicallyConforms` pair, `ConformanceGoal.dependencies`, `OsRuntime.isSymbol`.
- **Delete dead realization narrowers**: `CallableFieldRealization.callableRealizationOf`/`effectRealizationOf`, `OpaqueRealization.publicOrigin`.
- **Delete dead IR walkers**: `SuspensionMir.hasSuspension`, `Hir.hasUnavailable` (fold its one test into `Hir.firstUnavailable`), `Mir.suspensionControlEdges`.
- **Delete `ModuleTooling.make`** and the `DeclarationIndex.presentParameterNameEntries` no-op alias; drop the `FrontendTooling` re-exports of `ModuleTooling` symbols.
- **Remove the eager `SyntaxCorrespondence.between` call** in `ProjectAnalysis` (no production consumer) and its `Changed.correspondence` field.
- **Relocate `Mir.samples()`** + fixtures to `test/support/mirSamples.ts`, dropping the `effect/Option` import from `Mir.ts`.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

<!-- none: behavior-preserving refactor (skip_specs) -->

## Impact

Removes unused public surface from `Analysis`, `IntrinsicAvailability`, `Type`, `ConformanceGoal`, `OsRuntime`, `CallableFieldRealization`, `OpaqueRealization`, `Mir`, `Hir`, `SuspensionMir`, `ModuleTooling`, `DeclarationIndex`, and `FrontendTooling`. No language behavior changes; `skip_specs: true`.
