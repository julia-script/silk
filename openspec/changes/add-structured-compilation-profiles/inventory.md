# Migration inventory

Generated from tracked source paths before implementation. Generated files are regenerated from their owners.

## Ordinal API and source consumers

- `packages/compiler/src/Stdlib.generated.ts`
- `packages/compiler/stdlib/silk/target.silk`
- `packages/compiler/src/StaticEvaluation.ts`
- `packages/compiler/test/IntrinsicCatalog.test.ts`
- `packages/compiler/src/Intrinsic.ts`
- `apps/docs/content/reference/static-evaluation.md`
- `packages/compiler/scripts/generate-stdlib.mjs`
- `apps/docs/content/reference/compilation-profiles.md`
- `packages/compiler/test/IntrinsicAvailability.test.ts`
- `packages/compiler/test/StaticText.test.ts`
- `packages/compiler/test/StdlibResolution.test.ts`
- `packages/compiler/test/fixtures/intrinsic-inventory.json`

## Static environment and cache users

- `packages/compiler/src/Realization.ts`
- `packages/compiler/src/StaticEvaluation.ts`
- `packages/compiler/src/Driver.ts`
- `packages/compiler/src/NativeToolchain.ts`
- `packages/compiler/src/Analysis.ts`
- `packages/compiler/src/InspectorPanels.ts`
- `packages/compiler/src/ToolchainPlan.ts`
- `packages/compiler/src/Frontend.ts`
- `packages/compiler/src/Residualization.ts`
- `packages/compiler/src/InspectorRegistry.ts`
- `packages/cli/src/BuildPlan.ts`
- `packages/cli/src/BuildBatch.ts`
- `packages/cli/src/ProjectOptions.ts`
- `packages/cli/src/Workflow.ts`
- `packages/cli/src/BuildExeCommand.ts`
- `packages/compiler/test/NativeToolchain.test.ts`
- `packages/compiler/test/StaticText.test.ts`

## Replacement ownership

Target owns versioned machine facts. CompilationProfile owns normalized logical choices and canonical identity. PackageConfiguration owns external binding data, identity, precedence and value conversion. Residualization reuses ordinary Silk static evaluation for source schemas/defaults/predicates. Project/CLI/LSP own selection and application-edge fallback. Driver/backend/toolchain use the published profile; physical final-link inputs remain independently accounted by the existing integrity machinery.
