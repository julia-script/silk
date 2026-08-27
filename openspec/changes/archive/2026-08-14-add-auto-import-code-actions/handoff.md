## Implementation Handoff

The auto-import system was measured with the generated regression in
`packages/lsp/test/AutoImportScale.test.ts`. The fixture contains 201 project modules (one open
root and 200 closed candidates) and the active 43-module shipped toolchain catalog.

One local release-check run on 2026-08-14 produced these baseline observations:

- initial inventory: 244 modules and 1,256 public exports indexed; 244 summaries scanned and
  revised; project discovery took 67.08 ms and all project/toolchain summary construction took
  165.56 ms;
- one closed-module edit: two summaries scanned, the unchanged open root reused, the dirty closed
  module revised, no unrelated module removed or rebuilt; discovery/revision took 3.83 ms and
  summary construction took 0.07 ms; and
- exact `symbol137` auto-import discovery took 0.21 ms and returned only `Module137`, while the
  semantic project revision still contained only the open `Main` root.

These values are a development-machine baseline rather than performance budgets. Re-run with
`SILK_AUTO_IMPORT_MEASURE=1 pnpm --filter @silklang/lsp exec vitest run
test/AutoImportScale.test.ts` when changing discovery, invalidation, or indexing behavior.

## Verification

- `pnpm typecheck`: passed.
- `pnpm exec biome check .`: passed with the repository's existing informational
  `useLiteralKeys` suggestion in `packages/compiler/scripts/vendor-unicode-data.mjs`.
- All auto-import compiler, LSP lifecycle, protocol, scale, and real-stdio server tests passed;
  the LSP package reports 87 passing tests.
- `pnpm test` and the test phase of `pnpm check` reach one reproducible pre-existing failure in
  `packages/compiler/test/WasmShadowStackHeapCollision.test.ts:403`: the host-stack stress case
  expects `deep.value` to be `0` but receives `undefined`. The same test fails in isolation with
  one worker. No auto-import source, package, or test participates in that path.
- `pnpm release:candidate`: passed after extending both explicit compiler public-API allowlists for
  `AutoImport`, `ImportPlan`, `ModuleSummary`, `SourceAction`, and `WorkspaceInventory`.
