## Context

See proposal.md. All removals are verified zero-caller by grep across `src/` and `test/`. Removing `SyntaxCorrespondence.between` from `ProjectAnalysis.ts:122` deletes a full-tree FNV fingerprint that runs on every changed module each revision while nothing reads the result.

## Decisions

- **`Analysis` projections**: delete the nine zero-caller functions; move the remaining test-only `*Of` projections into `test/support/projections.ts` so `Analysis.ts` keeps only operations with production callers. `declarationLookup`/`parameterLookup` are forwards into `DeclarationIndex`/Elaboration and are deleted.
- **`SyntaxCorrespondence`**: keep the module; stop calling `between` from `ProjectAnalysis` and delete the `correspondence` field on `Changed`. Reintroduce only behind a real LSP consumer later.
- **`Mir.samples`**: move builders to `test/support/mirSamples.ts`; `Mir.ts` drops its sole `effect/Option` import.
- **`Type.intrinsicConformances`**: delete the empty registry and `intrinsicallyConforms`; its two call sites return before reaching that branch, so behavior is unchanged.
- **`Hir.hasUnavailable`**: delete; express the single test as `Hir.firstUnavailable(fn) !== undefined`.

## Risks / Trade-offs

- [Test-only projections move] → tests import from `test/support`; no production import path changes.
- [Public exports removed] → green-field means no compat contract; `pnpm typecheck` catches any missed call site.

## Validation

`pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`.
