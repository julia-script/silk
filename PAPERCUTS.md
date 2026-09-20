# Papercuts

Format: date · symptom · fix · project. Check here first when tooling is slow or fails mysteriously.

- 2026-09-19 · Ran the whole `packages/compiler` vitest suite (about 230 files, 25–40 min locally;
  each `ModuleVerification` shard alone is about 10 min) after every change and as the harness for
  a temporary probe, which cost hours · Typecheck, then run only the test files that exercise the
  change (`CI=true ../../node_modules/.bin/vitest run test/A.test.ts test/B.test.ts`); to learn
  which files matter, grep the tests or run a probe once and keep the list of files it flagged. Run
  the full suite once per milestone and on CI, which shards it four ways and is faster than a
  laptop · compiler
- 2026-09-19 · A full run left going in the background while editing mixed old and new code,
  because each worker loads whatever is on disk when its file starts, so the result proved nothing
  · Never edit under a running suite; stop it first · compiler
- 2026-09-19 · `pkill -f "vitest run"` left worker processes alive that kept writing output ·
  `pkill -f vitest`, then check `ps aux | grep -c "[v]itest"` is 0 · compiler
- 2026-09-19 · `ScannerDeterminism`, `ConditionalConformanceDeterminism` and the `StdlibResolution`
  byte-identity test fail with `ERR_MODULE_NOT_FOUND` for `dist/*.js`; `lsp`/`docgen`/`cli` tests
  fail to resolve `@silklang/*` · Build first:
  `CI=true node scripts/turbo.mjs run build --filter=@silklang/lsp...` · compiler, lsp
- 2026-09-19 · Three `ModuleVerification` cases fail locally with "Missing planned place" ·
  Identical on `main` locally and green on CI; not a regression, do not chase · compiler
2026-09-20 · `pnpm --filter @silklang/compiler exec tsx -e` could not load compiler sources because the workspace LLVM package did not expose `@silklang/llvm/ByteString` · use focused Vitest fixtures (which build workspace dependencies correctly) for runtime inspection · silk
