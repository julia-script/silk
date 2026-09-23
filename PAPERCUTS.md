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
- 2026-09-20 · `pnpm --filter @silklang/compiler exec tsx -e` could not load compiler sources
  because the workspace LLVM package did not expose `@silklang/llvm/ByteString` · Use focused
  Vitest fixtures, which build workspace dependencies correctly, for runtime inspection · silk
- 2026-09-20 · Documentation checking hit Node's 4 GB heap after the first target profile because
  the async loop retained the previous whole-project analysis frame · Isolate each profile analysis
  in a helper so only its detached documentation model survives the iteration · compiler
- 2026-09-20 · Running `pnpm --filter` in a detached comparison worktree with symlinked
  `node_modules` tried to replace the modules directory and aborted without a TTY · Invoke the
  primary worktree's `node_modules/.bin/vitest` directly from the comparison package directory ·
  compiler
- 2026-09-21 · The `silk-work` workflow's `superset mcp` transport was unavailable on PATH while
  claiming JUL-209 · Use the connected Linear MCP tools directly and preserve the same admission,
  evidence and reread gates · silk
- 2026-09-21 · Compiler typecheck produced hundreds of misleading missing `@silklang/llvm/*`
  errors in a fresh worktree · Build `@silklang/llvm` once before the focused compiler typecheck ·
  compiler
- 2026-09-21 · Native CLI tests failed with `Unknown attribute kind (102)` because `/usr/bin/clang`
  17 could not read LLVM 22 bitcode · Run native CLI checks with
  `PATH=/opt/homebrew/opt/llvm/bin:$PATH` so the compiler and Clang use the same LLVM generation ·
  cli
- 2026-09-21 · An effect test that locally provided a service failed native emission with an
  unresolved downstream `completeTest` continuation, matching the open generic bracket-runner
  defect · Exercise the closed Effect from a normal test until that separate compiler change lands
  · compiler
- 2026-09-21 · Local `pnpm release:candidate` could not install offline consumers because the pnpm
  metadata mirror lacked unrelated package records · Use exact CI for the full release-candidate
  gate; locally verify that changed export assertions advance past manifest validation · repository
- 2026-09-21 · A zsh test wrapper assigned to the readonly `status` parameter after the test had
  already run · Use a task-specific exit variable or avoid capturing the status when inspecting a
  redirected log · repository
- 2026-09-21 · A temporary comparison-worktree command was rejected because it began with
  `rm -rf` cleanup · Use a unique temporary path and add the worktree directly · repository
- 2026-09-22 · Indexing the current Superset workspace with codebase-memory failed because a
  pre-coordination or unverified generation was active · Inspect the worker log to confirm the
  guard, leave the unrelated indexer alone, and use direct read-only source inspection · silk
- 2026-09-22 · Vitest tried to bundle a temporary config beneath a read-only dependency symlink and
  failed with `EPERM` in `node_modules/.vite-temp` · Pass `--configLoader runner` when using a
  workspace-local temporary Vitest config · cli
- 2026-09-22 · The compiler documentation check could not start in a focused workspace because
  `@silklang/docgen` had neither its workspace dependencies nor a complete build · Run the check in
  a prepared checkout with the docgen dependency graph built first · compiler, docs
