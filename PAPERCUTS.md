# Papercuts

Format: date · symptom · fix · project. Check here first when tooling is slow or fails mysteriously.

- 2026-09-23 · A delegated worktree had only a partial `node_modules`; `pnpm exec` automatically
  attempted a full install and stalled on unavailable npm registry DNS · Stop the retry, use the
  prepared main checkout's dependency links and binaries for focused checks, and pass Vitest
  `--configLoader runner` to avoid writes through a read-only dependency link · compiler

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
- 2026-09-22 · A fresh codebase-memory index for the task worktree refused to start because a
  pre-coordination or unverified CBM generation was active · Reuse the indexed main Silk checkout
  for structural discovery and use targeted filesystem inspection only for uncovered task-local or
  ignored files · repository
- 2026-09-22 · A `cd` into a temporary probe project reset the shell cwd, and every following
  `pnpm exec silk` call failed with `MODULE_NOT_FOUND` before running · Keep the shell in the
  repository root and address probe projects only through `--manifest-path` · repository
- 2026-09-22 · A large union failed to parse with a misleading `Expected }` at an unrelated later
  variant because one field was named after the contextual keyword `role`; keyword fields such as
  `type` and `unsafe` reported clearly, but `role` did not · Check a new field name against the
  `TokenKind` keyword list before debugging the enclosing declaration · compiler
- 2026-09-22 · A lowering test asserted zero generic parameters because its fixture source named the
  function `run`, a keyword; the parser recovered the whole signature into a flat run of `Error`
  nodes, so the failure surfaced as a wrong count far from the cause rather than as a syntax error ·
  Dump a new fixture with `silk-compiler <file>` before asserting against it, and check fixture
  identifiers against the `TokenKind` keyword list · compiler
- 2026-09-23 · Starting an authorized merge in a linked worktree failed because the sandbox could
  not create the shared Git worktree's `ORIG_HEAD.lock` · Retry the exact scoped merge with approved
  Git metadata access instead of changing branches or bypassing the worktree · repository
- 2026-09-23 · A focused formatter/linter command used repository-root paths after changing its cwd
  to `packages/compiler`, so formatting matched no files and the package-local binary path was
  wrong · Keep the repository root as cwd for root-relative file lists and tool binaries · compiler
- 2026-09-23 · `pnpm exec silk` in a worktree ran the _main checkout's_ CLI: the globally installed
  `silk` shim execs a hard-coded `/Users/.../Documents/dev.nosync/silk/packages/cli/dist/bin.js`, so
  a compiler fix committed only in the worktree was invisible and `silk test` reproduced a bootstrap
  blowup the worktree had already fixed; every self-hosted figure taken this way is a measurement of
  the wrong compiler · Build locally with
  `CI=true node scripts/turbo.mjs run build --filter=@silklang/cli --filter=@silklang/compiler`, then
  run `node packages/cli/dist/bin.js <check|test|build> --manifest-path compiler/silk.toml`, and
  state which binary produced any self-hosted figure · repository
- 2026-09-23 · A bare `{ ... }` block inside a function body is not a nested block: the grammar reads
  the brace as a nominal record literal, so a shadowing fixture silently became a
  `StructLiteralExpression` with a missing type · Use a construct that owns its block (`if true { }`,
  `while`, `unsafe`) when a fixture needs a nested scope · compiler
- 2026-09-23 · `silk format --manifest-path compiler/silk.toml` reformatted every reachable source
  file, not just the ones the task touched, so a task diff suddenly contained unrelated parser and
  HIR churn; reverting it with `git checkout --` then also discarded the task's own edits to the
  same files · Do not run the project formatter on a task branch; if it is run, restore the
  unrelated files individually and re-apply the task edits from the change set · repository
- 2026-09-23 · `match value { true => ..., false => ... }` on a `bool` is `SEM0044 Match does not
cover bool` plus a parse error on each arm: `true` and `false` are not patterns, so the arms read
  as binding identifiers · Use an `if` with a `mut` local; `match` is for unions and enums only ·
  compiler
