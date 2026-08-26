# Tasks: CI test sharding spike

## 1. Workflow changes (ci.yml only)

- [x] 1.1 Split `validate`: replace `pnpm check` with explicit steps — biome, `turbo run build`,
      `turbo run typecheck`, `turbo run test --filter=!@silk-effect/compiler`, compiler
      non-vitest checks (`unicode:check`, `stdlib:check`, `toolchain:check`,
      `documentation:policy`, `documentation:check`, `documentation:examples`),
      `test:native-acceptance`, `test:scripts` — keeping `release:candidate`
- [x] 1.2 Add `compiler-tests` matrix job (shards 1–4): setup + turbo cache + native cache
      (restore-only) + install → `turbo run build` →
      `test:parallel -- --shard=N/4` with JSON reporter → upload timings artifact
- [x] 1.3 Sanity-check locally: YAML lints (actionlint if available), and vitest accepts the
      exact `--shard`/reporter invocation (`vitest list --shard=1/4` or equivalent smoke)

## 2. Measurement (needs branch pushed — checkpoint with user)

- [x] 2.1 Push spike branch, open draft PR, complete 3 green runs of the sharded workflow
- [x] 2.2 Collect per-run wall clock, per-shard durations (balance spread), and per-file
      timings from the artifacts

## 3. Findings

- [x] 3.1 Append a Findings section to design.md answering the proposal's four questions,
      with an adopt / adjust shard count / abandon recommendation
