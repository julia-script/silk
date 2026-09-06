# Verification — 2026-09-06

All required gates passed in order: `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`.

- Typecheck: 18 successful tasks.
- Full tests: 22 successful tasks, including 2,324 parallel compiler tests, 321 shared native acceptance cases, 158 LSP tests, 89 CLI tests and the docs/editor suites.
- Check: successful, including repository policy and package checks.
- Release candidate: all 10 tests passed against packed artifacts.
- OpenSpec: strict validation passed.

Earlier failures were consequences of this change and were fixed: OS-provider analysis fixtures needed explicit native profiles; an interactive landing-page example needed a native target in both the live element and verification harness; the release export inventory needed the new SourceCatalog and PlatformCatalog actors. Native provider documentation examples are excluded on the intentionally empty Wasm surface. The final runs have no unresolved failures.

The live element's target-switch test verifies that unchanged source loses stale diagnostics when its target changes. The initial complete native acceptance run executed all 321 cases; subsequent integration runs reused Turbo's successful unchanged compiler task.
