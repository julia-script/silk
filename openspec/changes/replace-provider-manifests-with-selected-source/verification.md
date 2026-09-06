# Verification — 2026-09-06

All required gates passed in order: `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`.

- Typecheck: 18 successful tasks.
- Full tests: 22 successful tasks, including 2,324 parallel compiler tests, 321 shared native acceptance cases, 158 LSP tests, 89 CLI tests and the docs/editor suites.
- Check: successful, including repository policy and package checks.
- Release candidate: all 10 tests passed against packed artifacts.
- OpenSpec: strict validation passed.

Earlier failures were consequences of this change and were fixed: OS-provider analysis fixtures needed explicit native profiles; an interactive landing-page example needed a native target in both the live element and verification harness; the release export inventory needed the new SourceCatalog and PlatformCatalog actors. Native provider documentation examples are excluded on the intentionally empty Wasm surface. The final runs have no unresolved failures.

The live element's target-switch test verifies that unchanged source loses stale diagnostics when its target changes. The initial complete native acceptance run executed all 321 cases; subsequent integration runs reused Turbo's successful unchanged compiler task.

Final integrated verification on source head `a239100e` (above origin/main `0ee2ed40`) passed all six
required repository gates. The final stack includes 2,343 compiler tests, 321 actually executed
native acceptance cases, 159 LSP tests, 89 CLI tests, 17 repository policy checks and 10 packed
release-candidate checks. See `../native-assembly-entry-contracts/verification.md` for integration
fixes, logs and the two unrelated baseline OpenSpec delta-validation failures. Submitted through
`gh stack` as draft PR #362; no merge is claimed.
