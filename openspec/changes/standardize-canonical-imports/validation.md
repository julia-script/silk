# Canonical import validation

## Passing checks

- `pnpm typecheck`, `pnpm format:check`, and `pnpm lint` passed locally.
- The production build passed all 11 tasks, including the documentation site.
- Standard-library source and documentation generation passed for all 100 modules, with no documentation-policy violations. All 57 stdlib doctests passed.
- The self-hosted compiler checked and built successfully. Its parser corpus verified all 168 source files with no failures.
- Raw Linux source analysis passed for both x86_64 and AArch64 Linux targets.
- Focused resolution, static evaluation, syntax, import, and ownership suites passed after the caller fixes. The final operator pipeline (2 tests), algorithmic acceptance (1 test), and recovered-writer MIR checks passed.
- `pnpm test:scripts` passed all 19 tests.
- OpenSpec validation and `git diff --check` passed.

## Full-suite outcomes

CI run [34515320421](https://github.com/julia-script/silk/actions/runs/34515320421)
passed validation (including `pnpm release:candidate`), documentation, CLI, LSP,
WebContainer browser, macOS native OS, native acceptance, and compiler shards 1 and 4.
Shards 2 and 3 each found one stale golden caused by changed source spans. The operator
HIR/MIR goldens and algorithmic MIR digest were updated in follow-up commits; their
focused tests passed locally. The operator LLVM IR and bitcode remained unchanged.
Follow-up pushes trigger the complete PR workflow again.

Local `pnpm test` was interrupted while the source migration was still being finalized.
The subsequent `pnpm check` passed formatting, build, lint, typechecking, supporting
package suites, and stdlib doctests, but was stopped after 12 compiler tests exceeded
their timeouts under local load. Therefore local `pnpm check` is **not a passing run**.
The same compiler suites completed in CI, with only the two golden mismatches above.
No baseline local run was performed, so the local timeouts are not claimed to predate
this change.

Local `pnpm release:candidate` also discovered an ignored historical checkout under
`.scratch` and hit the WebContainer test's 5-second timeout in both checkouts. Retrying
with `--exclude '.scratch/**'` selected only the current suite: 5 tests passed and 5
exceeded their existing 5- or 30-second limits (LLVM, compiler, docgen, CLI, WebContainer).
The unmodified release suite passed in CI. These local timeouts are recorded as local
validation failures rather than a passing release check.
