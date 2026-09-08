# Native runtime migration verification

The worktree implements JUL-129, JUL-134, JUL-136 and JUL-130 and completes their
core migration and documentation audit under JUL-147. All required repository and
release-candidate gates passed before integrating main at `7cc09e57`. Post-merge
validation is running; the original results below retain their original scope. No remote CI run or Linear status change is claimed.

## Delivered behavior and evidence

| Ticket  | Delivered behavior                                                                                                                                                                           | Maintained evidence                                                                                                                                                                                          |
| ------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| JUL-129 | Selected source storage providers own allocation and accounting. Execution, Wake, suspension, cancellation and transparent finalization retain exact owned captures and diagnostic outcomes. | [Storage contract](storage-contract.md), [removal inventory](storage-inventory.md), [24 native reentry/finalization lanes](storage-reentry-conformance.json), [40 Wasm lanes](storage-wasm-conformance.json) |
| JUL-134 | Source providers own spawning, argv/environment/cwd staging, startup confirmation, concurrent stdout/stderr capture, descriptor release and child reaping.                                   | [Admission and protocol](process-admission.md), [real and fault conformance](process-conformance.json), [pinned supplies](process-supplies.json)                                                             |
| JUL-136 | Source raw Linux entry handles the kernel stack and auxv, syscalls, checked mapping ownership, application invocation and termination in static non-PIE executables.                         | [Raw contract](raw-admission.md), [four executed target/optimization lanes](raw-conformance.json), [UAPI catalog](raw-catalog.json)                                                                          |
| JUL-130 | Installed source compositions own native/Wasm startup, process inputs, bounded diagnostics/reporting and terminal policy. Compiler, CLI, LSP and inspection use the selected configuration.  | [Hosted inventory](hosted-inventory.md), [66 native startup lanes](hosted-start-conformance.json), [input contract](host-input-contract.md), [report ownership](report-context-contract.md)                  |
| JUL-147 | Removed compiler/runtime paths are absent, retained compiler privileges are justified, and current docs/catalogs/examples agree with source ownership.                                       | [Complete WS/SPEC ledger](coverage-ledger.md), [privilege audit](privilege-audit.md), [source audit](source-absence-audit.json), [artifact audit](artifact-absence-audit.json)                               |

The source audit covers 294 compiler/CLI modules. The artifact audit verifies 124
native object inspections and 40 Wasm export sets against 41 exact retired names.
Its repeatable verifier is `packages/compiler/scripts/check-native-migration-audit.mjs`;
`--artifacts` additionally requires the preserved local conformance reports. Reports
retain their exact source/tool/supply provenance. Missing or changed evidence fails.

## Repository gates

| Gate                     | Result | Log                                        |
| ------------------------ | ------ | ------------------------------------------ |
| `pnpm typecheck`         | Passed | `.scratch/migration-final11-typecheck.log` |
| `pnpm format:check`      | Passed | `.scratch/migration-final11-format.log`    |
| `pnpm lint`              | Passed | `.scratch/migration-final11-lint.log`      |
| `pnpm test`              | Passed | `.scratch/migration-final11-test.log`      |
| `pnpm check`             | Passed | `.scratch/migration-final11-check.log`     |
| `pnpm release:candidate` | Passed | `.scratch/migration-final11-release.log`   |

The final11 sequence ran the gates in the required order. Turbo reused passing
results for unchanged package inputs: all 2,420 compiler tests and all 324 native
acceptance tests ran successfully in final9; the final snippet changes passed all
47 editor tests and 18 docs app tests in final10. The full repository test set is
3,372 passing tests. `pnpm check` additionally passed all 17 repository script tests,
and release-candidate validation executed all 10 packed-package/consumer checks.

Earlier failures and owner-level repairs are recorded in [progress.md](progress.md),
including the final10 stale export assertion corrected before the passing release
run. No failed or interrupted run is presented as passing evidence. The source and
artifact audit passed again in `.scratch/migration-final10-audit.log`.

## PR integration validation

The implementation PR merges main at `7cc09e57`, preserving contextual character
literals, compact import formatting, the lexer cleanup/documentation and generated
untracked toolchain integrity. The merge preserves explicit module analysis in the
landing-page verifier while adopting LSP diagnostic projection.

Post-merge type checking, formatting and linting pass. The focused character-literal,
lexer and formatter suites pass all 129 tests. The source audit was refreshed for the
six changed upstream compiler modules, and both source/artifact absence checks pass.
The superseded local post-merge run was interrupted by a concurrent rebuild; it is
not passing evidence. CI findings and their reproductions are recorded in
[progress.md](progress.md). Fixes in `a4fde63a` freeze Debian indexes, select explicit
filesystem execution storage, honor configured LLVM tools in native tests, and give
the aggregate doctest sweep its measured CI allowance. Clean Linux supply builds,
24 header checks, both Darwin filesystem lanes and eight native tests pass. The
`ci-fix2` ordered typecheck/format/lint/test sequence passed, including 2,428 compiler
and 324 native acceptance tests. Its following check was stopped to incorporate the
remaining Linux data-import and LSP worker repairs in `58564933`. Those fixes pass
55 ABI/planning tests, 24 workspace-engine tests, 16 stdio tests and exact GNU
executable codegen. The replacement complete stack sequence runs in
`.scratch/ci-fix3-{typecheck,format,lint,test,check,release}.log`; final results are pending. Earlier conformance receipts preserve their recorded source/tool/supply
provenance and are not represented as newly executed on the merged revision.

## Scope boundary

This is core migration closure. Static PIE, retained foreign contexts, advanced
native layouts and aggregate ABI, generalized TLS/linkage, atomics, Wasm
command/reactor profiles and networking retain their individual roadmap obligations.
[JUL-137–146 and the corrected JUL-148/149/150 outcomes](coverage-ledger.md) are tracked
separately. Pinned local conformance is distinct from remote CI; the required CI
matrix is configured, and no unexecuted remote result is claimed.
