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

The final local sequence passed in the required order after the compiler/runtime,
CI and LSP test repairs. The final test-only follow-up inherits the workspace's
60-second deadline for real-worker analysis instead of overriding it with 30 seconds.

| Gate                     | Result | Log                              |
| ------------------------ | ------ | -------------------------------- |
| `pnpm typecheck`         | Passed | `.scratch/ci-fix4-typecheck.log` |
| `pnpm format:check`      | Passed | `.scratch/ci-fix4-format.log`    |
| `pnpm lint`              | Passed | `.scratch/ci-fix4-lint.log`      |
| `pnpm test`              | Passed | `.scratch/ci-fix4-test.log`      |
| `pnpm check`             | Passed | `.scratch/ci-fix4-check.log`     |
| `pnpm release:candidate` | Passed | `.scratch/ci-fix4-release.log`   |

The package Vitest logs contain **3,283 passing tests**, including 2,429 compiler,
324 native acceptance, 160 LSP and 87 CLI tests. Turbo reused the successful compiler
and native results from `ci-fix3` because their inputs were unchanged; all 160 LSP
tests ran again with the corrected real-worker deadline. `pnpm check` additionally
passed all 19 repository script tests. Release validation ran all 10 packed-package
and consumer checks. The 53 standard-library doctests passed without skips.

The source and preserved artifact audit passed again in `.scratch/ci-fix4-audit.log`;
strict OpenSpec validation passed in `.scratch/ci-fix4-openspec.log`. Earlier failed,
interrupted and superseded verification runs are recorded in [progress.md](progress.md)
and are not counted as passing gates. Preserved conformance receipts retain their
actual source/tool/supply provenance rather than claiming a new execution revision.

## PR integration validation

Main at `7cc09e57` was integrated with contextual character literals, compact imports,
lexer cleanup and generated untracked toolchain integrity. The source audit was
refreshed for reviewed upstream changes and the matching C data-import repair.

[Implementation CI run 34261542409](https://github.com/julia-script/silk/actions/runs/34261542409)
passed on `d46071aa`, and
[audit CI run 34261608202](https://github.com/julia-script/silk/actions/runs/34261608202)
passed on `eff61c64`. Both runs passed all compiler and native shards, all three
platform-supply lanes, browser/macOS checks and complete validation/release checks.
Linux native shard 2 explicitly passed `foreign-libc-environ-static` and all 111
shard tests. The native job limit is 60 minutes so cold shards can finish; individual
test deadlines and assertions are preserved.

PRs #387 and #388 were merged into `main` at `a67354bc` while the local gate was
finishing. The follow-up contains only the final real-worker test deadline repair
and this completed verification record. The remote runs above precede that test-only
follow-up; its local validation is the complete passing `ci-fix4` sequence.

## Scope boundary

This is core migration closure. Static PIE, retained foreign contexts, advanced
native layouts and aggregate ABI, generalized TLS/linkage, atomics, Wasm
command/reactor profiles and networking retain their individual roadmap obligations.
[JUL-137–146 and the corrected JUL-148/149/150 outcomes](coverage-ledger.md) are tracked
separately. Pinned local conformance is distinct from remote CI; the required CI
matrix is configured, and no unexecuted remote result is claimed.
