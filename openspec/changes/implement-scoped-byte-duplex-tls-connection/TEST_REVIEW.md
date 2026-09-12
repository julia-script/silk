# JUL-188 test-economics assessment

This is a direct assessment by the implementation agent. The user explicitly instructed this
session not to start a new review-agent cycle; this document does not claim independent approval.
The comparison base is verified JUL-187 merge `6ab7959ad15841f390f326b8bdc1338a5f732624`.

## Placement and distinct claims

- Lifetime, service-row, callable specialization, provider selection, loan joins, and union-carrier
  selection are asserted through analysis, MIR, or layout in existing test files. The two loan
  reproductions execute in roughly 100 ms each; together with the whole-owner negative control
  they completed in 281 ms. The whole-union regression takes about 1.93 s and reproduces without TLS.
- The byte-duplex runtime case stays in the shared native corpus. Its borrowed-provider addition
  distinguishes incorrect reference-field addressing, verifies use before close, and records one
  final close. The complete case passed in 18.90 s with a fresh cache. Its expected virtual-clock
  count includes the additional write; no wall-clock timing assertion was added.
- The existing finalized-destroy corpus source now checks that release/drop have not happened when
  use begins. It retains success, failure, suspension/cancellation, original-outcome, and LIFO
  coverage. The focused native selection passed in 12.22 s.
- TLS native acceptance keeps the complete failure/cancellation matrix in one source program. The
  dedicated Wasm test retains one authenticated exchange, application I/O, and terminal release.
  It does not duplicate the full native matrix. Its separate process prevents unrelated compiler
  graphs from remaining in the same heap.
- No new per-feature process-determinism test, timing assertion, instruction count, or performance
  threshold was added. New small regressions reuse existing worker files. Full suites remain in CI.

## Measured verification

Observed focused groups include 105 type tests, 65 callback/provider tests, 56 instance tests,
48 release-lowering/provider tests, 90 suspension/instance/capture tests, 69 loan/slice/catch tests,
and 58 layout/match tests. Groups overlap and must not be summed as unique coverage. The layout
host-C case initially lacked `llvm-ar` on PATH and passed when rerun with the installed LLVM tools.
Two native slice-lowering tests passed in 2.16 s after the stored-reference correction.

These are observed runs, not a controlled before/after performance benchmark. Cache state,
concurrent checks, and changed compiler phases differ, so no numerical regression delta is claimed.
The initial TLS compilation exhausted 4/6 GiB; exact context-atom interning allowed discovery to
finish in 4 GiB. Subsequent representative Wasm compile/execution attempts took 208–255 s and
exposed distinct correctness failures rather than a timing gate. The final representative Wasm witness passed in 232.61 s. The full native TLS matrix exhausted
4 GiB after 324.10 s and is rerunning with the existing CI TLS allowance of 6 GiB. Final native
outcomes and the exact CI head are recorded in the PR and implementation handoff.

## CI scheduling correction

The implementation CI's last compiler shard passed 566 tests but timed out the single test that
bundled six invalid-generic driver cases under one 60-second budget. That group passed locally in
17.44 s. Each corpus entry now has its own named test and unchanged diagnostic/phase assertions;
the number of compiler invocations is unchanged.

Full verification also timed out an LSP workspace test. Its requested one-worker setting never
reached Vitest because Turbo used strict environment filtering. Before/after dry-run output proves
that `NODE_OPTIONS` and `VITEST_MAX_WORKERS` are now passed to package tasks. The existing concurrency
and configuration checks pass (11 tests). No global timeout or correctness assertion was relaxed.

## Shared-runner regression

The full native TLS matrix completed compilation work within 6 GiB and exposed an invocation-lifetime
mismatch in the MIR verifier (607.46 s locally; 1019.17 s in CI). Two scoped callbacks and a recovered
borrowed method call reproduce it without TLS in 2.03 s. The positive case requires lifetime-erased
runner reuse, and a corrupted provider-type binding still requires `InvalidEffectOperation`.
The native matrix is being repeated after the correction; the failed timings are not runtime passes.

## Assessment

The tests use the cheapest available tier for the observed failures and keep runtime evidence in
the existing corpus or the single target-specific witness. The remaining delivery gate is successful
TLS native execution and exact-head full CI, including `pnpm check` and `pnpm release:candidate`.
