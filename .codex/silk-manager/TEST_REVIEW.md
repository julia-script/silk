# Test relevance and runtime review

Read this reference for every `silk-work` implementation, including work reached through
`silk-now`.

## Role

Assign one dedicated test-economics reviewer subagent after the implementation and its focused
verification are stable enough to review. The reviewer does not edit project files and must be
distinct from the implementer and from any general code-review agent. It inspects the issue,
acceptance criteria, exact issue-scoped diff, affected production paths, new or changed tests,
nearby tests, and focused test timings.

The reviewer protects useful evidence per unit of default-suite time. Coverage percentage and test
count are not goals. Adding no permanent test is valid when existing evidence already falsifies the
regression or when the surface does not justify a durable test; the reviewer must say why.

## Four required justifications

Every new or materially expanded test must earn its place on all four axes:

1. **Reason to exist** — name the exact behavior or regression claim it protects and the distinct
   failure that neighboring tests would miss. Delete or consolidate tests that only restate existing
   evidence, exercise implementation details without a contract, or exist for coverage optics.
2. **Complexity** — explain why the test uses the cheapest tier, setup, data size, case count,
   backend set, process count, and assertion shape capable of falsifying that claim.
3. **Optimization evidence** — inspect and eliminate repeated parsing, analysis, compilation,
   evaluation, fixture construction, subprocess startup, backend execution, and equivalent cases.
   Examine loop and input-size scaling. Approval requires concrete evidence that no simpler shared,
   cached, table-driven, narrower, or lower-tier design preserves the same signal.
4. **Runtime cost** — measure the focused test cost and, when an existing file or suite grows,
   compare it with the verified PR base under equivalent conditions. State the absolute and
   incremental time and why the protected risk deserves that permanent default-suite cost.

“Fast enough,” “more coverage,” and “important behavior” are not justifications. A test can be
individually quick and still be wasteful through repetition across hundreds of cases.

## Review procedure

1. Inventory every added, removed, or materially changed test from the verified PR-base merge-base.
   Map each surviving test to one acceptance or regression claim.
2. Trace what each test actually executes: setup, source realization, generators, compilers,
   backends, native processes, browser or editor setup, retries, loops, parameter matrices, and
   teardown. Count repeated expensive boundaries rather than judging only the test body.
3. Challenge the execution shape:

   - use the cheapest semantic or structural tier that can falsify the claim;
   - share one immutable analysis or compiled result when isolation does not require rebuilding it;
   - collapse equivalent examples into one representative or a small table with distinct failures;
   - keep native, multi-backend, fresh-process, fuzz, stress, and large-size checks out of ordinary
     feature tests unless the claim is specifically about that boundary;
   - move performance characterization to opt-in benchmarks rather than correctness assertions;
   - test a stable reusable actor instead of exhaustively testing a disposable script, debug page,
     inspection tool, or thin wrapper.

4. Give low-risk support surfaces a high evidence bar. A one-off script or occasional debugging UI
   normally needs a focused smoke path, existing lower-level evidence, or explicit manual
   verification—not a dedicated exhaustive unit-test file—unless a concrete failure would justify
   its ongoing runtime and maintenance cost.
5. Inspect asymptotic behavior. Reject default-suite tests whose nested loops, growing fixtures,
   ordinal sweeps, retries, or cross-products scale beyond the minimum boundary points needed for
   the claim. A stress property belongs in an opt-in target unless repository policy explicitly
   makes it a default-suite invariant.
6. Measure using the repository test runner's focused file or test selection. Use the same machine,
   command, worker settings, and warm/cold conditions for base and branch comparisons. One
   representative measurement is enough for clearly small stable costs; use repeated runs and a
   median only when noise could change the decision. Never add timing assertions to the correctness
   suite. Never switch or modify the implementation checkout to obtain a base measurement; have the
   coordinator provide an isolated temporary worktree or another equivalent clean baseline.
7. If the PR adds no test runtime, confirm that existing tests or other verification adequately
   protect the change and report an incremental default-suite cost of zero.

## Reviewer return

```text
test_inventory:
  - test file and added, expanded, consolidated, or deleted cases
justifications:
  - test or group
    reason_to_exist
    distinct_failure
    complexity_justification
    optimization_evidence
    measured_cost
    benefit_vs_cost
findings:
  - severity, path, evidence, and required simplification or deletion
timing:
  base_command_and_result
  branch_command_and_result
  incremental_default_suite_cost
verdict: approve | revise | reject
```

`approve` requires all four justifications for every added or expanded test and no unresolved
material finding. `revise` means the evidence can become worthwhile after a named optimization,
consolidation, measurement, or explanation. `reject` means the test's value does not justify its
complexity or runtime and it should not ship.

The coordinator verifies the findings, applies valid changes, reruns affected checks, and repeats
this review until the reviewer approves the final committed PR diff. Record the timing delta,
deleted or consolidated tests, and reviewer verdict in the PR and Linear handoff.
