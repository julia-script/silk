# JUL-188 scoped callback investigation

Date: 2026-09-12. Status: **independent constraint bug fixed; captured scoped callback still blocked
pending a material language-design decision**. No lifetime, bracket, TLS, or runtime change was
implemented in this investigation.

## Reconciled revisions and preservation

Live GitHub reads established:

- `main`: `6ab7959ad15841f390f326b8bdc1338a5f732624`, the merge of JUL-187 PR #422.
- PR #422: merged; its final branch head was `14487d3f`.
- PR #424: open, targets `main`, head `77abbaedfde5a61240cc5dcbc0e88359ccfc0a51`.
  Its description still names `e25c2967` and an obsolete stack base. The published tree does
  not contain the later resource-bracket implementation. Its red CI is not evidence about this
  reproduction or the preserved correction set.
- Original worktree: `/Users/juliaortiz/.codex/silk-manager/worktrees/jul-188`, still at
  `d1d707e629a25316f0189cd65e5481114908d8aa` with exactly the five modified files listed in the
  request. Final read: 65 insertions, 23 deletions; original branch unchanged.
- Both preserved stashes remain: `a8c0572105521ac61a7767d15a86c332c92bfc20` and
  `9a2144084596c6c02967100bb79d6bbcb39a62ae`. Neither was applied, popped, modified, or deleted.

This task used the already isolated worktree
`/Users/juliaortiz/.codex/worktrees/a79b/silk`, creating branch
`julia/scoped-resource-callback` at `d1d707e6`. That base was selected to reproduce the actual
bracket under investigation; current main and the published JUL-188 head lack it. No work was
performed in the original implementation worktree. No rebase, merge, PR update, review-agent
cycle, or broader TLS implementation was performed.

## Reproduction and experiment results

`evidence/captured-resource.silk` contains the 22-line representative: Resource, borrowed Config,
a captured higher-ranked callback, and symbolic success/failure/service channels. It imports
only `silk.effect`; it needs no TLS, certificates, native provider, or fixture generation.
The analysis harness retains `main` and uses the existing ordinary-storage test support.

The initial red-capable invocation, run in `packages/compiler`, was:

```sh
../../node_modules/.bin/vitest run test/Suspendability.test.ts \
  -t 'reproduces captured scoped' --maxWorkers=1
```

The acceptance assertion failed with exactly `SEM0089`, reason `ContractRowInference /
NonFiniteRequirementRow`, spanning the bracket call at offsets 522–592. Test execution was
about two seconds after imports. An initial explicit-move mistake in the reproduction was
corrected before the isolation matrix; it is not part of the compiler finding.

The temporary exploratory test was replaced by a permanent characterization named
`preserves symbolic bracket rows without extending captured or resource lifetimes`. Run the
current evidence with:

```sh
cd packages/compiler
../../node_modules/.bin/vitest run \
  test/Suspendability.test.ts test/AnonymousCaptureStabilization.test.ts \
  -t 'preserves symbolic bracket|retains enclosing row' --maxWorkers=1
```

This command passes because it asserts the **current signature's rejection**, direct-forwarding
acceptance/MIR validity, and escape/exclusion negatives. It does not claim that the desired
captured callback has been accepted. Implementation task 1.2 converts the captured case to a
positive assertion against the proposed new signature, leaving escape rejection intact.

Each experiment changed one relevant part of the representative:

| Experiment and distinguishing question                                                                                | Observed result                         | What it establishes                                                                                                                  |
| --------------------------------------------------------------------------------------------------------------------- | --------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| Resource + borrowed config + captured generic callback: is TLS needed?                                                | `SEM0089` at bracket                    | Failure is independent of TLS/provider selection.                                                                                    |
| Remove only config use: is config uniquely responsible?                                                               | `SEM0089`                               | Captured callback validity alone still exposes the mismatch.                                                                         |
| Replace the symbolic row with concrete `&Probe`: is row algebra the root cause?                                       | `SEM0052` at bracket                    | Failure persists with a finite concrete service row.                                                                                 |
| Call top-level `read` instead of the captured callback, retaining config: is a captured generic callback necessary?   | `SEM0089`                               | Borrowed configuration alone also exposes the mismatch; the fallback diagnostic can still mention unresolved bracket row parameters. |
| Remove captures from the anonymous use body: does anonymity alone fail?                                               | No diagnostics                          | A capture-free anonymous use callback works.                                                                                         |
| Pass the generic higher-ranked callback directly, without the anonymous wrapper: does exact symbolic forwarding work? | No diagnostics; MIR verifies            | Existing row forwarding and the ordinary scoped contract compose.                                                                    |
| Return `&resource.value` from use: does the current boundary reject an escaping resource loan?                        | `SEM0089` at bracket                    | The current rigid generic-result/escape boundary remains closed. The diagnostic itself is imprecise.                                 |
| Anonymous body calls a wrapper needing the enclosing row exclusion, before the four-line patch                        | `SEM0074` at `allowed(move body)`       | Anonymous analysis loses lexical constraint evidence independently of the bracket.                                                   |
| Same row-exclusion program with the preserved patch                                                                   | No diagnostics                          | The patch addresses a real independent semantic-context bug.                                                                         |
| Pass the forbidden ambient `ByteDuplex` through that patched wrapper                                                  | `SEM0074` at `outer(ByteDuplex.read())` | Lexical evidence does not waive the concrete caller's exclusion proof.                                                               |
| Remove the allowed `Probe` service from the caller's declared row                                                     | `SEM0071` at `run outer(Probe.read())`  | The fix preserves the callback's requirement rather than dropping it.                                                                |

The exclusion regression uses a test-local `ByteDuplex` service to exercise ordinary nominal
service identity and `Without` constraints without importing transport implementation. It does
not substitute for checking the actual preserved TLS caller. Both regression groups pass after
the independent patch. No prior signature experiment was
repeated: no extra binders, outer bracket lifetimes, resource-state rewrite, explicit compound
row argument, or relaxed lifetime check was introduced.

## Root cause and existing rules

Ranked hypotheses were: an overstrong bracket result environment; loss of a legitimate bound
while opening a higher-ranked callable; independent symbolic-row inference failure. Direct
symbolic forwarding and concrete-row rejection rule out the row as the primary blocker.

Targeted, subsequently removed instrumentation showed:

```text
expected callback: for<'scope> once fn<'env>(&'scope mut Resource)
                   -> once Effect<'scope; A ! E ? R>
actual result:     once Effect<'env1; A ! E ? R>
actual bounds:     'env: 'env1
                   'scope: 'env1
```

`DeclarationFacts.executableLifetimes` creates the extra environment when retained inputs do
not have a known common lifetime. The anonymous callable quantifies its resource input, but
its result retains this distinct environment and the captured validity dependencies.
`inferQuantifiedExecutable` opens the invocation lifetime rigidly; ordinary result variance
would require actual environment `'env1` to outlive expected `'scope`. The compiler only has
an edge in the opposite direction. This is not a missing proof that can be supplied by treating
the current lower bound as an equality.

The existing `LIFE-004` contract, and the callable/lifetime specs, distinguish invocation and
capture validity and forbid strengthened offered validity. Thus accepting this callback under
the **unchanged** bracket signature is not justified. The later `NonFiniteRequirementRow`
diagnostic masks that earlier mismatch; merely changing the row solver would not fix it.

The generated `'env1` is a fresh **common lower bound**, not proof of a mathematical greatest
lower bound. This investigation does not establish that every imaginable source-level adapter
is impossible. It establishes the exact failed obligation and does not find a sound existing
contract for the required captured generic composition. The concrete proposed extension is
in `design.md`: express a finite intersection explicitly and derive it from complete retention
information, without reversing arbitrary bounds or introducing an extra universal binder.

## Independent compiler fix

Commit: `81eb46676f74ba656ca2f5b766a2a035f7d4f15b`

`ExpressionAnalysis.analyzeAnonymousCallable` now forwards the enclosing `constraints` and
`constraintContracts` to the anonymous preliminary declaration and therefore to the hidden
body derived from it. Anonymous collection already inherits the enclosing canonical type
parameters and their substitutions, and independent anonymous generics/constraints are not
introduced. The patch supplies lexical evidence for those same parameters; it does not create
new evidence, prove a failed lifetime, or suppress validation of concrete calls.

The isolated before/after row-exclusion test establishes its necessity, and the forbidden-row
and missing-service negatives constrain its effect. This is exactly the four-line change already
present in the original worktree, now assessed and committed independently with regressions.

## Verification

On commit `81eb4667`:

- `node_modules/.bin/tsc -p packages/compiler/tsconfig.test.json --pretty false`: **passed**.
- Oxfmt check of the three changed TypeScript files: **passed**.
- Oxlint on the same files: **passed**.
- Focused suites `AnonymousCaptureStabilization`, `Suspendability`, and
  `StoredCallableOwnership`, one worker: **41 tests passed**, 3 files, 99.85 s wall time.
  This includes existing structural bracket/finalizer and callable-ownership checks.
- The direct generic bracket positive lowers and passes MIR verification. The desired captured
  representative remains rejected; no backend or runtime fix is claimed.
- `openspec validate support-scoped-callback-environments --strict`: **passed**.
- `git diff --check`: **passed**.

No new cancellation/runtime mechanism was changed, and no native/TLS runtime acceptance sweep
was run. Broad `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and
`pnpm release:candidate` were not run locally, following the explicit focused-local/CI direction.
No broad CI run validates these commits: automatic approval review rejected the isolated branch
push because it did not find sufficient authorization/trust evidence for that publication.
The task did not bypass the rejection, and no PR was created or modified.

## Preserved JUL-188 caller

**Not fixed and not rechecked.** The required gate says to prove the tiny captured representative
before checking the preserved TLS caller. That positive gate still fails. Only the independent
ExpressionAnalysis correction was copied for assessment; the other four TLS correction files
were not installed into this worktree. Earlier reports of three TLS diagnostics remain
historical evidence, not a result of this session. There is no evidence here that the actual
preserved caller now analyzes or lowers.

## Integration instructions for the paused implementer

The branch is local in the same shared repository; its final tip includes a separate planning
commit containing this report, the proposal/design/specs/tasks, and the standalone reproduction.
Use `git log d1d707e6..julia/scoped-resource-callback` to identify the exact two task commits.

1. Keep the original five-file correction set and both stashes. When the original implementation
   session resumes, save its current corrections as its own coherent commit before integrating
   another commit; do not pop either experimental stash.
2. Cherry-pick `81eb46676f74ba656ca2f5b766a2a035f7d4f15b`. Its ExpressionAnalysis hunk is already in
   that correction set; a three-way integration should retain the identical code and add the
   two focused regression groups. Preserve the original TLS files.
3. Cherry-pick the separate `docs(openspec): propose scoped callback environment intersections`
   commit, or inspect its artifacts on `julia/scoped-resource-callback` until the design is
   accepted. It changes planning/evidence only.
4. Do not treat either commit as unblocking JUL-188. Obtain the explicit design decision, then
   implement the accepted proposal in an isolated compiler task. Prove the tiny captured case
   and negatives before checking the preserved caller.
5. Integrate only the task commits when reconciling the larger stack with main. This branch's
   historical base includes unpublished JUL-188 work and is not a main-ready merge range.

Exact next decision: **approve or reject explicit finite lifetime intersections for returned
Effect environments and the corresponding anonymous elaboration/bracket contract**. The
proposal is prepared for review; none of its material language changes is implemented.
Publication of `julia/scoped-resource-callback` to `julia-script/silk` also requires approval
following the automatic review rejection.
