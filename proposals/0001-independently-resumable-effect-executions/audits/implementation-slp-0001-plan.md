# SLP-0001 implementation plan

SLP: `proposals/0001-independently-resumable-effect-executions/proposal.md`
SLP revision: 31
SLP digest: `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`
Integration branch: `julia/slp-0001-reaudit`
Started: 2026-08-23
State: Done

## Dependency DAG

```text
establish-independent-execution-semantics
  -> add-independent-execution-packaging
    -> add-external-wake-parking
      -> add-independent-execution-engine-parity
        -> prove-independent-execution-separation
```

The SLP-0002 prerequisite for the final separation slice is archived and satisfied. Every layer has
width one because each SLP-0001 change depends on the preceding change and the compiler work shares
core semantic, MIR, ownership, evaluator, and backend files.

## Execution policy

Each change runs in its own worktree. A change must complete its OpenSpec tasks, hard gates, and one
three-lens conformance pass before integration. Hard-gate fixes are capped at three distinct root
causes; verified in-scope Critical/High conformance findings receive at most one fix pass. After each
merge, the integration branch runs the full repository gates before the next layer starts.

## Layer outcomes

| Layer | Change | State | Gate attempts | Stop reason | Findings |
| --- | --- | --- | ---: | --- | ---: |
| 1 | `establish-independent-execution-semantics` | Done | 3 + 1 post-audit rerun | Integrated at `ffb0ec2`; barrier passed | 21 reviewed; 11 fixed, 10 rejected as out of scope |
| 2 | `add-independent-execution-packaging` | Done | 3 initial fixes; 3 resume attempts + 1 post-audit rerun | Integrated at `ff39ca0`; barrier passed | 1 High fixed; 1 High rejected as Layer 3 scope; Medium/Low recorded |
| 3 | `add-external-wake-parking` | Done | 3 initial fixes + 1 conformance rerun + 1 resume bootstrap fix | Integrated at `f2e82a8`; barrier passed | 7 reviewed; 3 High fixed, 3 High rejected as Layer 4 scope, 1 Medium recorded |
| 4 | `add-independent-execution-engine-parity` | Done | 3 + 1 post-conformance rerun | Integrated at `81f55ac`; barrier passed | 9 reviewed; 2 Critical, 5 High, and 2 Medium fixed |
| 5 | `prove-independent-execution-separation` | Done | 3 initial; 2 resume; 3 post-conformance; 1 gate-resume | Integrated at `dd8e336`; barrier passed | 7 verified; 1 Critical and 6 High fixed; gate-cost blocker fixed |

## Integration gates

Layer 1 isolated gates passed: focused 77/77, typecheck 24/24, Biome 980 files, test 28/28
(compiler 2,090/2,090 plus native differential 1/1), check 42/42 plus scripts 16/16, and
release candidate 9/9. Its integrated full hard-gate command also exited successfully.

Layer 2 is preserved on branch `julia/slp0001-packaging` at `23e4550`. Its focused suite passed
47/47, but the fourth distinct hard-gate root cause caused ten finite Effect-join regressions after
2,090 compiler tests passed. The bounded gate budget was exhausted, so the change was not merged,
its conformance pass was not started, and downstream layers were parked.

Initial stop action was to start a fresh bounded implementation run for
`add-independent-execution-packaging`; trace represented-executable layout selection in the finite
Effect-join path, restore `Completed` outcomes without weakening execution-package planning, rerun
all hard gates, and then run the single three-lens conformance pass.

Resume 1 started after opening draft PR #248. The preserved Layer 2 commit is the implementation
baseline; the fresh gate budget applies only to new root causes discovered in this resumed run.

Resume 1 completed at `ff39ca0`. The finite-join regression was traced to missing composite-Effect
verification for the newly valid standalone represented layouts. The conformance pass then found
and fixed exact Wasm cleanup for represented body, callback, and endpoint environments. Final
isolated gates passed: focused 59/59, typecheck 24/24, Biome 983 files, test 28/28 (compiler
2,101/2,101 plus native corpus 1/1), check 42/42 plus scripts 16/16, and release candidate 9/9.
The integrated full hard-gate command also exited successfully.

Layer 3 started from integration commit `47c7dad` on branch `julia/slp0001-wake-parking`.
Its first checkpoint assigns the sealed Wake type, park/wake MIR contract, and canonical wake-cell
authority/state machine to this layer; full execution-owned continuation realization remains the
explicit Layer 4 boundary.

Layer 3 reached `91a622b` with 16/16 tasks and three verified High conformance findings fixed.
Its single post-conformance rerun passed typecheck and Biome, then parked on stale generated
`diagnostics.md`. A fresh bounded resume will generate/check the documentation and rerun the full
hard-gate sequence before integration.

The bounded Layer 3 resume completed on `julia/slp0001-wake-parking-resume`. It regenerated and
checked `diagnostics.md`, passed the focused 58-test conformance regression set and strict OpenSpec
validation, then passed typecheck 24/24, Biome over 986 files, test 28/28 (compiler 2,118/2,118 plus
native corpus 1/1), check 42/42 plus scripts 16/16, and release candidate 9/9. The fresh worktree
needed one prerequisite repair: a root topological build supplied missing compiler,
documentation, LLVM, and Wasm package outputs before documentation generation. No semantic source
change or new conformance finding was introduced, so the existing single three-lens pass was not
repeated.

Layer 3's integrated full hard-gate command also exited successfully. Layer 4 now owns complete
transition integration and evaluator, Wasm, native, reactor, and differential parity.

Layer 4 checkpoint `7373f5a` adds the composed target-neutral transition actor, canonicalizes
ExternalPark reachability on the sealed `ExecutionPark` operation identity, and retains evaluator
machine stacks across park, notification, eligibility, guard cleanup, resume, and completion.
Focused verification is 33/33; OpenSpec progress is 2/15.

Layer 4 checkpoint `34a9922` completes externally parked Wasm resume, readiness-callback
reconstruction, evaluator-to-Wasm later-owner transfer, and a non-LIFO-safe fixed-slot coroutine
frame freelist. Focused verification is 27/27 with compiler TypeScript and targeted Biome green;
native parity and cancellation/two-root matrices remain.

Layer 4 checkpoint `83798f7` adds native construction, drive, park, wake, frame/package cleanup,
and external status-2 relay. The designated evaluator/native differential case is 42/42, and Wasm
reverse-order root isolation is repaired by clearing transient transfer state. Cancellation,
DestroyPending, exact multi-package dispatch, and reactor evidence remain.

Layer 4 checkpoint `1545bcb` completes the evaluator, Wasm, and native transition/cleanup matrix,
including exact multi-package dispatch, local reactor polling, repeated generations, Eligible drop,
reentrant destroy, typed failure, and non-LIFO roots. Focused verification is 41/41 and the full
native differential corpus passes; OpenSpec progress is 12/15 before formal gates.

Layer 4 conformance checkpoint `e46f5e6` makes transition authority production-consumed, adds
typed Wasm frame cleanup and cancelled-Wake final release, fixes evaluator parked-machine/stack
accounting, publishes canonical traces, and adds the missing destructive/order-sensitive evidence.
Focused verification and the full native differential corpus pass before the single final rerun.

Layer 4 completed at `81f55ac`. Its final hard-gate rerun passed focused verification 36/36,
typecheck 24/24, Biome over 989 files, test 28/28 (compiler 2,134/2,134 plus native differential
1/1), check 42/42 plus scripts 16/16, and release candidate 9/9. The run consumed its 3/3 repair
budget; the final root cause scoped Wasm frame-cleanup thunk generation to modules with
execution-package cleanup authority. No Critical or High finding remains open.

Layer 4's integrated full hard-gate command also exited successfully. Layer 5 may now begin from
the verified integration branch.

Layer 5 checkpoint `021ae09` connects ordinary-source Scheduler-shaped ownership, homogeneous
first activation, selective readiness, timer/cancellation, Coroutine drop, and stale-ready
disposal evidence. It also fixes recursive Wasm cleanup for nested Execution/Wake values, the
first of three bounded root-cause repairs. Focused pressure verification is 8/8; OpenSpec progress
is 4/21 while allocation, pay-for-use, diagnostic, native-corpus, report, and documentation gates
remain.

Layer 5 checkpoint `9cbb79e` adds allocation rollback and post-publication failure evidence, the
seven-case structural pay-for-use matrix, actor-neutral privilege inventory, unowned-entry
diagnostics, and designated native corpus cases. It fixes Wasm `EffectBorrow` frame-root planning
without selecting independent machinery for local Shared captures, consuming repair 2/3. Focused
pressure verification is 14/14 and compiler typecheck is green; OpenSpec progress is 10/21.

Layer 5's initial bounded run parked at `b7004e0` after consuming repair 3/3. The third repair fixes
native exact-package capability dispatch and passes the minimized mixed direct/external case, first
activation, compiler typecheck, and targeted Biome checks. OpenSpec progress is 12/21. The
designated native Coroutine and dormant-cancel cases then exposed a new distinct cancellation/
resumption lifetime defect after the budget was exhausted, so formal hard gates and conformance
did not start. A fresh bounded resume will begin from the preserved checkpoint and target that
named native lifetime blocker.

Layer 5 resume checkpoint `8679557` fixes native address-root synchronization after `WritePlace`
and normalizes intentional negative native exits through the POSIX low byte, consuming resume
repairs 1/3 and 2/3. The pressure suite is 14/14, repository typecheck and the full designated
native differential corpus pass, and focused Biome is green. OpenSpec remains 12/21 while the
remaining evidence, report, documentation, and formal gates continue.

Layer 5 resume checkpoint `9940961` completes the connected evidence, findings, and public
Execution lifecycle documentation. The pressure suite is 14/14, documentation policy/generation
and doctests are 54/54, and repository typecheck passes. OpenSpec progress is 20/21; only the
formal hard-gate task remains, with resume repair usage still at 2/3.

Layer 5 conformance checkpoint `681dc81` fixes the Critical recursive Wasm cleanup-authority alias,
separates depth-indexed package and frame locals, and closes evaluator guard/frame/body-transfer
cleanup ownership. A nested-union dormant cancellation probe now observes exact hooks, canonical
transitions, balanced evaluator releases, a safe late Wake, and direct-Wasm result `111`. Focused
dormant/timer evaluator-Wasm tests and compiler typecheck pass; the six consolidated High
corrections remain in the same single conformance fix pass.

Layer 5 conformance checkpoint `9932bdd` removes compiler canonical identities and spelling checks
for `OutOfMemoryError`, `Allocator`, and `SystemAllocator`. Sealed `Intrinsic.StorageFailure` now
carries primitive refusal authority, while ordinary Silk converts it to public allocation policy;
allocation provenance follows Effect catch/pattern flow without actor names. Compiler typecheck and
the focused ordinary/renamed local-Shared witness pass. Timer, waiter, emitted-artifact, and
cross-engine observability findings remain in the same fix pass.

Layer 5 conformance checkpoint `3dac3eb` completes the connected Timer/Scheduler cancellation
witness, actual Deferred/Fiber waiter-allocation failure, seven-configuration emitted Wasm/native
inventories, owner-side `NonParking`, expanded wrapper/provider renaming and phase inventory, and
cross-engine body/frame/guard/endpoint/late-Wake cleanup probes. It also fixes post-activation Wasm
body/frame authority. The full pressure suite is 14/14; report/task reconciliation and the single
post-conformance hard-gate rerun remain.

Layer 5 consolidated conformance fix is pushed at `639549c`. The pressure suite passes 14/14, the
sealed-storage ordinary-policy migration suite passes 147/147, and the designated native
differential corpus passes with four renamed-policy outcomes. The report records all seven verified
findings and their dispositions, with no second conformance review. The single formal
post-conformance hard-gate sequence is now running.

Layer 5's post-conformance run parked at `390c7fd` after the final 3/3 repair slot reconciled the
sealed-storage formatted/generated surfaces. All seven Critical/High conformance findings are
fixed; focused pressure 14/14, storage migration 147/147, native differential 1/1, and intrinsic
catalog 9/9 pass. The final full test attempt passed 2,145/2,146 assertions but the actor-neutrality
test exceeded its 60-second timeout under parallel-suite load despite passing focused in 35.21s.
This is a new distinct test-cost cause after budget exhaustion, so `pnpm check` and release
candidate did not run. A fresh bounded gate-only resume will target that named timeout without a
second conformance review.

Layer 5 gate-resume checkpoint `c7769fe` removes the parallel timeout without raising deadlines or
reducing evidence. Actor-neutral evidence is split into four existing-file tests, each snapshot,
MIR, renamed source, and artifact is reused once, and compiler spelling inventory is isolated as a
synchronous check. Focused pressure verification passes 18/18 and the exact compiler parallel
context passes 220 files/2,150 tests. Fresh gate-resume repair usage is 1/3; the full mandated gates
are running.

Layer 5 completed at `dd8e336`. Its fresh isolated gates passed in the required order: typecheck
24/24, Biome over 991 files, test 28/28 (compiler 220 files/2,150 tests plus native corpus 1/1),
check 42/42 plus scripts 16/16, and release candidate 9/9. OpenSpec is 21/21, all seven conformance
findings are fixed, no second conformance review ran, and the final worktree was clean and pushed.

The final five-layer integration merge is `9a46646`. The complete integration branch then passed
`pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and
`pnpm release:candidate` in order. SLP-0001 implementation is ready for the separate
`slp-6-audit-implementation` stage.
