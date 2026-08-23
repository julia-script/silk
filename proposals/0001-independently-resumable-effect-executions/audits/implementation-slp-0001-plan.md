# SLP-0001 implementation plan

SLP: `proposals/0001-independently-resumable-effect-executions/proposal.md`
SLP revision: 31
SLP digest: `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`
Integration branch: `julia/slp-0001-reaudit`
Started: 2026-08-23
State: Running (resume 1)

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
| 4 | `add-independent-execution-engine-parity` | Running | — | — | — |
| 5 | `prove-independent-execution-separation` | Pending | — | Waiting for layer 4 | — |

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
