# SLP-0001 implementation plan

SLP: `proposals/0001-independently-resumable-effect-executions/proposal.md`
SLP revision: 31
SLP digest: `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`
Integration branch: `julia/slp-0001-reaudit`
Started: 2026-08-23
State: Parked

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
| 2 | `add-independent-execution-packaging` | Parked | 3 fixes; fourth root cause stopped | `pnpm test`: finite Effect joins become `Blocked` instead of `Completed` | 7 reviewed; 5 fixed, 1 parked, 1 accepted scope boundary |
| 3 | `add-external-wake-parking` | Parked | — | Downstream of parked layer 2 | — |
| 4 | `add-independent-execution-engine-parity` | Parked | — | Downstream of parked layer 2 | — |
| 5 | `prove-independent-execution-separation` | Parked | — | Downstream of parked layer 2 | — |

## Integration gates

Layer 1 isolated gates passed: focused 77/77, typecheck 24/24, Biome 980 files, test 28/28
(compiler 2,090/2,090 plus native differential 1/1), check 42/42 plus scripts 16/16, and
release candidate 9/9. Its integrated full hard-gate command also exited successfully.

Layer 2 is preserved on branch `julia/slp0001-packaging` at `23e4550`. Its focused suite passed
47/47, but the fourth distinct hard-gate root cause caused ten finite Effect-join regressions after
2,090 compiler tests passed. The bounded gate budget was exhausted, so the change was not merged,
its conformance pass was not started, and downstream layers were parked.

Exact next action: start a fresh bounded implementation run for
`add-independent-execution-packaging`; trace represented-executable layout selection in the finite
Effect-join path, restore `Completed` outcomes without weakening execution-package planning, rerun
all hard gates, and then run the single three-lens conformance pass.
