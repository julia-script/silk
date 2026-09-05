# Exclusive storage and dependent cleanup workload

Recorded 2026-09-05 on Node 26.7.0, macOS arm64. The source workloads use the target-neutral
frontend; the residual workload selects x86_64 Linux without constructing MIR or running a backend.
The complete observations, including failed diagnostics, counters, timing and host memory, are in
[lifetimes-dependent-latest.json](lifetimes-dependent-latest.json).

Reproduce after building the compiler:

```sh
node packages/compiler/scripts/lifetime-benchmark.mjs --sizes=4,8,16 --families=exclusiveChains,dependentCleanup,exclusiveReplacements,dependentPartial
node packages/compiler/scripts/lifetime-benchmark.mjs --sizes=4,8,16 --residual-only
```

All 18 source samples matched their acceptance expectation. Each invalid exclusive-chain sample
produced one OWN0011; each invalid replacement sample produced one SEM0037. The generators retain
a copied shared descendant, recursive exclusive fields with conservative Drop, repeated generic
replacement, and independently conditional dependent fields.

| Work at sizes 4 / 8 / 16                        |   4 |   8 |  16 |
| ----------------------------------------------- | --: | --: | --: |
| Exclusive chain region constraints              |   6 |  10 |  18 |
| Exclusive chain propagated points / edge visits |  90 | 230 | 702 |
| Recursive cleanup recipe computations           |  13 |  25 |  49 |
| Replacement cleanup recipe computations         |   5 |   5 |   5 |
| Replacement cleanup recipe cache hits           |  13 |  25 |  49 |
| Replacement cleanup-liveness required points    |  21 |  41 |  81 |
| Partial dependent unique state nodes            |  19 |  35 |  67 |
| Dependent residual instances                    |   3 |   3 |   3 |
| Dependent residual ownership checks             |   0 |   0 |   0 |

Chain propagation grows with ancestor/descendant point relationships; it is not a combinations-of-
initialization-states search. The invalid parent write preserves the same edge-visit counts.
Recursive declared components and partial-state nodes grow linearly in these samples. Repeated
replacement reuses five cleanup recipes; each extra destination obligation adds required points
without new type regions. Endpoint lookup indexes source points once, then uses binary search per
cleanup exit. The two cleanup passes report their solver work separately as `cleanupLivenessSolver`
and `cleanupValiditySolver`, alongside original region solving, loan access, nominal variance,
contents summaries, comparison caches and cleanup derivation. A rejected ownership body skips
ordered cleanup validation; its absent work is not reported as a successful proof.

The residual dependent-owner family has 4, 8 and 16 distinct borrowed call sites but retains exactly
three instances (main, the generic owner and its Drop hook). All three ownership queries reuse
source proofs, with zero executed residual ownership work. Frontend and residual elapsed time stay
separate. These are opt-in observations, not timing or count assertions in the correctness suite.

ProjectAnalysis correctness tests separately verify private-body reuse, variance invalidation and
reuse after a hook-body edit. Adding a new Drop conformance changes the provider discovery catalog
and conservatively rechecks bodies; merely editing that hook's implementation checks only the hook.
Instances tests verify that exclusive holders with distinct lifetimes share layouts and instances.
