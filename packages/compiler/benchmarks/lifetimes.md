# Lifetime and partial ownership growth

Run `pnpm --filter @silklang/compiler lifetime:bench --sizes=4,8,16` to build and measure the
frontend. With a current compiler build, run
`node packages/compiler/scripts/lifetime-benchmark.mjs --sizes=4,8,16` from the repository root.
The opt-in script writes JSON to stdout and is absent from the default test pipeline. It uses
in-memory sources and executes no target backend, optimizer, native binary, or residual program.

Each family changes one named dimension: nested borrowed wrappers, nominal union width, live
loans, reborrow depth, recursive type/call component size, moved fields, nested projection depth,
independent conditional fields, callback count, Effect capture depth, and higher-ranked binder
width. Sparse arrays vary length exponentially while holding the two accessed indices fixed.
The module family separately measures cold and warm analysis, one private body edit, lifetime
binder spelling changes, an exported bound edit, and one additional generic call in an existing
module. Invalid loan and joined-owner inputs retain diagnostics and source spans; the exported
bound edit also measures diagnostics for each affected consumer.

[`lifetimes-latest.json`](lifetimes-latest.json) records one local run on Node v26.7.0, Darwin
arm64. Samples are single runs in one process, ordered by size: the first sample includes cold
initialization and later samples benefit from JIT warmup. Times are observations, not asymptotic
proofs or CI thresholds. The report records end-of-process heap usage and process maximum RSS;
neither is a per-family allocation measurement. The input byte length and all changing dimensions
are retained alongside every sample.

## Work attribution

`body-queries` counts actual semantic body executions, semantic reuse/rebinding, ownership
executions/reuse, dependency comparisons/cache hits, and resolved recursive call components.
These are current-revision counters, rather than module invalidation labels. Source syntax and
header rebinding still do work when body check counts are zero.

`retainedProofs` describes the proof artifacts present in a snapshot, including reused bodies.
It sums the actual `Lifetime.solve` region/constraint/point/edge work, final body
`TypeCompatibility` contexts, source ownership path/shape/projection/join operations, loans and
referents, and retained sparse state nodes and edges. It also reports the already derived nominal
variance/type-outlives summaries and cleanup recipe cache work. Anonymous callable preliminary
checking is not included in final-body comparison totals. The enclosing body's query owns that
work; ownership counts include its hidden functions separately.

Conditional paths count `Maybe` state requirements, not emitted MIR flags. Source analysis does
not execute residual ownership, suspension frame construction, or backend emission. Lifetime
solving and lifetime comparison accept finite semantic data/assumptions, with no name resolver or
candidate-discovery callback; this is architectural evidence only. The script does not report
unmeasured resolver invocations as zero. Candidate-resolution and residual/backend work require
separate instrumentation before making end-to-end work-attribution claims.

## Representative observations

At sizes 4, 8, and 16:

| Observation                                          |     4 |     8 |      16 |
| ---------------------------------------------------- | ----: | ----: | ------: |
| Private body edit: semantic / ownership checks       | 1 / 1 | 1 / 1 |   1 / 1 |
| Alpha rename: semantic / ownership checks            | 0 / 0 | 0 / 0 |   0 / 0 |
| Additional generic call: semantic / ownership checks | 1 / 1 | 1 / 1 |   1 / 1 |
| Exported bound edit: semantic / ownership checks     | 5 / 5 | 9 / 9 | 17 / 17 |
| Projection selector type computations                |     4 |     8 |      16 |
| Conditional root lattice joins                       |     4 |     8 |      16 |
| Sparse array length                                  |    16 |   256 |  65,536 |
| Sparse array maximum state nodes                     |     3 |     3 |       3 |
| Sparse array selector type computations              |     2 |     2 |       2 |
| Higher-ranked rigid binders                          |     4 |     8 |      16 |
| Higher-ranked comparisons, 32 repeated requests      |     5 |     9 |      17 |
| Retained moved-field snapshot edges                  |    10 |    36 |     136 |
| Retained conditional snapshot edges                  |    20 |    72 |     272 |

Cold module-fanout times were 7.97 / 8.83 / 15.02 ms; warm times were 4.73 / 4.15 / 4.95 ms.
Private-body edits took 6.79 / 7.60 / 12.13 ms and alpha renames 5.34 / 7.23 / 11.17 ms, despite
constant checker work, because closure/header observation and source-fact rebinding still scale
with the project. Recursive source components took 10.95 / 11.83 / 24.43 ms, with one actual call
SCC and variance visits 16 / 32 / 64. Sparse-array samples took 3.12 / 1.79 / 1.74 ms.

The first growth run exposed two avoidable costs. Prefix projection types previously restarted
from the root (10 / 36 / 136 selector computations); a per-root projection cache now computes each
prefix once. Joining identical incoming states previously rebuilt unchanged roots (20 / 72 / 272
lattice joins in the conditional family); identity reuse now performs only 4 / 8 / 16 joins and
shares unchanged child states. The recursive source family also found that layout-cycle analysis
incorrectly descended through references and slices; these address-bearing types now stop inline
layout traversal, while full semantic dependency reporting remains intact.

## Remaining costs and boundaries

Sparse state size follows touched paths, independent of fixed-array length. Historical snapshots
retain sorted child arrays for every distinct root state so diagnostics, inspection, and cleanup
lowering can consume exact before/after states. Moving `n` distinct fields retains `n(n+1)/2`
child edges; sequential conditional changes retain `n(n+1)`. Child state objects are shared, so
this is quadratic retained edge-array space, not eager aggregate/array expansion or exponential
variant combinations. A persistent child map would reduce storage of history at the cost of a
new representation shared by ownership inspection, MIR validation, and cleanup consumers. This
run documents that tradeoff; it does not establish a linear total-memory claim.

Flow joins still enumerate live binding keys, and canonical selector-path encoding/copying costs
depend on path depth. Linear selector computations and lattice-join counts do not claim those
supporting operations are linear. The recursive source sample contains mutually recursive
reference-bearing nominal types and functions; it does not synthesize every possible recursive
constraint graph.

Adding a new root module currently changes the conservative global resolution catalogue key,
which can recheck existing bodies even when the new module adds no impls. The additional-call
family changes an existing module and therefore isolates the accepted generic-body reuse claim.
Static/generic bodies remain implementation dependencies for their actual consumers; the cache
does not treat all generic bodies as interface-only. Accepted diagnostics and rebinding are
covered by focused `ProjectAnalysis`, `SemanticInvalidation`, and `InlineStructReach` tests;
performance dimensions and timing remain exclusively in this opt-in script.
