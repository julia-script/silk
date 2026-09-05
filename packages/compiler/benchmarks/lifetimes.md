# Lifetime and partial ownership growth

Run `pnpm --filter @silklang/compiler lifetime:bench --sizes=4,8,16` to build and measure the
frontend. With a current compiler build, run
`node packages/compiler/scripts/lifetime-benchmark.mjs --sizes=4,8,16` from the repository root.
The opt-in script writes JSON to stdout and is absent from the default test pipeline. Default
output retains every sample and exact per-initiator-kind counters, with one exact request example
per observed operation/initiator category. Add `--details` to retain every request record in stdout
for deeper attribution; the committed artifact uses the compact default. It uses
in-memory sources and executes no target backend, optimizer, native binary, or residual program.

Each family changes one named dimension: nested borrowed wrappers, nominal union width, live
loans, reborrow depth, recursive type/call component size, moved fields, nested projection depth,
independent conditional fields, callback count, Effect capture depth, higher-ranked binder
width, conformance candidate width, consecutive stored-reference resets, and loop-carried loans. Sparse arrays vary length exponentially while holding the two accessed indices fixed.
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
variance/type-outlives summaries, cleanup recipe cache work, lazy control-flow reachability work
and retained query sets, and activated storage obligations. Retirement comparison counters
distinguish actual requests from reused finite lifetime proofs. Anonymous callable preliminary
checking is not included in final-body comparison totals. The enclosing body's query owns that
work; ownership counts include its hidden functions separately.

Conditional paths count `Maybe` state requirements, not emitted MIR flags. Source analysis does
not execute residual ownership, suspension frame construction, or backend emission. Lifetime
solving and lifetime comparison accept finite semantic data/assumptions, with no name resolver or
candidate-discovery callback; this is architectural evidence only.

`resolution` separately counts actual reachable-index name, path, associated-member, and
conformance-discovery operations and the candidate loops of explicitly observed selected-call
provider queries. Each observation retains its initiating request and source span where available.
Tooling occurrence reconstruction is tagged separately: alpha-renamed bodies perform zero body
checks but still rebuild tooling occurrences through name/path lookups. Cold conformance samples
visit 20 / 40 / 80 declared rows across five actual conformance queries. These are observed rows,
not estimates from declaration count. Discarded intermediate declaration indexes and standalone
provider oracles without an observation context are outside this report; it makes no whole-compiler
resolver-total claim. Residual/backend work is not executed.

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

Cold module-fanout times were 7.52 / 8.35 / 14.11 ms; warm times were 3.29 / 4.57 / 6.60 ms.
Private-body edits took 6.59 / 7.04 / 12.68 ms and alpha renames 6.27 / 6.82 / 12.46 ms, despite
constant checker work, because closure/header observation and source-fact rebinding still scale
with the project. Recursive source components took 7.79 / 11.01 / 24.19 ms, with one actual call
SCC and variance visits 16 / 32 / 64. Sparse-array samples took 2.47 / 2.12 / 1.96 ms.
Stored-reference resets took 16.94 / 12.92 / 33.08 ms; invalid loop-carried loan samples took
5.57 / 5.46 / 10.03 ms and retained their expected source diagnostics.

The first growth run exposed two avoidable costs. Prefix projection types previously restarted
from the root (10 / 36 / 136 selector computations); a per-root projection cache now computes each
prefix once. Joining identical incoming states previously rebuilt unchanged roots (20 / 72 / 272
lattice joins in the conditional family); identity reuse now performs only 4 / 8 / 16 joins and
shares unchanged child states. The recursive source family also found that layout-cycle analysis
incorrectly descended through references and slices; these address-bearing types now stop inline
layout traversal, while full semantic dependency reporting remains intact.

The control-flow families exposed repeated traversal for reachability queries sharing a start
and the same re-creation barriers. Lazy reachable sets reduced visited edges for 16 resets from
1,001,633 to 66,236. They retain 492 requested rows containing 66,712 point entries; the compiler
does not precompute all point pairs. Body-local retirement proof reuse reduced 9,622 repeated
lifetime comparisons to 18 distinct queries (9,604 cache hits). In consecutive samples the
16-reset elapsed time fell from approximately 172 ms before these changes to 33 ms afterward;
those single-run timings support the measured work reduction, not a stable speedup ratio.

## Remaining costs and boundaries

Sparse state size follows touched paths, independent of fixed-array length. Historical snapshots
retain sorted child arrays for every distinct root state so diagnostics, inspection, and cleanup
lowering can consume exact before/after states. Moving `n` distinct fields retains `n(n+1)/2`
child edges; sequential conditional changes retain `n(n+1)`. Child state objects are shared, so
this is quadratic retained edge-array space, not eager aggregate/array expansion or exponential
variant combinations. A persistent child map would reduce storage of history at the cost of a
new representation shared by ownership inspection, MIR validation, and cleanup consumers. This
run documents that tradeoff; it does not establish a linear total-memory claim.

Reachability caches trade repeated traversal for retained sets. Query-specific barrier sets,
replacement-carrier comparisons, and constraint propagation still grow with source use points;
16 resets retain 66,712 reachable point entries, so this is not a linear total-space guarantee.
The report exposes this cost rather than hiding it in an unmeasured solver phase.

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

## Residual query growth

Run `node packages/compiler/scripts/lifetime-benchmark.mjs --residual-only --sizes=4,8,16`
with a current compiler build. The separate
[`lifetimes-residual-latest.json`](lifetimes-residual-latest.json) artifact records nine samples on
Node v26.7.0, Darwin arm64, selecting the `x86_64-unknown-linux-gnu` target. The existing default
mode and its artifact remain frontend-only. Residual mode analyzes each program once with
`Analysis.ofSource`, then times the residual work separately. It performs no MIR construction,
backend emission, optimizer execution, or native execution.

The ordinary-specialization family declares `n` distinct nominal owners and calls one generic
function with each. `Instances.discover` creates those runtime specializations plus the entry
function. The distinct-branch family calls one static-parameter function with `n` different
arguments selecting different source branches. Discovery also residualizes its entry function
because that function contains static calls. The repeated-query family uses one fixed source
program and submits the same static application `n` times to one `Residualization` coordinator
and one `ResidualOwnership` coordinator; this measures query cache reuse without instance-key
deduplication hiding repeated requests.

At sizes 4, 8, and 16:

| Residual observation                                |     4 |     8 |      16 |
| --------------------------------------------------- | ----: | ----: | ------: |
| Ordinary family: runtime instances                  |     5 |     9 |      17 |
| Ordinary family: body / ownership checks            | 0 / 0 | 0 / 0 |   0 / 0 |
| Ordinary family: ownership source-proof hits        |     2 |     2 |       2 |
| Ordinary family: ownership cache hits               |     3 |     7 |      15 |
| Distinct branches: body / ownership checks          | 5 / 5 | 9 / 9 | 17 / 17 |
| Distinct branches: executed loan-access checks      |     4 |     8 |      16 |
| Distinct branches: executed cleanup-plan queries    |    13 |    25 |      49 |
| Repeated application: body / ownership checks       | 1 / 1 | 1 / 1 |   1 / 1 |
| Repeated application: body / ownership cache hits   | 3 / 3 | 7 / 7 | 15 / 15 |
| Repeated application: executed loan-access checks   |     1 |     1 |       1 |
| Repeated application: executed cleanup-plan queries |     2 |     2 |       2 |

Every sample completed without source diagnostics, residual diagnostics, unavailable ownership,
or specialization failures. `residualBodies` records actual static selection executions and
cache/source-body reuse. `residualOwnership` independently records requests, exact source-proof
hits, checks, cache hits, and `executedWork`. Source/cache hits contribute zero executed work even
though the returned ownership proof retains its original work fields. Compact observations retain
the declaration, selection reason, and actual ownership branch; grouping observations changes no
counter totals. The distinct-branch artifact attributes its extra entry check to `StaticCall`
and the selected function checks to `StaticArguments`.

The ownership cache requires the same HIR function, semantic fact, declaration index, and ordered
local-shared boundary spans. Changed checker inputs are not inferred to be reusable from a
matching declaration name. These cold-snapshot samples exercise the source-proof handoff and
same-coordinator reuse; they do not measure cross-revision residual reuse, target changes, failed
static applications, or total compiler allocation. Failure caching and changed-input misses have
separate focused correctness assertions. Timing remains a single-process observation affected by
initialization and JIT warmup, with frontend and residual durations reported separately; it is not
a CI threshold or a claim that total compilation cost is constant.
