# Borrowed Effect outcomes and suspension growth

With a current compiler build, run:

```sh
node packages/compiler/scripts/lifetime-benchmark.mjs --effect-outcomes --sizes=2,4,8 --verify-codegen
```

The opt-in command measures composition depth, generic callback applications, selected-provider
forwarding, independent conditional fields across suspension, outer quantified binder width,
and incremental module fan-out. Each source family includes its matching invalid program.
The separate binder comparison microbenchmark and module-edit sequence isolate comparison and
query reuse without backend work. No timing thresholds enter the correctness suite.

[`lifetimes-outcomes-latest.json`](lifetimes-outcomes-latest.json) records 45 samples on Node
v26.7.0, Darwin arm64. Every source sample matched its expected diagnostic verdict. The 24
composition, callback, provider, and suspension samples additionally ran realized MIR and both
debug and release LLVM emission for `x86_64-unknown-linux-gnu`: all accepted programs verified
and emitted successfully, and both profiles rejected every invalid program. This checks emission
verdicts; runtime behavior is covered separately by the shared native acceptance corpus.

## Observed work

| Measurement                                          |         2 |         4 |          8 |
| ---------------------------------------------------- | --------: | --------: | ---------: |
| Composition: checked bodies / runtime instances      |     4 / 4 |     6 / 6 |    10 / 10 |
| Composition: lifetime constraints / edge visits      |    5 / 19 |    9 / 35 |    17 / 67 |
| Callbacks: checked bodies / runtime instances        |   933 / 5 |   933 / 7 |   933 / 11 |
| Callbacks: lifetime constraints / edge visits        | 673 / 864 | 679 / 946 | 691 / 1230 |
| Callbacks: residual ownership cache hits             |         1 |         3 |          7 |
| Providers: checked bodies / runtime instances        |   935 / 7 |   937 / 9 |   941 / 13 |
| Providers: lifetime constraints / edge visits        | 674 / 845 | 678 / 861 |  686 / 893 |
| Partial owner: retained flags / total frame slots    |     2 / 3 |     4 / 5 |      8 / 9 |
| Partial owner: cancellation releases                 |         1 |         1 |          1 |
| Partial owner: historical sparse state edges         |         6 |        20 |         72 |
| Binder width: rigid binders / comparisons            |     2 / 3 |     4 / 5 |      8 / 9 |
| Private body edit: semantic / ownership checks       |     1 / 1 |     1 / 1 |      1 / 1 |
| Alpha rename: semantic / ownership checks            |     0 / 0 |     0 / 0 |      0 / 0 |
| Additional generic call: semantic / ownership checks |     1 / 1 |     1 / 1 |      1 / 1 |
| Exported bound edit: semantic / ownership checks     |     3 / 3 |     5 / 5 |      9 / 9 |

Callback and provider totals include the ordinary-source standard-library closure. Its fixed
cost explains the high baseline. Adding callbacks does not check the generic bodies again;
it creates the necessary represented runtime instances. Instance discovery records zero new
residual body or ownership checks in these families: selected source proofs are reused, and
repeated ownership requests hit the existing cache. The report retains actual phase counters,
including request and source/cache reuse counts, rather than estimating work from source size.

Each conditional field contributes one retained boolean flag. The suspended record remains one
owner with one cancellation release restricted by its sparse initialization state. No product
of conditional states is materialized. Historical source snapshots still retain `n(n+1)` child
edges here, as in the earlier [partial ownership measurements](lifetimes.md); frame slot growth
does not establish a linear total-memory bound.

Callback edge propagation grows faster than the number of constraints because composed
environments remain live through later uses. These sizes do not prove an asymptotic bound or a
stable speedup. Timings include process warmup and shared-machine load; heap and maximum RSS
describe the process, not individual samples. Module private edits and alpha-renames still do
header and tooling work even when their checker counts are constant or zero.

`resolution.byInitiatorKind` and `resolutionExamples` retain actual observed name, path, member,
and conformance requests. The finite lifetime solver consumes selected semantic facts and has
no resolver callback. These observations cover the instrumented resolution surfaces, not every
possible lookup inside the compiler. Invalid source samples retain diagnostic codes and spans,
including source destruction, provider destruction, and reads of missing suspended fields.
