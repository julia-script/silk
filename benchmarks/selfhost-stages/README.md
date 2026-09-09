# Real self-hosted compiler: cold-build stages

This experiment uses the actual modules under `compiler/src`, not a smaller parser written to
approximate them. It measures what is added to native debug-build latency at each stage:

| Stage    | Entry point                         | Runtime work                                                                             |
| -------- | ----------------------------------- | ---------------------------------------------------------------------------------------- |
| `lexer`  | `Lex.silk`                          | Copy embedded source bytes, run the real lexer into its token vector, check token count  |
| `parser` | `Parse.silk`                        | Same lexing, then the real parser, AST count/span/child-ID checks, no printing           |
| `print`  | `Print.silk`                        | Same parsing and checks, then the real `SyntaxTree.write` with `StdoutWriter`            |
| `cli`    | Unmodified `compiler/src/main.silk` | Real argument/path/filesystem providers, file reading, lexing, parsing, and AST printing |

The input is a snapshot of `compiler/src/main.silk` itself. The first three stages embed exactly
those UTF-8 bytes; the fourth reads the same bytes from a file. There is no stdin stage. Runtime
execution and output capture happen **after** compilation timing stops.

## Initial ablation — 2026-09-08

Apple M1 Max, macOS 15.5, Node 26.7.0, Homebrew LLVM/Clang 22.1.8. The compiler includes the
existing uncommitted performance repairs; revision, compiler diff hash, source hashes, and built
toolchain identity are recorded in the [raw results](results/2026-09-08-m1-max.json).

Three successful cold builds per stage, with successful runtime checks for all 12 executables:

| Stage                        | Median |        Range | Emitted symbols | Median peak RSS |
| ---------------------------- | -----: | -----------: | --------------: | --------------: |
| Lexer only                   |  8.01s |   7.25–8.20s |             122 |         715 MiB |
| Add real parser, no printing | 33.58s | 31.46–34.78s |             469 |       2,817 MiB |
| Add AST printing             | 37.19s | 36.28–41.18s |             553 |       2,965 MiB |
| Full CLI/file input          | 44.18s | 39.97–45.61s |             720 |       2,998 MiB |

The largest adjacent median increase is adding the actual parser: about **25.6 seconds**.
Printing adds a net 3.6 seconds; the production CLI adds a further net 7.0 seconds. Approximately
three quarters of the full build's median time, and nearly all of its peak memory, are already
present in the parser-only entry point. These are entry-point contrasts, subject to the probe and
transitive-import caveats below, not independently additive timings of individual APIs.

Selected phase medians explain where the measured increase appears:

| Phase                  | Lexer | Parser | Printing | Full CLI |
| ---------------------- | ----: | -----: | -------: | -------: |
| Elaboration            | 1.05s |  1.74s |    1.79s |    3.27s |
| MIR lowering           | 0.12s |  2.59s |    3.48s |    5.45s |
| Backend                | 0.47s | 17.39s |   17.90s |   19.43s |
| Native object emission | 0.07s |  2.60s |    2.43s |    2.36s |
| Linking                | 0.04s |  0.07s |    0.07s |    0.06s |

The backend increase of roughly 16.9 seconds is the largest named-phase change from lexer to
parser. The backend here is the compiler's reported backend phase, distinct from the later
native object-emission phase. This locates a useful next profiling target; it does not yet
distinguish excessive generated code from expensive per-instruction construction/validation or
garbage collection. The parser-only stage is now a direct, smaller reproduction of most of the
full compiler's slow build, unlike the earlier toy AST benchmark.

All stages use the same 1,027-token input; parser stages produce 452 nodes with zero diagnostics.
All six printing executables produce identical AST bytes. The full CLI output also passes the
existing bootstrap differential checker. The first setup attempt successfully built the lexer
but rejected an invalid borrow expression in the probe's tree checker; that incomplete attempt
was corrected before this batch and is not a performance sample in these results.

This remains a shared-host measurement. One-minute load averages ranged from about 8.3 to 19.0
at sample boundaries and generally fell near the end; the ranges are retained, with no samples
dropped. No production compiler or standard-library files were edited for this experiment.

## Run

```sh
pnpm build
python3 benchmarks/selfhost-stages/run.py --samples 3 --output /tmp/selfhost-stages.json
```

Requires macOS, Python 3 (standard library only), Node, and the built workspace's native toolchain.
The output path must not already exist. Use `--stages lexer parser` to run a subset. Builds are
serial; stage order rotates each round. Three samples are the default; use four or a multiple of
four to balance every stage's order position in a complete four-stage experiment.

The harness creates a temporary source snapshot below `benchmarks/`, copies the real compiler
modules unchanged, and adds the probe entry points plus a generated `bench.Input` module. Setup,
source embedding, and correctness-oracle preparation are outside timing. Every sample starts a
fresh Node process with fresh outputs and disabled persistent native/Node compile caches. The OS
filesystem cache is not flushed. No user cache is erased. The owned temporary directory is removed
on success and failure; failed measurements do not publish a result JSON.

Build time includes process startup, toolchain validation, frontend, backend, object emission,
runtime support, linking, and `--timings` reporting. Raw commands, stdout/stderr, individual phases,
symbol counts, peak RSS, load averages, source hashes, and toolchain identity are saved. Repeated
occurrences of a phase are summed within a sample before calculating its median. Phase medians
need not sum to total-wall-time median: medians are computed independently and some work is not
covered by named phases. RSS is macOS's per-command maximum, not a sum of all process-tree heaps.

## Correctness and interpretation

The bootstrap compiler supplies expected token and AST-node counts for the input. The embedded
lexer checks its token count. The parser probes also check node count, root identity/kind/span,
zero diagnostics, node span bounds, child-before-parent IDs, and token-index bounds. These checks
keep their results observable without importing an output provider into the non-printing entries.

Every output binary must execute successfully. Non-printing stages must produce no output. The
printing and CLI stages must produce byte-identical AST output across every build, with the
expected node count and zero diagnostics. The first CLI binary also runs through the existing
`compiler/scripts/test-parser.mjs` differential checker against the bootstrap AST: full significant
tree shape, postorder IDs, ordered spans, token ownership, and absence of orphan nodes. This is a
single valid-input benchmark, not another complete parser correctness suite or recovery sweep.

These are **entry-point ablations**, not isolated library microbenchmarks:

- The grammar, node/element representation, and standard-library implementations stay unchanged.
- `SyntaxTree` and lexer token modules already import formatting/writer modules. Not calling their
  printing functions removes reachable output code from native emission, but does not necessarily
  remove those modules from frontend loading and checking. Inspect both phase and symbol changes.
- The first three probes add small correctness checks and an embedded-source copy. The CLI stage
  uses the exact production entry point, without those probe checks, and allocates source bytes
  through file reading instead. Its difference from `print` is the net entry-point change, not a
  mathematically isolated price for one filesystem operation.
- Function bodies differ greatly in size and lowering complexity. Emitted symbol counts are
  descriptive; they are not normalized units of compiler work.
- Shared-host contention can change between stages. Retain all samples and their ranges; do not
  turn a single adjacent-stage difference into an exact causal percentage.

No production compiler or standard-library optimization was part of the initial ablation above.

## Cold-profile fixes — 2026-09-08, before the runtime merge

The follow-up profiles the actual parser-only entry point and makes three small backend-related
repairs, without changing Silk source, parser representation, language semantics, or persistent
cache policy:

- `NativeAggregate.failurePayload` prepares each member's repacking plan once instead of once
  per destination lane. If no mapped failure carries a lane, it emits its zero value directly
  instead of comparisons and selections between identical zeroes. This matters when a wide
  success value shares an outcome carrier with a zero-field error such as `OutOfMemoryError`.
- `Type.runtimeKey` memoizes immutable object-shaped types in a `WeakMap`, like `Type.key`
  already did. Recursive layout/instance queries reuse the canonical string within the current
  process; the map neither persists builds nor keeps completed type objects alive.
- Effect joins exclude their explicitly reloaded result from the general `NativeStorage`
  reload pass. Previously that pass loaded every result lane, then the join immediately loaded
  the same lanes again and discarded the first copy. Other roots, alias handling, and suspension
  inputs are unchanged; this is not a new liveness analysis.

The parser-only IR changed from **1,115,863 to 1,004,011 instructions**: 36,265 redundant
comparisons, 36,265 selections, and 39,322 duplicate result loads were removed. Textual IR shrank
from 88,183,810 to 77,818,523 bytes. Function count stayed at 469; loads fell from 436,272 to
396,950. These are measurements of
the debug IR, not assertions in the correctness suite. The large remaining load population is
a separate optimization target; these repairs do not solve its aliasing/liveness questions.

### Repeated cold builds

Three fresh-process debug builds per stage, with separate before, intermediate two-fix, and
final three-fix batches, using the unchanged harness and matching input/source hashes. Final
comparison:

| Stage       | Before median (range) |  After median (range) |
| ----------- | --------------------: | --------------------: |
| Parser only | 36.99s (35.91–40.34s) | 34.69s (33.86–35.12s) |
| Full CLI    | 48.79s (47.89–54.28s) | 43.76s (42.08–46.92s) |

Raw commands, compiler-diff identities, phases, resource usage, and all samples are retained in
[before](results/2026-09-08-profile-before.json) and
[final](results/2026-09-08-profile-final.json). The
[intermediate two-fix batch](results/2026-09-08-profile-after.json) is also retained: its medians
were 35.15s and 47.21s, with ranges overlapping the baseline. No samples were dropped. Native and Node compile
caches were disabled; OS caches were not flushed. The compiler changes were temporarily removed
for the before batch and restored and rebuilt for the after batch; earlier performance repairs
were retained in both.

Backend phase medians fell from 19.55s to 17.18s for parser-only and 22.47s to 18.62s for the CLI.
Median process CPU time (`user + sys` from `/usr/bin/time -l`) fell from 46.02s to 43.39s and from
59.45s to 55.14s respectively. CPU totals can exceed elapsed time because compilation uses
multiple threads. Median peak RSS was 2,771 → 2,483 MiB for parser-only and 2,778 → 2,622 MiB for
the CLI.

**Interpretation: modest progress, not a solved cold-build problem.** The wall-time medians are
about 6% and 10% lower. This is a shared host with other builds running:
one-minute load averages ranged from 10.1 to 18.1 before and 8.6 to 12.1 in the final batch. Batches were not
interleaved A/B trials. Do not attribute every timing difference to the patch or promise a precise
speedup from these samples.

### Profile evidence and correctness

Sequential diagnostic CPU profiles of parser-only gave 41.33s before, 38.36s after the payload
repair, and 33.55s after runtime-key memoization. Their backend phases were 22.40s, 19.86s, and
16.66s; sampled GC time was 8.44s, 7.65s, and 6.73s. These individual instrumented observations
motivated the fixes, but **are not the repeated unprofiled speedup** reported above. GC is already
included in elapsed/phase time, not an extra cost to add to it.

All 18 measured executables passed their runtime checks. Every full-CLI AST output was identical
across all three revisions; all batches passed the bootstrap differential check. A retained final
executable also passed all 102 parser/stdlib source fixtures, including recovery and flat-tree,
span, and token-ownership invariants. Adding runtime-key memoization alone produced a
byte-identical parser executable; removing duplicate result reloads alone produced a byte-identical
full-CLI executable. Regression tests cover zero-only failure payload selections, duplicate result
reloads at the same join, and repeated/nested runtime-key queries; they do not assert wall time
or instruction counts.

To repeat the measured after batch after building the workspace:

```sh
python3 benchmarks/selfhost-stages/run.py --samples 3 --stages parser cli --output /tmp/selfhost-after.json
```

For a fresh CPU profile of the production CLI compilation, create a new output directory first:

```sh
mkdir /tmp/silk-cli-profile
env -u NODE_OPTIONS -u NODE_COMPILE_CACHE -u NODE_V8_COVERAGE -u SILK_NATIVE_CACHE_DIR \
  NODE_DISABLE_COMPILE_CACHE=1 node --cpu-prof --cpu-prof-interval=2000 \
  --cpu-prof-dir=/tmp/silk-cli-profile packages/cli/dist/bin.js build-exe \
  compiler/src/main.silk --source-root compiler/src --optimization debug \
  --output /tmp/silk-cli-profile/compiler --timings
```

## Post-runtime-merge baseline — 2026-09-08

These are the current measurements after integrating PR #388 (`origin/main` at `a67354bc`)
into the local parser branch. The merge commit is `815e0986`; the uncommitted compiler changes
are identified by `compilerDiffSha256` in the [raw results](results/2026-09-08-integrated.json).
The performance fixes above, and the earlier discovery, field-lookup, LLVM handle, and
block-local reload fixes, remain in this build.

| Stage       | Cold median |        Range | Backend median | Object median | Symbols | Median peak RSS |
| ----------- | ----------: | -----------: | -------------: | ------------: | ------: | --------------: |
| Parser only |      60.54s | 59.89–70.00s |         36.47s |         6.74s |     642 |       3,768 MiB |
| Full CLI    |      84.60s | 81.71–85.45s |         50.09s |         8.69s |     878 |       4,272 MiB |

There are three fresh-process samples per stage, all retained and runtime-checked. Median
process CPU time (`user + sys`) is 83.49s for parser-only and 115.67s for the CLI. One-minute
host load averages range from 5.0 to 11.1. Our workspace builds and test suites did not overlap
this batch; other host work was not stopped. Cache policy and timing boundaries are unchanged.

The previous 34.69s / 43.76s medians are **not** measurements of this runtime. Hosted startup
now compiles from Silk source and includes process-input capture, diagnostic observation, and
execution-storage support. It captures inputs even when the application's entry does not
request HostInput. Parser-only therefore still includes that startup code, despite its own
entry doing no input reading or output printing. This is not stdin blocking: native execution
is outside the compilation timer, and no probe reads stdin.

The application entry also needed migration: `main` now requests mutable HostInput from
hosted startup instead of constructing `OsHostInput.make()` without a snapshot. Consequently
the shared benchmark input, still the exact current `compiler/src/main.silk`, is now 995 tokens
and 437 AST nodes rather than 1,027 / 452. These are new-revision baselines, not a controlled
one-variable measurement of the merge. Symbols describe the larger reachable program;
they are not a normalized measure of compiler work.

### Integration correctness

The new startup exposed two related nominal-union catch bugs:

- Synthetic catch matches used one coverage identity per error type, but layout expands nominal
  union errors into their variants. Catch lowering now uses layout coverage and computes each
  handler arm's coverage transition and decision candidates from it.
- A nominal variant tag can occupy a widened or floating-point structural-union payload carrier.
  Native match branching now recovers the i32 tag before comparing it.

A small mixed-error-row regression reproduced both failures in sequence. It now verifies MIR
and LLVM emission for both catch-all and selective recovery. All 20 selective-catch tests pass;
the integration also passed 140 focused compiler tests, 12 LLVM function-body tests, workspace
typechecking, and the workspace build. A retained final CLI passed all 113 parser/stdlib source
fixtures, including recovery. All six measured programs passed, and the CLI AST output was
byte-identical across this batch and passed the bootstrap differential checker.

Release-candidate validation passed all 20 checks. Full-suite validation was still running at
handoff; no complete-suite pass is claimed. Root formatting, linting, and `pnpm check` remain
blocked by pre-existing files/configuration under the unrelated `.claude/worktrees` directory.
Changed TypeScript files and this report pass their scoped formatting/lint checks.

The first incomplete measurement attempt built parser-only in 73.13s, then rejected the obsolete
HostInput constructor in the CLI. It produced no result file and is not part of the successful
batch above. Subsequent failed diagnostic builds isolated the catch bugs before remeasurement.

### Remaining profile target

A successful instrumented full-CLI build took 108.29s, with 58.79s in backend emission, 10.45s
in native object emission, and approximately 28.69s attributed to garbage collection by the
CPU sampler. That diagnostic run overlapped the final workspace build initially; it is not an
unprofiled benchmark sample. GC is included in the elapsed phases, not an additional cost.
The profile also samples LLVM handle/instruction construction, bitstream writing, and canonical
key construction. It supports investigating generated-code volume and allocation pressure next,
including an ablation of default startup versus application code; it does not establish a new
optimization's speedup.

## Shared storage and completion cleanup (2026-09-08)

The next pass targets emitted work, using the same post-merge source, hosted runtime,
995-token / 437-node input, toolchain, cache policy, and three-sample harness.
[Raw results](results/2026-09-08-shared-storage-completion.json) retain every sample,
phase timing, source hash, and compiler-diff identity.

| Stage       | Previous cold median | New cold median |    New range | Reduction | New median peak RSS |
| ----------- | -------------------: | --------------: | -----------: | --------: | ------------------: |
| Parser only |               60.54s |          34.93s | 33.90–35.49s |     42.3% |           2,737 MiB |
| Full CLI    |               84.60s |          45.80s | 42.37–46.25s |     45.9% |           2,681 MiB |

Parser backend time falls from 36.47s to 16.23s; CLI backend time falls from 50.09s
to 17.76s. Native object emission falls from 6.74s to 2.32s and from 8.69s to 4.57s,
respectively. The reachable program is unchanged: 642 parser symbols and 878 CLI symbols.
All six executables pass their runtime checks; AST output passes the bootstrap differential
checker. Our workspace builds and tests did not overlap this batch. One-minute host load
averages at sample start range from 8.1 to 13.2; unrelated host activity was not stopped.
These are fresh-process/output builds, not measurements with the OS page cache flushed.

### What changed

- **One backing store for addressable locals.** Mutable lane pointers now project the same
  bytes used by borrowed and pointer access. Previously, every possible alias write caused
  addressable lanes to be reloaded and copied into a separate mutable allocation. Shared
  storage removes those mirrors and lets reloads ignore locals absent from the current
  block: later blocks read the canonical bytes on entry. Suspension remaps the lane pointers
  when it selects persistent frame storage. The foreign-call regression checks that a later
  return actually consumes the load from that storage.
- **One diagnostic cleanup epilogue per completed invocation.** Previously, every normal
  return emitted releases for every diagnostic outcome in the function. Return paths now
  transfer the selected result, join its payload and metadata through phi nodes, and release
  remaining outcomes once in shared code. Parking and trap exits remain separate; this does
  not remove diagnostic observation or alter ownership transfer.

A temporary read-only LLVM snapshot census found 2,911,244 instructions in the original
full CLI, 1,692,034 after shared storage, and 1,025,734 after both changes: approximately
65% fewer instructions. `Token.write` alone shrinks from 184,347 instructions / 26,203
blocks to 12,093 instructions / 1,725 blocks. These are opt-in diagnostic observations,
not correctness-test thresholds. The census hook lives outside the repository and is not
enabled in the cold-build samples. Its instrumented phase timings are not used in the table.

### Validation and remaining work

Workspace typechecking and build pass, as do all 20 release-candidate checks and 19 script
tests. The retained optimized CLI passes all 113 parser
and standard-library fixtures, including recovery, and prints byte-identical output to the
pre-optimization CLI for `compiler/src/main.silk`. Structural regressions cover canonical
backing storage, post-foreign-call reads, and return payloads joining before diagnostic cleanup.

All 2,437 compiler tests pass across 210 files, as do all 53 executable documentation examples.
Native shards 2/3 and 3/3 each pass 111 tests; shard 1/3 subsequently passed 112 tests.
The workspace run finished successfully with all 22 tasks passing. The workspace invocation
is `SILK_NATIVE_SHARD=1/3 pnpm test --env-mode=loose
--cache=local:r`; loose environment mode passes the shard selection, while read-only caching
prevents a partial native sweep from being stored as a complete-suite cache entry. The other
two shards run through `pnpm --filter @silklang/compiler test:native-acceptance` with their
respective `SILK_NATIVE_SHARD` values.

Root formatting, linting, and `pnpm check` remain blocked by the unrelated
`.claude/worktrees` files/configurations described above. The changed TypeScript files pass
scoped formatting and linting. The earlier pre-optimization native run was interrupted to
make room for clean measurements and validation of this version; it is not counted as a pass.
The first release-candidate attempt overlapped a rebuild and failed four packaging checks
with missing generated modules. Its clean rerun passed; the failed attempt is not treated as
a compiler regression or a successful validation.

The CLI still spends about 17.76s in backend emission and 7.07s in MIR lowering. These remain
profiling targets; this pass does not establish parity with Rust or Zig, and compilation is
still much slower than the eventual target.

A follow-up CPU profile still samples 14.64s in garbage collection across the complete
instrumented process, 2.44s in canonical-record construction, and substantial LLVM bitcode
writing and MIR traversal. This profile overlapped validation workloads and is not a cold
benchmark sample; its 28.22s backend phase must not replace the 17.76s unprofiled median.
It points toward further allocation and traversal work without establishing its likely gain.

## Block-local reference intervals (2026-09-08)

This pass narrows the reload set within each linear block. Existing inputs are active at
block entry. Locals first mentioned only as destinations become active at their definitions;
a destination also appearing as an operand remains an input. Locals leave the set after
their final reference. Operation-internal joins still retain the current operation's roots,
and hidden suspension-frame payloads stay pinned. Cleanup flags and selector inputs remain
references. This is conservative local interval tracking, not global liveness analysis or
dead-store elimination; canonical storage still carries values between blocks.

The [raw results](results/2026-09-08-reference-intervals.json) use the same source hashes,
995-token / 437-node input, hosted runtime, toolchain versions, and fresh-process/output
policy as the previous batch. There are three successful samples per stage, all retained.
No workspace builds or tests overlap the measurements; the OS page cache is not flushed.

| Stage       | Previous cold median | New cold median |    New range | Previous median peak RSS | New median peak RSS |
| ----------- | -------------------: | --------------: | -----------: | -----------------------: | ------------------: |
| Parser only |               34.93s |          33.69s | 33.20–34.22s |                2,737 MiB |           2,309 MiB |
| Full CLI    |               45.80s |          44.69s | 44.56–46.54s |                2,681 MiB |           2,434 MiB |

The observed wall medians improve by only 3.5% and 2.4%. The timing ranges overlap the
previous batch, so this does **not** establish a large or precise speedup. Load averages at
sample start are much higher in this batch (13.3–82.8 versus 8.1–13.2); these snapshots
include recent activity, not just work concurrent with each compilation. Median process
CPU time (`user + sys`) falls from 44.98s to 42.47s for the parser and from 58.06s to 54.85s
for the CLI. Peak memory improves more clearly: median reductions of 15.7% and 9.2%.

Parser backend median falls from 16.23s to 12.73s. CLI backend median changes from 17.76s
to 17.22s, and CLI object emission from 4.57s to 2.79s. Other phase times vary too; phase
median differences are not independently additive. The reachable program remains 642 / 878
symbols, and all six binaries pass the runtime oracle and bootstrap differential check.

### Structural evidence and verification

The read-only LLVM census identified 320,044 unused mutable/address-root loads before this
pass. Retiring last references alone removes 88,612 loads; delaying activation of new
definitions removes another 134,258. Together they remove 222,870 unused loads, taking the
full CLI from 1,025,734 to 802,864 instructions (21.7% fewer). The remaining 100,437 unused
loads include other lowering categories. This census is an opt-in diagnostic observation,
not a correctness threshold, and its temporary hook is disabled during cold measurements.

The regression first failed on a refresh after the last reference, then failed separately
on a load immediately overwritten by a local's initial value. Both assertions now pass.
All 45 focused backend/selective-catch tests pass. The optimized executable passes all 113
parser/stdlib fixtures, including recovery, and produces byte-identical AST output to the
previous executable for `compiler/src/main.silk`.

Workspace typechecking and build, test typechecking, scoped formatting/linting, all 20
release-candidate checks, and all 19 script tests pass. `pnpm check` and root formatting
still stop at `.claude/worktrees/agent-ac09fe0ffa27c76a5/packages/compiler/src/LowerExpression.ts`;
root lint still rejects a nested worktree's `options.denyWarnings` configuration. These
pre-existing worktree issues are unchanged. A fresh broader compiler run, including native
shard 1/3, is running via `SILK_NATIVE_SHARD=1/3 pnpm test --filter=@silklang/compiler
--env-mode=loose --cache=local:r`; no full-suite pass is claimed for this pass yet.

A fresh CPU profile of the current CLI build, without overlapping our builds or tests,
takes 47.88s with profiling enabled. It samples 6.47s in GC across the whole process,
1.58s in canonical-record construction, 1.03s in MIR local-reference scanning, and 1.02s
in HIR expression-tree traversal. These are investigation targets, not additive predicted
speedups. Type and instance keys already have identity caches; simply proposing another
cache would not explain the remaining work. Repeated traversal and allocation are the
next useful targets after this modest wall-time improvement.

## Typed aggregate places — 2026-09-08

The implementation now keeps aggregates in canonical typed storage, with selected field
reads and explicit boundary conversion. Primitive scalars and bounded descriptors stay
direct. Outcome/composite storage planning, alias writes, joins, captures, cleanup, and
suspension all use that model; the aggregate lane-cache path is removed. Calling conventions
are unchanged. Proven undersized private frame reservations for borrowed descriptors and
concrete callable environments were corrected in planning.

### Cold measurements

Three baseline samples and two consecutive after batches of three samples per stage are
retained. All builds ran in fresh processes without persistent compiler caches, with no
overlapping builds/tests from this work. OS caches were not flushed. Source inputs, Node
26.7.0, and Clang 22.1.8 match; the compiler source and built-module fingerprints were stable
across both after batches, including the new untracked actors. The generated toolchain
integrity digest changes with compiler implementation, as expected.

| Stage       | Before median (range) | After median (range), six samples | Process CPU median |   Peak RSS median |
| ----------- | --------------------: | --------------------------------: | -----------------: | ----------------: |
| Parser only |  35.49s (34.47–41.04) |              32.61s (31.51–34.03) |     43.09 → 42.81s | 2,241 → 2,583 MiB |
| Full CLI    |  46.32s (45.51–47.05) |              43.26s (42.91–47.32) |     56.26 → 56.00s | 2,271 → 2,586 MiB |

**This does not establish a causal cold-build speedup.** Host load at sample boundaries fell
from 12.4–17.7 before to 4.3–9.7 afterward. Unchanged frontend phases also became faster,
while process CPU stayed effectively flat. Backend phase medians increased from 13.86 to
15.52s for the parser and 17.41 to 17.96s for the CLI. Peak RSS increased about 15% and 14%.
The first after batch's medians were 31.71/42.97s; the confirmation batch's were 33.59/44.09s.
No outliers or unsuccessful-looking samples were removed.

Raw evidence: [baseline](results/2026-09-08-typed-places-before.json),
[first after batch](results/2026-09-08-typed-places-after.json),
[confirmation batch](results/2026-09-08-typed-places-after-repeat.json), and
[combined summary and fingerprints](results/2026-09-08-typed-places-summary.json).
All twelve after-batch runtime oracles and both bootstrap AST differential checks passed.
The separate [scalar/descriptor controls](../cold-compilation/results/2026-09-08-typed-places-controls.json)
passed all six runtime oracles, with medians of 6.68s and 6.44s. Their hosted startup is
included; there is no matched pre-change descriptor baseline or new Rust/Zig comparison.

### What changed structurally

The [opt-in census](results/2026-09-08-typed-places-census.json) reconciles against the
immutable LLVM module and is not a timing run:

| CLI measurement         |  Before |   After |
| ----------------------- | ------: | ------: |
| LLVM instructions       | 802,864 | 800,774 |
| Loads                   | 246,523 | 133,739 |
| Stores                  | 204,060 | 145,744 |
| Unused loads            | 100,437 |     760 |
| Allocations (`alloca`)  |  78,431 |  12,594 |
| Join/alias reload lanes | 166,725 |   1,939 |

Aggregate refresh traffic is gone; remaining refreshes are bounded direct values, at most
two lanes in this workload. But total instruction count falls only 0.3%. Field addresses,
copies, and active-union boundary conversion replace much of the removed scalar traffic:
GEPs increase from 71,403 to 225,777, phis from 12,294 to 55,724, and 13,143 switches are
introduced. The existing flattened calling convention still requires wide argument/result
materialization. Reducing that expansion is a separate calling-convention change, not a
reason to reintroduce aggregate caches or claim this patch has solved backend latency.

A separate [current CPU profile summary](results/2026-09-08-typed-places-profile.json) records
6.64s of whole-process GC samples and 8.08s of exclusive samples in the LLVM package.
NativePlace/ValueStorage together account for 0.68s of exclusive samples. These are
diagnostic whole-profile observations, not additive predictions, matched before/after
profile deltas, or named-phase timings. They do not identify a single new lookup as the
main bottleneck. The profiled binary passed the real-input bootstrap AST oracle.

### Validation

All 2,442 compiler tests, all three native acceptance shards (112/111/111 tests), and the
remaining workspace packages passed. Debug and optimized real CLI builds each passed the
113-file parser/stdlib corpus; debug and optimized Wasm lifecycle probes passed. Typechecking,
lint, all 20 release-candidate tests, and all 19 repository-script tests passed.

Coverage was completed in parts: after the successful compiler-suite rerun, the root runner's
redundant native sweep was interrupted because every native shard had already passed.
Remaining packages passed via `pnpm test --filter='!@silklang/compiler'`. A successful
monolithic `pnpm test` exit is not claimed. `pnpm check` and root formatting still stop at
the pre-existing `.zuse/settings.toml` formatting issue, which was left untouched. Detailed
commands and integration repairs are in the
[implementation record](../../openspec/changes/archive/2026-09-08-lower-aggregates-through-typed-places/implementation-notes.md).

## Snapshot facts and lookup indexes — 2026-09-08

Four structural changes remove repeated searches without changing the parser, language, or ABI:

- Derive diagnostic-observer presence once per immutable MIR module, including negative results.
- Share HIR function indexes across resolution stages; narrow native candidates by canonical
  declaration, then apply the unchanged generic/static instance matcher.
- Build one lazy local-use index per constructor-folded function, preserving ordered attribution
  to operations, outcomes, cleanup, and nested executions.
- Index active LLVM globals once, then emit in variable/function/alias actor order with original
  global indices. Deleted, replaced, and category-rebound globals retain their existing semantics.

Weak ownership ties the JavaScript caches to immutable compilation snapshots, not persistent
build caches. Transformations publish new snapshots and therefore derive new facts/indexes.

### Real parser workload: no established total-build speedup

The [new raw results](results/2026-09-08-small-wins.json) contain three cold samples per stage.
The comparison uses the six typed-place after-samples above. Source hashes, embedded input,
CLI AST bytes, toolchain versions, and cache policy match; all six new runtime oracles and the
bootstrap differential pass. No builds/tests or instrumentation overlapped these timing runs.

| Stage       | Previous wall median | New wall median (range) | Previous CPU median | New CPU median |
| ----------- | -------------------: | ----------------------: | ------------------: | -------------: |
| Parser only |               32.61s |    33.03s (31.66–33.99) |              42.81s |         43.12s |
| Full CLI    |               43.26s |    43.72s (43.22–50.57) |              56.00s |         55.16s |

**The earlier estimate of 2–3 seconds saved on this workload is not established.** The total
wall medians are effectively flat within observed variation. MIR lowering improves from
3.04s to 2.24s for the parser and 6.97s to 6.22s for the CLI, but other phases offset that gain.
Phase differences are not independently additive. The 50.57s sample is retained, not discarded.
One-minute load averages at sample start ranged from 7.3 to 13.7, versus 4.3 to 9.7 in the
earlier batch. This is a shared-host, non-interleaved comparison, not a controlled causal estimate.

### Function-count scaling: substantially better, still not fully linear

The diagnostic sweep uses constant-sized scalar functions in a balanced call tree: N leaf
functions, N−1 joining functions, and a checksum-checking main. N is 0, 16, 64, 256, or 1,024;
the zero case is just a main returning zero. Leaves return their argument plus their ordinal;
joins add their two children, and main checks the result at argument 1 against N(N+1)/2.
Every function stays reachable. Aggregate width, effect density, and stdin are not varied.

Both the [before](../cold-compilation/results/2026-09-08-scaling-before-small-wins.json) and
[after](../cold-compilation/results/2026-09-08-scaling-after-small-wins.json) data retain three
serial fresh-process builds at each size, rotating size order. Fixture content hashes match.
Compiler source/dist fingerprints stayed fixed within each batch. All 30 runtime checks pass.
OS caches were retained; native and Node persistent compilation caches were disabled.

| Added functions | Before cold median | After cold median | Before CPU median | After CPU median |
| --------------- | -----------------: | ----------------: | ----------------: | ---------------: |
| 0               |              7.20s |             6.73s |            10.10s |            9.94s |
| 31              |              6.92s |             6.64s |            10.08s |            9.84s |
| 127             |              7.53s |             6.90s |            10.76s |           10.11s |
| 511             |              8.96s |             8.04s |            12.78s |           11.90s |
| 2,047           |             19.17s |            13.35s |            24.13s |           18.46s |

The largest case improves by 30.3% in wall time and 23.5% in CPU time. Its backend median
falls from 7.00s to 3.09s and instance discovery from 2.11s to 0.81s. However, increasing
added functions roughly 4× from 511 to 2,047 still increases baseline-subtracted wall time
about 5.1× (previously 6.8×). This improves scaling; it does not prove global linearity.
Tiny baseline-subtracted differences are dominated by noise, including the negative 31-function
difference in both batches. The synthetic gain must not be substituted for the real parser result.

### Structural and correctness checks

An instrumented real CLI build compared every new local-use query against the previous
implementation: all 1,444 queries across 430 functions matched in length and ordered
region/operation identity. A separate synthetic attribution probe covers composite operations,
shared/cyclic metadata, cleanup, outcomes, and repeated operation objects. Its operation visits
grow as 4/16/64/256 rather than 16/256/4,096/65,536. No legacy scan remains in production.

The real CLI queries diagnostic observation 881 times on the same 878-function MIR module;
the module is now scanned once, visiting 707 functions before finding an observer. The separate
support module is also scanned once. HIR lookup tests cover new snapshots, missing/unresolved
declarations, and module/name collisions; native lookup tests retain generic/static distinctions.
The LLVM ordering regression reproduces a byte golden captured before the implementation.

All 2,444 compiler tests, native acceptance shards (112/111/111), 78 LLVM tests plus parity,
20 release-candidate checks, and 19 script tests pass. The rebuilt CLI also passes all 113
parser/stdlib files. Validation was completed in parts; the redundant serial native runner was
interrupted, so a successful monolithic `pnpm test` exit is not claimed. All 160 LSP tests and
the remaining workspace tasks pass on the final rerun without competing native jobs. An LSP
inspector test initially failed during concurrent validation and passed unchanged in isolation.
Its misleading missing-project response can also represent a query deadline; even token inspection triggers
full realization. That separate issue was not changed by this patch.

Workspace typechecking and lint pass. Root `pnpm check` and formatting still stop at the
pre-existing `.zuse/settings.toml` formatting issue, left untouched. Diagnostic instrumentation
and temporary executables are outside the repository under `/tmp/silk-small-wins.3eqhvx`;
the diagnostic scaling generator is `/tmp/silk-perf-hypotheses.uPqhbt/scaling.mjs` (temporary,
not a shipped benchmark interface). The real-parser benchmark remains reproducible with `run.py`.

## Transient Effect-outcome storage — 2026-09-09

This experiment removes a redundant local home for single-definition `RunEffectValue` and
`RunStaticEffect` outcomes whose payload is consumed entirely inside the producing operation.
Their returned lanes already supply success extraction and failure propagation. Diagnostic
ownership lives in separate slots and is preserved. Any later value reference, address use,
redefinition, cleanup dependency, or suspension dependency keeps ordinary payload storage.
This is dead-storage elimination, not an aggregate calling-convention change or an SSA cache.

The [raw results](results/2026-09-09-transient-outcome-storage.json) include an isolated,
alternating six-build comparison of the actual `compiler/src/main.silk` CLI. Each sample uses
a fresh Node process with persistent compilation caches disabled. A temporary read-only loader
returns an empty transient-local set for the eager-storage control; both modes use the same
loader and otherwise identical compiled sources. The source/dist fingerprint is checked before
and after the batch. The eager-control binary is byte-identical to the pre-change binary at
`1f41c8cb`, and the optimized binary is byte-identical to its instruction-attribution build.
All six binaries produce the same AST output. No tests or other task builds overlap this batch.

| Metric                            | Eager storage | Transient outcomes |
| --------------------------------- | ------------: | -----------------: |
| Cold wall median                  |        42.55s |             40.55s |
| Cold wall range                   |  41.90–44.00s |       39.50–41.50s |
| CPU median                        |        54.77s |             52.29s |
| Backend median                    |        17.25s |             16.26s |
| Main-module LLVM instructions     |       800,774 |            717,180 |
| Parser-prefixed LLVM instructions |       642,402 |            562,036 |

The measured wall reduction is **4.7%**, CPU reduction **4.5%**, and main-module IR reduction
**10.4%**. This is a shared-host experiment with three samples per mode, not a universal speedup
guarantee. Startup load averages vary substantially; phase differences cannot be added as
independent savings. Peak RSS is essentially flat at these sample sizes.

Instruction attribution reconciles every function with its immutable final bitcode snapshot.
The exact difference is 1,236 allocations, 41,797 stores, and 40,561 address calculations.
Calls, loads, control-flow instructions, returns, casts, and all other opcode totals are
unchanged. The largest `primaryInner` runner falls from 31,126 to 27,284 instructions.
The remaining capture/result scalarization is not addressed by this patch.

The raw artifact also retains the earlier non-interleaved parser/CLI pilot batches, including
the 40.09s parser outlier. They are not the primary timing comparison: the failing regression
test overlapped one before sample, and the full parser oracle overlapped the start of the after
batch. Those runs motivated the isolated paired experiment rather than being selectively
discarded. Temporary controls and executables are under `/tmp/silk-effect-result.IuDji7`;
no measurement switch or loader is installed in production code.

The rebuilt parser passes all 113 source fixtures. Focused Effect, diagnostic, and suspension
tests pass; the regression first failed with eager storage and passes with the analysis.

Final validation passes all 2,444 compiler tests, native acceptance shards (112/111/111),
53 standard-library doctests, 20 release-candidate checks, and 19 script tests. The remaining
workspace run passes all 21 tasks, including 160 LSP and 87 CLI tests. Unchanged dependencies
reuse their valid Turbo results, including LLVM's 78 tests and parity checks.

Validation was completed in parts: Turbo's strict environment filtering stripped
`SILK_NATIVE_SHARD` from the initial root test command, so its redundant unsharded native runner
was interrupted. All three shards were run directly with their shard setting preserved; a
successful monolithic `pnpm test` exit is not claimed. Typechecking and lint pass. Root
`pnpm check` and formatting stop at the pre-existing `.zuse/settings.toml` formatting issue,
left untouched. No OpenSpec artifacts were added for this experiment.

## Caller-provided result storage experiment — 2026-09-09

**Rejected:** changing native result transport did not demonstrate a cold-compilation gain.
The experimental implementation and its ABI-specific tests were removed; the preceding
transient-outcome optimization remains unchanged. The rebuilt compiler's entire `dist`
directory is byte-identical to the pre-experiment snapshot.

The experiment kept scalar returns direct and moved aggregate results into caller-provided
stack storage. Suspension returned its status separately, with result storage confined to an
invocation rather than retained in continuation frames. Diagnostic ownership remained separate.
Four variants tested flat result fields, coalesced zero initialization, canonical byte-layout
transfers, and finally batched success projection plus direct construction of a single safe
return-only local in the caller's buffer. The first three single-build pilots were slower;
the final variant received the alternating comparison below.

| Metric                        | Existing ABI | Experimental ABI |
| ----------------------------- | -----------: | ---------------: |
| Cold wall median              |       38.28s |           38.60s |
| Cold wall range               | 37.93–45.07s |     37.98–38.62s |
| CPU median                    |       50.13s |           50.42s |
| Backend median                |       14.75s |           14.66s |
| Object-generation median      |        3.26s |            3.47s |
| Main-module LLVM instructions |      717,180 |          686,830 |

This is three fresh-process samples per mode, with persistent compilation caches disabled.
Both modes use the same read-only loader: the control loads the initial compiled compiler
snapshot, which already includes transient-outcome storage elimination. LLVM is identical in
both modes; the experimental fixed-size entry-allocation option defaults to its old behavior.
Source/dist fingerprints agree before and after the batch. No tests or other task builds overlap
the timing samples. All six binaries produce identical real-input AST output, and the final
experimental binary passes all 113 parser differential fixtures.

The fifth attempted sample timed out after reporting compilation complete but before emitting
its timing breakdown. It is retained as an excluded attempt and was retried before the final
after sample. The initial control's 45.07s remains in the batch; its apparent advantage over the
first after sample did not reproduce. Medians are effectively neutral, not evidence of a win.

Separate instruction-attribution runs reconcile every function with its final LLVM snapshot.
The final variant removes 47,792 `extractvalue`, 13,597 `insertvalue`, and 9,079 phi instructions,
but adds 23,900 loads, 20,988 address calculations, 1,252 allocations, and 884 calls. Total IR
shrinks only **4.2%**. Backend emission improves **0.6%**, object generation regresses **6.5%**,
and total wall time regresses **0.8%**. Lower instruction count alone was not an adequate proxy
for compilation time.

The [raw record](results/2026-09-09-result-transport-experiment.json) includes all six samples,
phase timings, fingerprints, opcode totals, negative pilot logs, and the excluded attempt.
The recoverable experimental diff and binaries remain under `/tmp/silk-result-transport.MoglOG`;
no alternate ABI, benchmark switch, or compatibility path remains in production.

The next hypothesis should target materialization across complete producer/consumer paths,
especially successful Effect payload forwarding, rather than change only the return boundary.
This experiment does not establish that caller-provided result storage is intrinsically worse;
it establishes that this implementation is not a demonstrated fix for this cold-build workload.

After restoration, typechecking and lint pass, 32 focused compiler tests pass, and the
non-compiler workspace test run passes all 21 tasks using their valid cached results.
The full root test command was stopped rather than repeat the unchanged compiler's complete
native corpus; its successful monolithic exit is not claimed. The preceding section records
that implementation's full validation. Root formatting and `pnpm check` still stop only at the
pre-existing `.zuse/settings.toml` formatting issue, which was left untouched.

## Adjacent success-return fusion experiment — 2026-09-09

**Rejected:** removing intermediate materialization from successful Effect forwarding did not
demonstrate a cold-build improvement. The implementation and experimental assertion were
reverted. The preceding transient-outcome optimization remains intact, and the rebuilt compiler
`dist` is byte-identical to the pre-experiment snapshot.

The experiment fused adjacent `RunEffectValue` / `RunStaticEffect` → `PackEffectOutcome(tag 0)`
→ `Return` chains, passing successful payload fields directly to return packing. It retained the
existing native ABI and failure cleanup. Fusion excluded suspendable functions and addressed or
redefined intermediate roots; it did not retain a value cache across joins or other operations.
An initial scan found 102 syntactic candidate chains before those safety filters.

| Metric                        | Existing emitter | Fused emitter |
| ----------------------------- | ---------------: | ------------: |
| Cold wall median              |           41.09s |        41.41s |
| Cold wall range               |     40.56–41.12s |  40.77–41.91s |
| CPU median                    |           52.34s |        52.21s |
| Backend median                |           16.24s |        16.42s |
| Object-generation median      |            2.42s |         2.44s |
| Main-module LLVM instructions |          717,180 |       686,353 |

The six fresh-process builds alternate before/after/after/before/before/after, with persistent
compilation caches disabled. Both modes use the same read-only loader, which selects the
pre-experiment compiler snapshot for the control. Source/dist fingerprints agree before and
after the batch, LLVM is unchanged, and no task tests or builds overlap timing samples. This
is a shared-host measurement, not an OS-page-cache-cold benchmark. All six binaries produce
identical AST output for `compiler/src/main.silk`.

Separate instruction attribution reconciles every function with its final LLVM snapshot.
Fusion removes 6,636 stores, 7,222 loads, 13,350 address calculations, 2,052 phi instructions,
977 branches, 588 switches, and two casts. Other opcode totals are unchanged. The **4.3% IR
reduction** does not translate into a demonstrated timing gain: wall time is 0.8% higher,
CPU time is effectively flat, and backend time is 1.1% higher. These differences do not establish
a reliable regression either. The narrower forwarding chains are not a demonstrated fix for
this workload; this does not rule out broader producer/consumer materialization elimination.

The targeted structural regression failed before fusion and passed afterward. Full native
acceptance was not run for the rejected implementation, so the AST comparison is not a claim
of general semantic equivalence. The [raw record](results/2026-09-09-success-return-fusion-experiment.json)
retains all samples, phase timings, fingerprints, and opcode totals. The recoverable patch,
before/after source files, binaries, and probe scripts remain under
`/tmp/silk-materialization.42ovcr`; no experimental path remains in production.

A next measurement can separate Effect capture materialization from result materialization.
The existing `MirNormalization` direct-run conversion deliberately excludes affine or borrowed
captures. Measure how much work those excluded cases account for before changing their
ownership-sensitive representation; simply relaxing that guard would not be justified.

After restoration, typechecking and lint pass, all 32 tests in `Backend.test.ts` and
`StoredEffectMir.test.ts` pass, and the non-compiler workspace run passes all 21 tasks using
their valid cached results. The unchanged compiler's full native corpus was not rerun; its
preceding validation is recorded above. Root `pnpm check` stops at the pre-existing
`.zuse/settings.toml` formatting issue, left untouched. The experiment's Markdown and JSON
records pass targeted formatting, and `git diff --check` passes.

## Effect capture materialization probe — 2026-09-09

**Positive headroom measurement, not an implemented optimization.** Separating capture loads
from the rest of Effect execution identifies a larger target than capture-container construction.
No production source, ABI, or ownership behavior was changed.

A read-only instruction probe labels the exact `NativeStorage.materialize` calls that unpack
the environment for `RunEffectValue` and `CatchEffect`. Its binary is byte-identical to the
uninstrumented baseline. Every function's instruction attribution reconciles with its final
LLVM snapshot. In the 717,180-instruction main module:

| Emission work                       | Instructions | Main-module share |
| ----------------------------------- | -----------: | ----------------: |
| Capture construction (`MakeEffect`) |        4,510 |              0.6% |
| Capture loads for `RunEffectValue`  |      127,514 |             17.8% |
| Capture loads for `CatchEffect`     |           23 |            <0.01% |

Capture construction already uses canonical byte transfers for aggregate fields. The costly
step is translating their stored representation into flattened call arguments. `NativePlace`
must emit loads, tag dispatch, and joins for nested union representations. The callee then
stores its incoming parameters again. Removing `MakeEffect` alone would retain the argument
conversion and therefore miss most of the measured work.

The declared main module contains 1,229 `MakeEffect` sites and 1,023 `RunEffectValue` sites;
973 constructions are immediately followed by a run of the constructed value. Captures are
not predominantly borrowed-pointer fields: the construction scan includes 1,382 `Take:Value`
fields. Common parser captures are `State` (336 bytes), `NodeResult` (344 bytes), and
`ElementsResult` (416 bytes). These are static site/layout observations, not runtime execution
counts or live-memory measurements. The parser intentionally threads state by ownership;
this result does not require changing that source-level protocol.

### Deliberately invalid omission experiment

To test compilation-time headroom before an argument-transport rewrite, a temporary loader
replaces only those two capture materializations with correctly typed null argument values.
**The omission outputs are semantically invalid, were never executed, and have executable
permissions removed. They are not usable compiler artifacts.** This is a diagnostic knockout,
not a candidate optimization or a correctness test.

| Metric                   | Normal capture loads | Loads omitted |
| ------------------------ | -------------------: | ------------: |
| Cold wall median         |               40.13s |        37.64s |
| Cold wall range          |         39.98–41.45s |  37.01–38.40s |
| CPU median               |               51.02s |        49.17s |
| Backend median           |               16.06s |        13.89s |
| Object-generation median |                2.41s |         2.52s |

The omitted main module has 589,643 instructions, exactly 127,537 fewer than the control.
The difference consists of 45,356 loads, 43,499 address calculations, 27,040 phi instructions,
7,748 branches, 3,868 switches, and 26 casts. Other opcode totals are unchanged, and the separate
runtime module remains at 112 instructions. This confirms that the measured capture conversion
includes substantial control-flow expansion, not only ordinary scalar loads.

Six fresh-process samples alternate control/omit/omit/control/control/omit. Persistent
compilation caches are disabled; both modes use the same loader and identical compiler/LLVM
source and dist files. The source/dist fingerprint agrees before and after the batch, and all
three control binaries are byte-identical to the uninstrumented baseline. No task tests or
other builds overlap the timing samples. This is a shared host without an OS page-cache flush.
Separate instruction-attribution builds are excluded from timings.

The omission probe saves **2.50s (6.2%)** in median wall time and **2.17s (13.5%)** in backend
time. It does not predict a guaranteed implementation gain: a real transport scheme must do
replacement work, and constant arguments can affect downstream code generation. Nor is this
a strict bound for a different ABI that might also remove callee-side reconstruction. The
earlier single-build pilot (40.39s versus 37.61s) is retained separately from the paired result.

The next justified prototype is canonical-memory transport for large internal aggregate
arguments: pass their stored address and preserve value/ownership semantics with a byte copy
or a proven ownership transfer, rather than unpacking and repacking every scalar field. It must
preserve capture-time snapshots, reusable Effects, borrows, diagnostic cleanup, and suspension
lifetimes. Do not simply relax `MirNormalization`'s affine-capture guard or move ordinary reads
across capture construction.

The [raw record](results/2026-09-09-capture-materialization-probe.json) retains the samples,
phase timings, fingerprints, instruction attribution, static capture-shape summary, and the
omission loader/harness. Scratch probes and clearly marked non-runnable outputs are under
`/tmp/silk-captures.oUGJGc`. No OpenSpec or production benchmark switch was added.

Validation checks the six-sample record, matching control hashes, source/dist fingerprint,
reconciled opcode/bucket totals, and non-executable omission artifacts. Compiler `dist` remains
byte-identical to the pre-experiment snapshot. Typechecking and lint pass; the non-compiler
workspace test run passes all 21 tasks using valid cached results. Compiler correctness/native
suites were not rerun for this measurement-only change. Root formatting and `pnpm check` stop
at the pre-existing `.zuse/settings.toml` formatting issue, left untouched. The new records pass
targeted formatting and `git diff --check`.

## Canonical aggregate arguments — 2026-09-09

**Implemented and kept.** The runnable argument-transport change saves **3.18s (7.4%)** in
median cold compilation of `compiler/src/main.silk` and removes **136,497 LLVM instructions
(19.0%)** from its main module. This follows the capture-materialization probe above; it does
not omit capture values or change the source program.

Synchronous Effect runners now accept canonical aggregate addresses rather than flattened
field lanes. The caller retains the aggregate's stored representation, and the callee copies
its bytes into its own local storage on entry. Capture arguments are projected from the
stored capture-time environment, never reread from the original source locals. Value isolation
and ownership behavior are therefore preserved without requiring copy-elision proofs.

The policy uses the existing semantic `Place` classification, not a size threshold tuned to
this benchmark. Results, machine/C ABI, scalar arguments, suspension frames, and persistent
continuation payloads retain their respective representations. Already-materialized composite
and continuation arguments are lowered through the same declared parameter plan. When they
need a temporary canonical slot, its allocation is placed in the LLVM entry block so calls in
loops do not repeatedly grow the stack. No production benchmark switch or OpenSpec was added.

### Final paired measurement

| Metric                   |       Before |        After |
| ------------------------ | -----------: | -----------: |
| Cold wall median         |       42.78s |       39.61s |
| Cold wall range          | 42.17–43.32s | 37.36–42.29s |
| CPU median               |       54.75s |       50.12s |
| Backend median           |       16.76s |       14.77s |
| Object-generation median |        4.11s |        2.32s |
| Main LLVM instructions   |      717,180 |      580,683 |

The CPU reduction is **8.5%** and the backend reduction is **11.8%**. The initial runnable
prototype independently measured **40.08s → 37.19s (7.2%)** across another six-build batch,
before entry-allocation lifetime and zero-capture-path refinements. Its object-generation
median was essentially flat (2.43s → 2.47s), so the larger final-batch object-time difference
should not be treated as a stable separate gain. The batches are recorded separately, not
pooled. Wall-time variability remains visible; these are small paired samples on a shared host.

Both batches alternate before/after/after/before/before/after, use fresh Node processes, and
disable persistent compilation caches. No task tests or other builds overlap included timing
samples; there is no OS page-cache flush. A read-only loader supplies the pre-change compiler
snapshot, which already includes the preceding transient-outcome optimization. Both final-batch
modes use the final LLVM library; entry placement is opt-in. Every control binary is byte-identical
to the original baseline, and every candidate binary is byte-identical to the separately
instrumented candidate. All six final AST outputs match the reference, and the source/dist
fingerprint agrees before and after the batch.

The instruction attribution reconciles every emitted function. Main-module function count stays
at 900; the separate seven-function runtime stays at 112 instructions. Capture-load instructions
under `RunEffectValue` and `CatchEffect` fall from 127,537 to 935. Field-by-field parameter ingress
falls from 24,929 to 13,421 instructions, with 169 copy instructions on the new entry path.
Overall opcode reductions include 45,251 loads, 46,854 address calculations, 27,040 phi nodes,
8,210 branches, 4,328 switches, 4,952 stores, and 32 casts; there are 169 additional calls and
one additional allocation. This is a structural reduction, not a JavaScript-specific shortcut.

### Correctness and validation

The new structural regression proves the aggregate pointer signature and callee-owned copy;
it fails under the original transport. The LLVM allocation regression proves that fixed call
storage is placed in entry without moving the loop insertion point. The existing composite
capture-arity corpus case now also checks an aggregate snapshot after the original Copy value
has been changed.

All 2,445 compiler tests, 79 LLVM tests plus parity, 319 native corpus cases plus five shared
native checks, 113 parser inputs, 53 stdlib doctests, 68 docgen tests, 20 release-candidate checks,
and 19 script tests have passing coverage. The remaining workspace run passes all 20 tasks
with compiler and docgen validated separately.

The initial validation jobs were over-parallelized. Two compiler tests, the shared archive-relink
check in each native shard, one parser input, one docgen test, and six release checks timed out;
all passed unchanged on retries with reduced contention. No timeout or assertion was relaxed.
These validation timings are not performance samples. Final typechecking and lint pass.
Root formatting and `pnpm check` remain blocked solely by the pre-existing user-owned
`.zuse/settings.toml` formatting issue, left untouched; the changed files pass targeted formatting
and `git diff --check`.

The [raw record](results/2026-09-09-canonical-argument-transport.json) retains both timing batches,
phase data, fingerprints, reconciled instruction totals, harnesses, and validation outcomes.
Before-source snapshots, logs, and runnable binaries remain in `/tmp/silk-arguments.abqEOK`.
