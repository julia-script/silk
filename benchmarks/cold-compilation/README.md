# Cold compilation baseline

Matched, entirely in-memory programs in Rust, Zig, and Silk. This benchmark measures
developer-facing native executable build latency, including compiler startup, compilation, runtime
support generation when needed, and linking. It does not change the compiler or use stdin/stdout,
filesystem APIs, or other runtime input services in the fixtures.

## Larger-workload comparison — 2026-09-08

Same toolchain versions and host as the original baseline below. The repeat batch measured four
samples per cell, including linking; every resulting executable passed its self-checks.

| Workload                   | Rust median (range) | Zig, seeded toolchain (range) | Zig, empty caches (range) |    Silk median (range) |
| -------------------------- | ------------------: | ----------------------------: | ------------------------: | ---------------------: |
| Small evaluator (`parser`) |    180 ms (172–190) |        1,456 ms (1,418–1,477) |    4,046 ms (4,015–4,116) | 1,432 ms (1,373–1,448) |
| Lexer + flat AST (`ast`)   |    369 ms (211–575) |        1,507 ms (1,446–1,772) |    4,107 ms (4,014–4,516) | 7,252 ms (7,038–8,143) |

For the AST fixture, Silk is **19.7× Rust**, **4.81× seeded Zig**, and **1.77× empty-cache Zig**.
The proposed 10× faster-native budget is **3.69 seconds**: this fixture misses it by approximately
2×. The small evaluator remains within its 1.80-second budget at 7.97× Rust. Within this repeat
batch, Silk's median rises about 5.1× between workloads, Rust's about 2.1×, and seeded Zig's about
1.04×. These are observed workload contrasts, not isolated per-feature costs or asymptotic bounds.

AST median peak RSS is approximately Rust 103 MiB, seeded Zig 236 MiB, empty-cache Zig 381 MiB,
and Silk 753 MiB. The runtime execution that verifies each binary is outside the build timing.

The [repeat results](results/2026-09-08-m1-max-ast-repeat.json) retain every sample, command,
source hash, and toolchain-seed invocation. The [first batch](results/2026-09-08-m1-max-ast.json)
is also retained in full: it began under severe competing load (one-minute load average 111.30)
and ended at 36.84. Its tiny Silk median was 2.94 seconds and AST median 7.14 seconds, with an AST
range of 6.97–17.46 seconds. Competing jobs subsided during that batch, so comparing its workloads
directly would mix markedly different load conditions. We reran the entire batch, not selected
outliers. Both batches together contain 64 successful measured builds/executions plus 16 successful
untimed Zig seed builds/executions.

The repeat was still a shared interactive host, not an isolated machine. Its one-minute load
average declined from 34.61 to 11.91, and Rust's AST samples still ranged from 211 to 575 ms.
The ratios are an engineering baseline, not a precise language ranking. The robust finding is that
adding a real lexer, allocated flat AST, recovery, and evaluation exposes multi-second Silk build
costs **without stdin, filesystem input, or AST output**. It does not isolate which added feature
causes that cost, nor explain all of the earlier full compiler's approximately 46 seconds.

## Original small-program baseline — 2026-09-08

Apple M1 Max, 32 GiB RAM, macOS 15.5 / arm64. Rust 1.96.0 (LLVM 22.1.2), Zig
`0.17.0-dev.1503+1f1bee62e` (default backend reports `stage2_llvm`), Node 26.7.0, and
Homebrew LLVM/Clang 22.1.8. Silk includes the uncommitted compiler performance repairs; the
exact revision, tracked source-diff digest, CLI digest, and generated toolchain identity digest
are recorded in the [raw results](results/2026-09-08-m1-max.json).

Six samples per cell, all 36 builds and resulting executables successful. Times include linking.

| Workload                    | Rust median (range) | Zig median (range)     | Silk median (range)    | 10× faster-native median |
| --------------------------- | ------------------- | ---------------------- | ---------------------- | ------------------------ |
| Minimal executable          | 246 ms (231–624)    | 4,706 ms (4,560–5,721) | 1,753 ms (1,702–2,090) | 2,463 ms                 |
| In-memory expression parser | 254 ms (241–615)    | 4,593 ms (4,533–4,674) | 1,894 ms (1,828–2,512) | 2,545 ms                 |

Silk is **7.1× Rust** on the minimal executable and **7.4× Rust** on the parser. It meets the
proposed 10× median budget on these two fixtures. This result does **not** establish acceptable
performance for the complete self-hosted compiler: it isolates a small-program baseline against
which future, larger matched workloads can expose scaling costs.

Median OS-reported peak RSS is approximately 69 / 368 / 263 MiB for minimal Rust / Zig / Silk,
and 80 / 375 / 271 MiB for the parser. These are resource observations, not equivalent live-heap
measurements across implementations.

This was a shared interactive host, not an isolated benchmark machine. Load average was
12.71 / 12.58 / 11.79 before the run and 11.11 / 12.21 / 11.73 afterward. Earlier smoke builds
were used only to validate the harness and fixtures; all six samples in each measured group are
retained, including the Rust outliers. An SDK-version metadata probe failed to resolve
`/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk`; its full error is preserved in the JSON.
All actual native compilation and linking commands nevertheless succeeded. No SDK configuration
was changed as part of this experiment.

Zig's larger number is for **empty local and global caches**, not ordinary repeat project builds.
Rust retains its installed precompiled standard library. Do not use this table to claim Silk has
a faster code generator than Zig; the included toolchain setup costs differ. The 10× budget uses
the faster native result (Rust here), rather than benefiting from that asymmetry.

## Workloads

- `minimal`: an executable that returns successfully, to expose the fixed build cost.
- `parser`: a six-function recursive-descent evaluator with a fixed-capacity input array, mutable
  cursor, whitespace skipping, decimal integers, parentheses, addition, multiplication, and invalid
  input handling. Each executable checks the same six inputs and expected results. All arithmetic
  in this corpus stays within signed 32-bit bounds; whitespace is ignored even between digits.

The small `parser` arrays contain ASCII values and an explicit `-1` end marker. There is no Unicode, dynamic
allocation, token vector, AST construction, file loading, or effectful I/O in this small evaluator.
It is **not equivalent in scope to the complete self-hosted Silk parser**. Do not compare its
native baseline directly with the earlier 46-second full-compiler build and call that a slowdown
ratio. A full-parser comparison would require a correspondingly larger matched Rust/Zig workload.

The six cases are `(12+3)*(4+5)` → 135, `1+2*3` → 7, `(7+8)*2` → 30, and three invalid inputs:
`1+`, `(1+2`, and `1x`. Invalid input returns the sentinel 255. Every successful fixture run exits
zero without output; a mismatched result makes the executable fail.

### Larger lexer and flat AST (`ast`)

The three `ast.*` programs implement the same small language and pipeline:

1. Lex an in-memory UTF-8 string as ASCII bytes into a growing token vector. Tokens retain their
   half-open byte spans. Skip whitespace and `#` line comments; report unknown bytes.
2. Parse semicolon-terminated expressions and assignments into a growing flat node vector.
   Node IDs are array indices; children precede parents. A separate vector stores statement roots.
3. Recover from missing operands, missing closing parentheses, and missing semicolons. Keep error
   nodes and diagnostics, synchronize at semicolons, and continue parsing following statements.
4. Evaluate the AST in postorder using growing vectors for node results and variable bindings.
   Checked indexing into the results vector rejects forward/out-of-bounds child IDs.

The AST is a tagged union with number, name, negate, binary, assignment, and error variants.
Names are single lowercase letters; operators are unary minus and binary `+`, `-`, and `*`, with
parentheses and conventional precedence. Unassigned names and error nodes evaluate to zero.
Parentheses group expressions without creating their own nodes. This is a bounded benchmark
corpus, not an arbitrary-input parser: it has no integer-overflow diagnostics, Unicode identifiers,
or recursion-depth limit. The supplied corpus stays within signed 32-bit arithmetic bounds.

All implementations start collections empty and grow through their standard-library APIs:
Rust `Vec`/`String`, Zig `ArrayList` with `page_allocator`, and Silk `Vector`/`String` with an
allocator provider. Allocation strategies and failure machinery are not identical. Zig propagates
allocation errors; Silk exercises effectful allocation and provider specialization; Rust uses its
ordinary infallible collection APIs. Their compilation costs are intentionally included, as they
are part of implementing this workload idiomatically in each language.

Every binary checks these cases without printing or reading runtime input:

| Source                                                               | Sum of statement values | Nodes | Roots | Diagnostics | Diagnostic checksum |
| -------------------------------------------------------------------- | ----------------------: | ----: | ----: | ----------: | ------------------: |
| `a = 12; b = (a + 3) * 9; b - a * 2; -b + 140;`                      |                     263 |    17 |     4 |           0 |                   0 |
| `1 + ; 2 * 3; (4 + 5; 6;`                                            |                      22 |    10 |     4 |           2 |                  53 |
| `@; 7;`                                                              |                       7 |     2 |     2 |           2 |                   5 |
| `1 2; 3;`                                                            |                       4 |     2 |     2 |           1 |                   9 |
| A comment followed by a newline                                      |                       0 |     0 |     0 |           0 |                   0 |
| First case repeated 64 times, each followed by a comment and newline |                  16,832 | 1,088 |   256 |           0 |                   0 |

Checks also require every node span to be ordered and within the source. The diagnostic checksum
is the sum of `start + end + code`, not a collision-free comparison of the complete diagnostic
sequence. Codes are 1 = invalid byte, 2 = expected operand, 3 = expected `)`, 4 = expected `;`.
The repeated case constructs its source at runtime and exercises vector growth; this is not a
runtime-speed benchmark, and making the source string longer does not itself enlarge the compiler
workload. The lexer, AST, collections, recovery, and evaluation implementations are what add code.

This fixture sits between the tiny evaluator and the complete self-hosted compiler. A diagnostic
Silk build emits 204 symbols, compared with 6 for the tiny evaluator and approximately 720 for the
full compiler. Symbol counts are descriptive, not equivalent units of work across languages.
It still excludes the full Silk grammar, nested generic AST element vectors, filesystem input,
and AST printing. It cannot attribute the full compiler's build time specifically to its I/O shell.

During fixture development, Silk rejected two equivalent compact source forms in MIR validation:
reading `parser.position` after borrowing `parser.tokens` in a call, and assigning an effectful
binary-node result directly to a loop-carried variable. The fixture uses a saved position and a
named result temporary. No production compiler repair or validation bypass was made in this
experiment; those compact forms remain follow-up compiler issues.

## Reproduce

Requirements: macOS, Python 3, `rustc`, `zig`, and Node on `PATH`, plus a built Silk workspace and
its native LLVM toolchain. No Python packages, Cargo dependencies, or Zig packages are needed.

```sh
pnpm build
python3 benchmarks/cold-compilation/run.py --samples 4 --workloads parser ast --output /tmp/cold-baseline.json
```

Choose a new output filename for each run: the harness refuses to overwrite an existing result.
The initial workspace build is preparation, **outside** Silk's measured source-to-executable build.
Rust and Zig are also already-installed compilers, not rebuilt from their implementation sources.
The harness uses the built Silk CLI directly through Node, excluding pnpm's package-runner overhead.

Omit `--workloads` to include all three workloads. The current harness records four configurations:
Rust, Zig with empty caches, Zig with a seeded toolchain cache, and Silk. The original JSON above
was produced before the seeded configuration was added and remains a historical artifact.

Run with other CPU/memory-heavy work stopped. The harness serializes all builds and rotates the
configuration order each round; four rounds put each configuration in each position once. Program execution
and correctness checks occur after timing stops. A failed build, timeout, unexpected output, or
failed program prevents publication of a result file. Compiler stdout/stderr, OS resource reports,
the exact command, input hashes, source revision/diff hash, and load averages are retained.

## What “cold” means

Every sample gets a new compiler process and a new output directory. No warmup samples are removed.
Rust is invoked directly without `-C incremental`; wrapper and incremental environment overrides
are cleared. Both Zig configurations disable incremental compilation and use fresh local caches:

- `zig` also gets an empty global cache, measuring strict build-cache-cold latency.
- `zig-seeded` gets an independent global cache populated only by compiling `toolchain-seed.zig`.
  This seed is a distinct, trivial arithmetic program, not any measured fixture. Its build and
  verification occur outside the measured interval and are recorded separately in each row.
  Each sample gets its own seed/cache, so no parser or AST compilation can carry over. This
  measures project-cold latency with some toolchain support available, not fully empty caches.

Silk's persistent native cache is disabled by removing its environment
configuration; a fresh Node process cannot reuse the process-local cache. Node's compile cache is
also disabled. Scratch directories are removed after each sample, including on failure.

This is **build-cache-cold**, not machine-cold: the OS filesystem page cache is not flushed, compiler
binaries may already be resident, and installed SDK/toolchain assets remain available. In
particular, Rust's installed prebuilt standard library is retained. Zig's fresh global cache can
require regeneration of toolchain support that ordinary repeat builds reuse. This is an important
asymmetry of the installed toolchains, not evidence that Zig's frontend is intrinsically slower.
Do not relabel the empty-cache measurements as incremental or warm-toolchain builds; the seeded
measurements are labeled separately and do not reuse a measured program's application artifacts.

## Build settings

Only debug/unoptimized builds are measured, matching the original Silk debug-build complaint.
The fixed inputs are deliberately not used to make optimized-build claims: that would need an
opaque-input design and verification that the evaluator had not been constant-folded away.

| Language | Settings                                                                                                                  |
| -------- | ------------------------------------------------------------------------------------------------------------------------- |
| Rust     | `rustc --edition=2024 -C opt-level=0 -C debuginfo=2`                                                                      |
| Zig      | `zig build-exe -O debug -fno-incremental`, default backend, fresh local cache; empty or independently seeded global cache |
| Silk     | `node packages/cli/dist/bin.js build-exe … --optimization debug`                                                          |

Native host targets and the installed tools' default linkers are used. Debug modes are comparable
developer build modes, **not identical debug-info, safety-check, target-feature, or runtime
implementations**. The recorded Zig version uses lowercase `debug`; older releases may use
different option spelling. The harness intentionally records versions rather than silently
substituting toolchains or backends.

Wall time uses a monotonic clock around `/usr/bin/time -l <compiler> …`, through compiler exit and
the completed executable. Peak RSS is the OS value reported by that command, in bytes on macOS;
it is not the sum of simultaneously live memory across an entire process tree. Binary sizes are
recorded for inspection, not used as a correctness or performance target.

Rust option semantics: [rustc code-generation options](https://doc.rust-lang.org/rustc/codegen-options/).
Zig's options/backend are checked against the installed `zig build-exe --help` and
`zig build-exe --show-builtin -O debug`, because this host uses a development toolchain.

## Interpreting a 10× target

For each workload, use **10 × the faster native compiler's median**, not 10× whichever comparison
makes Silk look best. The JSON records that budget and all individual slowdown ratios. Medians
and full ranges should be considered together; these small sample sets are a local engineering baseline,
not a statistically portable ranking of languages.

Meeting this budget establishes performance only for the measured fixture. The larger AST fixture
adds generic collections and ownership/effect lowering, but neither fixture establishes acceptable
scaling for the complete self-hosted compiler or every large generated LLVM module.

## Silk scalar and descriptor controls

```sh
python3 benchmarks/cold-compilation/silk-controls.py --samples 3 --output /tmp/silk-controls.json
```

This Silk-only companion reuses the same fresh-process measurement and runtime-oracle code.
It rotates the minimal scalar executable and `descriptor.silk`, which passes a UTF-8 string
through ordinary calls, converts it to a byte slice, and checks the descriptor length and first
byte. There is no input, allocation loop, wide user aggregate, or Rust/Zig counterpart claimed
for the descriptor fixture. Hosted startup and reachable standard-library code remain included.
These controls measure small-program build costs, not just the isolated instructions handling
a scalar or descriptor. The descriptor control was added with the typed-place implementation;
it has no matched pre-change timing baseline.
