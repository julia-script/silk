# Native LLVM construction benchmark (JUL-154)

This opt-in benchmark measures `NativeProgram.emit` from immediately before `Builder.make`
through the completed module, immediately before `Verify.verify`. It excludes verification,
bitcode encoding, source analysis, MIR realization, object generation, and linking. No timing
threshold is part of `pnpm check`.

## Recorded result

On Node v26.7.0, Apple M1 Max, arm64 macOS (Darwin 24.5.0), with `--expose-gc`:

| Workload                                        | Baseline median | Candidate median |                        Construction reduction |
| ----------------------------------------------- | --------------: | ---------------: | --------------------------------------------: |
| One-function control                            |        0.938 ms |         0.959 ms | -2.2% (0.021 ms regression; passes allowance) |
| Arithmetic (161 symbols, 13,764 MIR operations) |    2,222.007 ms |     1,228.513 ms |                                         44.7% |
| Lexer (347 symbols, 6,526 MIR operations)       |    4,859.083 ms |     2,591.815 ms |                                         46.7% |

All three bitcode artifacts and symbol/MIR inventories match byte for byte; all diagnostics and
verifier results are unchanged. Median whole-process peak RSS is within the 10% bound for each
workload. These are same-host observations, not portable absolute thresholds or an attribution
of memory savings to construction.

[evidence/comparison.json](evidence/comparison.json) records both revisions and every gate.
`evidence/baseline-samples.json` and `evidence/candidate-samples.json` retain every warmup,
measured duration, fresh-process RSS observation, and environment identity. The corresponding
`*-artifacts.tar.gz` archives contain the exact bitcode, full inventories, and a separate
construction-only lexer CPU profile plus its sample metadata. `recorded-harness.mjs` in each
archive is the exact harness at `63eff3ed`, matching the recorded SHA-256; subsequent tooling
changes make buffer decoding, configuration reads, and ordering explicit without changing the
measured module-construction boundary. Archives can be extracted with
`tar -xzf`; copy the corresponding sample JSON to `samples.json` in each extracted directory
to rerun `construction-compare.mjs`.

The separate cold profiles attribute 809 ms of baseline sampled self time to `Result.js` and
626 ms to `InstructionEncoder.js`, versus 281 ms and 236 ms in the candidate. Garbage-collector
sampled time also falls from 1,327 ms to 767 ms. These profiles identify the allocation-heavy
instruction path; their cold timings and inspector overhead are excluded from the performance
gates. The complete module totals and aggregation method are in
[evidence/attribution.json](evidence/attribution.json).

## Frozen workloads

- `lexer/`: experimental lexer copied from the untracked `compiler/src` and `compiler/silk.toml`
  in Julia's `c266` worktree on 2026-09-07. No build outputs are copied. Its current standard-library
  realization has 347 functions and 6,526 MIR operations, rather than the historical issue's 314
  symbols. Inputs are compiled, not executed; the original empty runtime input is immaterial.
- `arithmetic/`: 160 explicitly frozen scalar functions with eight conditional updates each,
  called by one entry function. This exercises checked arithmetic and control flow rather than
  the lexer's bytes, aggregates, allocator, filesystem, and Effect machinery.
- `minimal/`: exact one-function `return 42` control.

The comparison baseline is checkout `dd4510fa0f42dc436da6d17e476cbc91dba593fe`, not the issue's
older dirty `03ec67f` tree. The same current compiler, embedded standard library, frozen sources,
realized MIR, target, and complete normalized debug profile are used on both sides. The baseline
already contains the August 31 removal of per-operation semaphores and instruction tracing.

## Run

The benchmark requires Node 22.15 or newer for synchronous module loader hooks. Use the recorded
Node version and flags when reproducing this comparison. Build using the pinned workspace
dependencies:

```sh
pnpm install --frozen-lockfile
pnpm exec turbo run build --filter=@silklang/compiler...
node packages/compiler/scripts/construction-benchmark.mjs --output=/tmp/construction-candidate
```

The coordinator starts one timing process per workload, collects garbage outside the measured
boundary before each emission, discards two warmups, then records five emissions of the same MIR
and request. It also runs five fresh one-emission processes for peak RSS. All children use
`--expose-gc` plus the coordinator's Node flags; Node version, flags, `NODE_OPTIONS`, host CPU,
architecture, OS, target, complete normalized profile, source hashes (including standard library),
built JavaScript hashes, harness hash, and compiler/LLVM revision identities are recorded.

RSS is `process.resourceUsage().maxRSS` in KiB: the entire child process, including frontend,
construction, verification, and encoding, excluding child-process memory. Timing-process RSS is
retained but is not used for the RSS gate. No construction-memory improvement is claimed.
Backend caching is bypassed by directly emitting each newly constructed module. Run benchmarks
sequentially on an otherwise idle host, with no concurrent checks or CPU profiler.

For a controlled LLVM-only comparison, save the unmodified baseline `packages/llvm/dist` before
building the candidate, then use:

```sh
node packages/compiler/scripts/construction-benchmark.mjs \
  --llvm-dist=/absolute/path/to/baseline-dist \
  --llvm-revision=dd4510fa0f42dc436da6d17e476cbc91dba593fe \
  --output=/tmp/construction-baseline
node packages/compiler/scripts/construction-compare.mjs \
  /tmp/construction-baseline /tmp/construction-candidate
```

The synchronous Node loader substitutes only the baseline LLVM modules; compiler code, module
identities, Effect dependency, and instrumentation remain identical. Built-file hashes identify
exact executed code even when the workspace contains documentation or benchmark edits. Use a
fresh output directory per run: mismatched bitcode from any sample is rejected.

The loader injects the two timing boundaries only into the built native program module in the
benchmark process. Both source anchors must occur exactly once or it fails. Production packages
have no benchmark service, timers, or altered public API.

Each run retains `samples.json`, exact symbol and per-function MIR operation inventories, and one
bytecode artifact per workload. All timing and memory emissions must produce the same bytes.
The comparator refuses mismatched environments, sources, profiles, requests, or inventories and
checks exact bitcode bytes as well as the 40% medium, minimal-control, and 10% RSS bounds.

## Profiling and the bounded hot path

Run a separate construction-only profile, excluded from timing samples:

```sh
node --expose-gc packages/compiler/scripts/construction-benchmark.mjs \
  --sample --profile --workload=lexer --output=/tmp/construction-profile
```

Use `--llvm-dist` for the baseline profile. The inspector starts before the construction timer
and stops after it, before verification. Retain the `.cpuprofile` with the timing evidence.

The optimization is confined to `BuilderState`'s synchronous mutation section and
`FunctionBodyState`'s per-instruction append, operand resolution, and body validation. It removes
intermediate Effect steps and Result generators, allocates forward-cycle tracking only when a
forward operand is encountered, and freezes privately owned instruction containers in place at
commit. Public Effect signatures, typed errors, pre-mutation owner checks, fiber ownership,
atomic synchronous transitions, and the scoped body transaction are preserved. This evidence
fulfills the measured-hot-path requirement of `llvm-builder-parity`; no prescriptive contract
changes or OpenSpec delta are needed.
