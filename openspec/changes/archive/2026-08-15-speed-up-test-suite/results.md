# Before/after: packages/compiler suite

Same machine, same all-cores contended conditions, workspace built. Baseline run on the
merge of origin/main (691092e); final run after all changes.

| Metric | Baseline | After | Delta |
|---|---|---|---|
| Wall | 184.0s | 133.9s | −27% |
| Test CPU (sum of per-file) | 1,673s | 1,174s | **−30%** |
| Test files | 233 | 213 | −20 |
| Tests | 1,852 | 1,818 | −34 (plus ~150 native legs removed inside surviving tests) |
| Failures | 1 (pre-existing on main) | 1 (same: WasmShadowStackHeapCollision, flagged separately) | — |

Solo-file measurements (uncontended): LexerPressure 143→39s, StackVmPressure 82→30s,
SynchronousEffectCost 41→10s.

Top remaining files (contended): TemporaryDirectoryAcceptance 123s (real-syscall KEEP),
LexerPressure 77s, DriverNativeAcceptance 68s (absorbed 19 folded programs), StackVmPressure 57s,
ModuleVerification 41s.

## Target check (task 8.3)

Target was ≥35% compiler-suite CPU reduction; measured −30% under contention. Per the task's
instruction, the shortfall points at the follow-up list rather than more scope here:

- **`Analysis` snapshot sharing across engines** — parity tests still build 2–3 full frontends per
  program; the native spike measured this as the single largest remaining cost.
- **Stdlib elaboration memoization** — 551 `ofSourceRealized` sites re-elaborate stdlib at
  ~200–450ms each; also benefits the LSP.
- **TemporaryDirectoryAcceptance** (123s) is now the worst file; it is a legitimate syscall KEEP,
  but its three tests re-compile large programs and could share one compiled binary.

CI additionally gains what the local diff cannot show: turbo cache and native artifact cache
persistence across runs (cold CI previously re-ran everything), and `pnpm check` on the 4-core
runner drops from ~7 to ~5 minutes of compiler-suite CPU before any cache hit.
