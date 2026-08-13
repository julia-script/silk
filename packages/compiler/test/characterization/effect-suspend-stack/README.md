# Effect suspension stack characterization

This fixture records the pre-`Effect.suspend` recursion boundary independently for four shapes:

- `scalar-non-tail`: scalar non-tail recursive Effect execution;
- `box-build`: recursive Effect construction followed by iterative consuming teardown;
- `box-walk`: iterative construction, recursive borrowed traversal, then iterative consuming teardown;
- `box-drop`: iterative construction followed by ordinary recursive `Box.drop` cleanup.

The iterative teardown used by `box-build` and `box-walk` consumes every link through `Box.into`,
so every allocation is released normally without adding recursive `Box.drop` to those measured
paths. The fixture does not suppress cleanup or intentionally leak storage.

Run it after building the compiler:

```sh
pnpm --filter @silk-effect/compiler build
node packages/compiler/test/characterization/effect-suspend-stack/characterize.mjs \
  --engine wasm --case scalar-non-tail --depth 8000
```

`--engine` accepts `native`, `wasm`, or `evaluator`. `--case` accepts `scalar-non-tail`,
`box-build`, `box-walk`, or `box-drop`. Add `--expect completed`, `--expect
host-stack-exhaustion`, or another reported outcome kind to make one invocation an assertion.

Each invocation prints one JSON record containing the engine, case, depth, outcome, elapsed time,
and host versions. Stack thresholds are host properties: compare shapes within one pinned host and
record the host metadata instead of treating one machine's depth as universal.

## Local phase-attribution sample

Measured on arm64 Darwin with Node 26.5.0 and Homebrew clang 22.1.8:

| engine | case | depth | outcome |
|---|---|---:|---|
| Wasm | scalar non-tail | 1,000 | completed |
| Wasm | scalar non-tail | 8,000 | host stack exhaustion (`RangeError`) |
| Wasm | Box build | 1,000 | host stack exhaustion (`RangeError`) |
| Wasm | Box walk | 1,000 | completed |
| Wasm | Box drop | 1,000 | completed |
| Wasm | Box walk | 4,000 | host stack exhaustion (`RangeError`) |
| Wasm | Box drop | 4,000 | host stack exhaustion (`RangeError`) |
| native release | Box build | 1,000 | completed |
| native release | Box walk | 1,000 | completed |
| native release | Box drop | 1,000 | completed |

This sample attributes the earliest local Wasm failure to recursive Effect construction, while
also confirming that ordinary recursive walk and recursive Drop have independent later limits.
The Linux/Node 22 `unreachable` reported at a walk depth near 1,000 did not reproduce on this host;
it remains a separate environment-sensitive defect rather than an `Effect.suspend` acceptance
criterion.
