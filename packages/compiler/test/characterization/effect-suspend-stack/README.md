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
pnpm --filter @silklang/compiler build
node packages/compiler/test/characterization/effect-suspend-stack/characterize.mjs \
  --engine wasm --case scalar-non-tail --depth 8000
```

`--engine` accepts `native`, `wasm`, or `evaluator`. `--case` accepts `scalar-non-tail`,
`box-build`, `box-walk`, or `box-drop`. Add `--expect completed`, `--expect
host-stack-exhaustion`, or another reported outcome kind to make one invocation an assertion.

Each invocation prints one JSON record containing the engine, case, depth, outcome, elapsed time,
and host versions. Stack thresholds are host properties: compare shapes within one pinned host and
record the host metadata instead of treating one machine's depth as universal.

## Pinned-host Box recursive-phase matrix

Measured twice on 2026-08-13 at Git revision
`3145e5a4ef1612de29a660e425fddf78104ccd9e` with:

- Apple M1 Max, arm64 macOS 15.5 (24F74);
- Node.js 26.5.0;
- pnpm 11.10.0; and
- Homebrew Clang/LLVM 22.1.8 targeting `arm64-apple-darwin24.5.0` for native release.

The values below are reproducible tested brackets on this host, not exact maximum depths or
portable thresholds.

| engine         | isolated recursive phase | deepest tested completion | first tested failure | failure classification               |
| -------------- | ------------------------ | ------------------------: | -------------------: | ------------------------------------ |
| direct Wasm    | Effect + Box build       |                       725 |                  750 | host stack exhaustion (`RangeError`) |
| direct Wasm    | borrowed Box walk        |                     2,000 |                3,000 | host stack exhaustion (`RangeError`) |
| direct Wasm    | recursive Box Drop       |                     3,000 |                4,000 | host stack exhaustion (`RangeError`) |
| native release | Effect + Box build       |                    32,000 |               48,000 | `SIGSEGV`                            |
| native release | borrowed Box walk        |                   100,000 |              320,000 | `SIGSEGV`                            |
| native release | recursive Box Drop       |                   100,000 |              320,000 | `SIGSEGV`                            |

The matrix was produced after `pnpm --filter @silklang/compiler build`. Each Wasm row used:

```sh
node packages/compiler/test/characterization/effect-suspend-stack/characterize.mjs \
  --engine wasm --case <box-build|box-walk|box-drop> --depth <depth> --expect <outcome>
```

Each native row used the same command shape with:

```sh
SILK_EFFECT_STACK_CLANG=/opt/homebrew/opt/llvm/bin/clang \
node packages/compiler/test/characterization/effect-suspend-stack/characterize.mjs \
  --engine native --case <box-build|box-walk|box-drop> --depth <depth> --expect <outcome>
```

This attributes the earliest local failure to recursive Effect construction while confirming two
independent ordinary-recursion limits: borrowed traversal and automatic recursive Drop. Those
ordinary recursion defects are tracked separately under `.scratch/effect-suspension-boundary/` and
are not `Effect.suspend` acceptance criteria.

The borrow-heavy Wasm `unreachable` reported on Linux/Node 22 near depth 1,000 did not reproduce on
this host: the borrowed walk completed at 2,000 and later raised a host `RangeError` at 3,000. It is
tracked separately as an environment-sensitive compiler-correctness defect rather than being
folded into the suspension boundary.
