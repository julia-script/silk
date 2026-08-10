# Static runner CFG inlining spike

**Date:** 2026-08-10  
**Disposition:** backend-only closure — production optimization remains with LLVM

## Decision

Do not propose runner CFG inlining or static-runner delegation as current compiler work. Production
builds go through LLVM, and Clang `-O2` already reduces every measured Effect entry to its imperative
shape. The custom direct-Wasm backend exists primarily to keep Silk independent of external tools
and fully compilable in browser demos; matching LLVM's optimizer is not one of its current goals.

The remaining direct-Wasm runner call is therefore acceptable. If direct-Wasm performance or code
size later becomes important, these findings can seed a general MIR or backend optimization effort.
Collapsing one-region forwarding runners is one possible first optimization, but it is optional
future work rather than a prerequisite for Effect, the language, or production releases.

## Evidence

The deterministic cost harness follows every `RunStaticEffect` identity to its concrete MIR
function. Across seven separate normalized Effect cases it finds 13 roots. These are corpus
observations—including nested runners and repeated patterns—not 13 runtime calls in one program:

| Case | Roots | Runner sizes (regions + operations + outcomes) | Direct-Wasm entry calls |
| --- | ---: | --- | ---: |
| map | 1 | 15 | 1 |
| mapBoth success | 2 | 4, 14 | 1 |
| mapBoth failure | 2 | 4, 14 | 1 |
| flatMap | 1 | 16 | 1 |
| affine imperative | 3 | 4, 14, 22 | 1 |
| affine Effect | 3 | 4, 14, 15 | 1 |
| trap | 1 | 15 | 1 |

No root fits the deliberately small synthetic prototype vocabulary. Four roots are minimal
forwarding runners: one operation region, one return, `RunStaticEffect`, and `PackEffectOutcome`.
They could be collapsed by a future optimizer. The remaining composition runners require broader
semantics:

- 9 roots contain cleanup and affine operations;
- 8 contain nested matches and dynamically applied callables;
- 12 execute another Effect inside the runner;
- 9 use a structured or cleanup region.

No classified runner is recursive and none uses an explicit MIR loan. Estimated cloned sizes range
from 4 to 22 nodes. The spike therefore shows that a shared inliner would need typed-exit,
nested-effect, callable, and ownership semantics; it does not show that such an inliner is necessary
now or that Effect composition is expensive in production.

The test-only immutable remapper proves the mechanical subset. It deterministically renumbers local
and region identities, rewrites `Forward`, binds the sole `Return` value into a caller destination,
and forwards to a supplied continuation. It rejects unknown locals or regions, conditional/loop
regions, lexical loop exits, cleanup, cycles, and any graph without exactly one return. It cannot
reach compiler MIR or a backend.

## Limitations

- The classifier describes the current cost corpus, not every future Effect runner.
- Affine evidence counts explicit take/exclusive captures and move/drop operations; it is not a new
  ownership analysis.
- The prototype does not model nested `Match` operations, typed failure lanes, cleanup transfer,
  suspension, recursion, or callable-environment realization.
- LLVM optimization success applies to the measured corpus and current production path; it is not a
  universal performance guarantee for every future Effect program.
- Direct-Wasm optimization goals may change if browser demos eventually require smaller or faster
  artifacts.

## Reproduction

Build the compiler because the fixture imports its emitted ESM, then run the focused deterministic
classifier and remapper tests:

```sh
pnpm --filter @silk-effect/compiler build
pnpm --filter @silk-effect/compiler exec vitest run \
  test/StaticRunnerInliningPrototype.test.ts \
  test/SynchronousEffectCost.test.ts
```

To inspect the per-runner facts directly:

```sh
node packages/compiler/test/fixtures/synchronous-effect-cost.mjs \
  | jq '[.cases[] | select(.runners | length > 0) | { id, runners }]'
```
