# Static runner CFG inlining spike

**Date:** 2026-08-10  
**Disposition:** named prerequisite — static runner delegation summaries

## Decision

Do not propose general runner CFG inlining yet, and do not close the remaining direct-Wasm call as
backend-only overhead. First add a shared analysis/normalization prerequisite that summarizes and
collapses a one-region, one-return static runner whose only effectful step delegates to another
`RunStaticEffect`. Then rerun this classifier against the exposed runner. A CFG-inlining proposal is
justified only when that rerun finds a useful closed, acyclic, loan-free, cleanup-free, non-affine
runner with one typed exit map.

This is not a semantic objection to source-defined Effect composition. Clang `-O2` already reduces
the measured native entries to their imperative shapes. It is a proof-boundary decision for shared
MIR and the unoptimized direct-Wasm backend.

## Evidence

The deterministic cost harness follows every `RunStaticEffect` identity to its concrete MIR
function. Across seven normalized Effect cases it finds 13 roots:

| Case | Roots | Runner sizes (regions + operations + outcomes) | Direct-Wasm entry calls |
| --- | ---: | --- | ---: |
| map | 1 | 15 | 1 |
| mapBoth success | 2 | 4, 14 | 1 |
| mapBoth failure | 2 | 4, 14 | 1 |
| flatMap | 1 | 16 | 1 |
| affine imperative | 3 | 4, 14, 22 | 1 |
| affine Effect | 3 | 4, 14, 15 | 1 |
| trap | 1 | 15 | 1 |

No root fits the synthetic prototype vocabulary. Four roots are minimal forwarding runners: one
operation region, one return, `RunStaticEffect`, and `PackEffectOutcome`. They are blocked only by
nested static execution, which is the named prerequisite above. The remaining useful composition
runners require broader semantics:

- 9 roots contain cleanup and affine operations;
- 8 contain nested matches and dynamically applied callables;
- 12 execute another Effect inside the runner;
- 9 use a structured or cleanup region.

No classified runner is recursive and none uses an explicit MIR loan. Estimated cloned sizes range
from 4 to 22 nodes, so size is not the blocker. Typed exit, nested-effect, callable, and ownership
semantics are.

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
- LLVM optimization success is performance evidence, not proof that shared MIR cloning is safe.

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
