# Handoff: finish the Effect runner execution-contract fix

## Checkout and scope

- Repository: `julia-script/silk`
- Checkpoint branch: `julia/http1-wip-checkpoint-2026-09-13`
- Base before the checkpoint: `359926dc` (`julia/http1-stack-08-known-provider-inference`, PR #438)
- Work only on the compiler prerequisite and its focused regressions. Do not mix the buffered I/O,
  socket, TLS, content-decoding, or server implementation into the eventual compiler stack commit.

The immediate objective is to make this single regression pass with strict MIR verification:

```sh
pnpm --filter @silklang/compiler exec vitest run test/HttpContent.test.ts \
  -t "keeps provided Effect.result success shapes distinct across recovery families" \
  --maxWorkers=1
```

Do not start with a broad suite. When the regression is green, run the reduced positive server
case, the focused `ConditionalConformance` file, and only then any broader ticket-local checks.

## Problem and evidence

One lexical `Effect.result` / `catchAll` site is realized under two different semantic outcomes:

- content path: a `ContentProgress` / `ContentError` result family;
- callback path: an `i32` success with a callback/content/allocation failure union.

They are ABI-equivalent in places, so runtime-key or calling-shape matching reused the wrong runner.
The unsafe `sameCallingShape` fallback has been removed. Exact call-result provenance now comes from
the selected `Instances.CallInstance`, and generic identity lookup remains exact-only.

The last run before the current execution-contract rewrite passed call-result lowering, runner
emission, calling-shape root semantics, and provisional-control correlation, then failed because
re-lowered contextual runner variants imported caller-local lifetime/access facts into a physical
runner body. The first failure was an `InvalidCallShape` in
`MemoryByteDuplex.make$effect$-1$provided$188`: `Vector.append` saw `_local70` versus `_local64`, and
the returned Effect saw invocation-local access/lifetimes versus the discovered physical contract.
That established that contextual runner cloning was the wrong layer.

## Intended greenfield model

The saved implementation now follows this model:

1. `Mir.EffectValue.type` retains the exact call-site semantic view.
2. A runner execution contract is exact over success, failure row, and requirement row.
3. Only the outer Effect access and executable lifetime proof metadata are erased for runner
   selection. They govern use of the Effect value, not the machine implementing its execution.
4. Provider specialization may remove only exact requirement-row members authorized by capability,
   role, and `requirementAccess`. Provider type, witness, and runtime access remain in the exact
   provided-runner identity and verifier checks.
5. Physical environment/site, runner declaration, runtime type arguments, static arguments, and
   provider specialization remain exact. Never fall back to ABI shape or `runtimeKey` alone.
6. Zero-provider call-site variants use the already-discovered physical runner. Provided wrappers
   remain distinct, but are lowered from the authoritative physical base type/environment rather
   than from caller-local proof metadata.

The central actor is `packages/compiler/src/internal/EffectExecutionContract.ts`. The main consumers
are `ValueType.ts`, `Mir.ts`, `MirVerification.ts`, `EffectLowering.ts`, `SuspensionMir.ts`,
`NativeEffectOperation.ts`, `NativeExecutionOperation.ts`, and `NativeSuspension.ts`.

`Layout.callingShape` deliberately returns a root whose `.type` is the exact requested semantic type
while reusing the runtime-canonical tree and lanes. Keep that: it fixed the isolated
`provided$202` success-shape mismatch without changing the ABI.

## Current unverified edits

After the model change, only this cheap test was run and passed:

```text
Type.test.ts -t "keys Effect execution by exact channels"
1 passed, 63 skipped, 4.26s
```

The authoritative HttpContent collision regression has **not** been rerun after the final saved
changes. Immediately before checkpointing, the following additional review fixes were made:

- `Mir.matchesEffectInstance` uses the centralized execution relation and distinguishes an omitted
  provider list from an explicitly empty provider list.
- direct MIR-verifier and native static-run checks use the same centralized relation;
- deferred native suspension selection now checks the exact Effect execution outcome;
- `ProvisionalMir.runnerOf` selects the semantic environment by exact A/E/R plus exact provider
  subtraction;
- effect-block, catch, builtin, witness, and represented Effect environment selection now requires
  the exact requested semantic contract instead of taking the first same-site candidate.

These edits were formatted and `git diff --check` was clean before the handoff files were added, but
they still require the focused regression above.

## Next diagnostic loop

1. Run the one HttpContent regression command above.
2. If red, stop and classify only the first failure. Capture:
   - enclosing MIR function id/instance;
   - operation runner id, type args/statics, requested outcome, and providers;
   - candidate runner result, `effectRunner.base`, and providers;
   - selected physical `Layout.EffectEnvironment` identity and exact A/E/R.
3. Fix the producer/selector that loses one of those exact facts. Do not weaken
   `EffectExecutionContract`, `Mir.matchesEffectInstance`, MIR verification, or ownership checks.
4. Rerun only the same regression until it passes.
5. Then run the reduced positive server realization, the focused
   `packages/compiler/test/ConditionalConformance.test.ts`, and the focused Type/Layout/Provisional
   unit assertions. Avoid the full native corpus until those are green.

## Stack publication

Once coherent, extract only the compiler actor, compiler tests, and
`openspec/changes/support-conditional-effect-witness-inference/` into
`julia/http1-stack-09-conditional-witness-runners`, based on PR #438. Open it as the new top stacked
PR. Intermediate stack PRs may be red; only the newest top PR's exact head is the CI gate.
