# Synchronous Effect cost spike

Status: measured 2026-08-10. This is implementation evidence, not a language guarantee.

## Decision

Propose a guarded, backend-independent MIR normalization for statically known synchronous Effect
composition.

Clang 22.1.8 reduces every measured native `silk_main` to the same LLVM and AArch64 control shape as
its imperative baseline, including stored composition, typed failure, requirement provision,
traps, and allocator-backed affine cleanup. The direct WebAssembly backend retains source-defined
combinator calls and substantially larger modules. A pipe rewrite would target syntax that is
already gone before MIR, while a synchronous runner ABI is premature because Silk has no
suspension representation yet. The smallest justified intervention is therefore a shared MIR
normalization guarded by statically selected runners and callbacks, non-escaping Effect values,
and preserved failure, requirement, trap, ownership, and cleanup behavior.

The normalization should initially target only cases this corpus proves. Dynamic callables,
escaping Effect values, reusable mutable Effects, and future suspension remain unspecialized.

## Implemented follow-up

The first shared slice shipped on 2026-08-10. It folds direct functions whose complete body is one
`MakeEffect` and return, then replaces a same-region, single-use Copy/shared environment plus
`RunEffectValue` with `RunStaticEffect`. The evaluator, LLVM, and direct-Wasm backends consume that
same operation. MIR records accepted and first-rejection verdicts; the labs expose their counts.

This intentionally stops before runner CFG inlining. Entry-local WAT showed that constructor and
environment round trips can be removed without cloning control flow, while eliminating the final
runner call would require typed-exit, region, and affine-cleanup remapping. Stored/provider shapes
therefore receive constructor folding but may retain a value run; affine/exclusive captures and
unknown suspension are rejected.

The deterministic harness now emits normalized and explicitly unnormalized behavior and WAT entry
structure. Selected entry results are:

| Case | unnormalized calls | normalized calls | folded constructors | direct static runs |
| --- | ---: | ---: | ---: | ---: |
| map Effect | 3 | 1 | 2 | 1 |
| mapBoth success Effect | 4 | 1 | 4 | 2 |
| mapBoth failure Effect | 4 | 1 | 4 | 2 |
| flatMap Effect | 3 | 1 | 2 | 1 |
| provide + generic adapter Effect | 4 | 2 | 2 | 0 |
| stored maps Effect | 4 | 1 | 3 | 0 |
| affine allocation Effect | 3 | 1 | 5 | 3 |
| trap Effect | 3 | 1 | 2 | 1 |

The tiny whole-module Wasm size reduction is intentionally not presented as dead-code elimination:
specialized constructors and runners remain exported. Native optimized entry structure remains
unchanged, and normalized/unnormalized evaluator and Wasm behavior agree. The affine pair retains
the same Drop count on both paths.

## Reproduction

Baseline compiler commit: `6e566f6914540788f70b0faf245d38d3ab5929f5`.

```sh
pnpm --filter @silk-effect/compiler build
node packages/compiler/test/fixtures/synchronous-effect-cost.mjs > /tmp/silk-effect-cost.json
pnpm --filter @silk-effect/compiler exec vitest run test/SynchronousEffectCost.test.ts
```

To retain the complete normalized artifacts instead of only the JSON structural summary:

```sh
artifact_dir="$(mktemp -d)"
SILK_EFFECT_COST_ARTIFACT_DIR="$artifact_dir" \
  node packages/compiler/test/fixtures/synchronous-effect-cost.mjs \
  > "$artifact_dir/report.json"
```

Each case directory then contains HIR, MIR, debug and release compiler-emitted LLVM IR, Clang
`-O2` LLVM IR, AArch64 assembly, LLVM bitcode, direct-backend WAT, and Wasm bytes. The committed
test runs the harness twice in fresh processes and requires byte-identical normalized JSON.

Measured environment:

- macOS 15.5 on arm64;
- Node 26.5.0 and pnpm 11.10.0;
- Homebrew Clang, LLVM, and `llvm-dis` 22.1.8;
- wasm-tools 1.255.0; and
- wasm2wat 1.0.41.

The compiler targets were `aarch64-apple-darwin` and `wasm32-unknown-unknown`; compiler codegen and
Clang used release/`-O2` mode for the final comparisons.

## Corpus and semantic gates

Nine matched pairs cover pure pipe syntax, `map`, successful and failing `mapBoth`, `flatMap`, a
generic requirement adapter plus `provide`, stored multi-map composition, allocation-backed affine
transport, and a trap. Before artifact comparison, every completing pair must agree between the
MIR evaluator and instantiated direct Wasm. The trap pair must trap in both. The affine pair must
allocate through `SystemAllocator`, return `42`, and execute one `Payload` Drop hook on each side.

The failure case proves short-circuiting by translating `Problem` only through the failure callback
before recovery. The stored case makes order observable as `addOne` followed by `double`. The trap
case proves that `Effect.map` does not turn a division trap into typed failure. The provision case
crosses a named generic Effect helper before satisfying its requirement.

One additional probe exposed a separate ownership limitation: capturing an allocation-backed
`Payload` in an Effect, returning that same payload, and then mapping it currently attempts to
consume its reclaim ticket twice. The measured affine pair allocates the payload inside the runner,
then moves the result through `Effect.map`; this preserves the intended allocation and cleanup
pressure while keeping capture-transfer repair outside this evidence-only spike.

## Results

The table reports the final `silk_main` AArch64 shape, total normalized AArch64 assembly text, and
the direct backend's whole-module WAT call count and Wasm byte size. Assembly text size is a
structural code-size proxy, not an executable-size or timing promise.

| Pair | Form | entry calls | entry branches | entry asm bytes | module asm bytes | WAT calls | Wasm bytes |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| pure pipe | imperative | 0 | 0 | 169 | 1,251 | 1 | 285 |
| pure pipe | piped | 0 | 0 | 169 | 1,191 | 1 | 270 |
| map | imperative | 0 | 0 | 169 | 1,239 | 1 | 282 |
| map | Effect | 0 | 0 | 169 | 13,656 | 7 | 3,277 |
| mapBoth success | imperative | 0 | 0 | 169 | 1,395 | 1 | 321 |
| mapBoth success | Effect | 0 | 0 | 169 | 68,044 | 14 | 16,585 |
| mapBoth failure | imperative | 0 | 0 | 169 | 1,775 | 1 | 420 |
| mapBoth failure | Effect | 0 | 0 | 169 | 70,234 | 14 | 17,023 |
| flatMap | imperative | 0 | 0 | 169 | 1,299 | 1 | 297 |
| flatMap | Effect | 0 | 0 | 169 | 17,336 | 8 | 4,141 |
| provide + generic adapter | imperative | 0 | 0 | 169 | 1,644 | 1 | 481 |
| provide + generic adapter | Effect | 0 | 0 | 169 | 17,079 | 5 | 4,185 |
| stored maps | imperative | 0 | 0 | 169 | 2,256 | 2 | 548 |
| stored maps | Effect | 0 | 0 | 169 | 25,211 | 12 | 5,900 |
| affine allocation | imperative | 2 | 2 | 1,826 | 37,846 | 12 | 8,364 |
| affine allocation | Effect | 2 | 2 | 1,826 | 54,508 | 16 | 12,129 |
| trap | imperative | 0 | 0 | 129 | 1,247 | 1 | 260 |
| trap | Effect | 0 | 0 | 129 | 13,132 | 7 | 3,164 |

No measured HIR or MIR contains a `|>` token. No MIR, optimized LLVM entry, assembly, or WAT
contains active suspension dispatch, scheduler, fiber, or Effect/runtime continuation machinery.
The MIR `suspended=false` loan field is ordinary borrow metadata and is deliberately not classified
as an async state.

## Cost classification

| Observation | First visible | Native result | Direct-Wasm result |
| --- | --- | --- | --- |
| Pipe spelling | syntax only | absent by HIR/MIR | absent by HIR/MIR |
| Source combinator calls and Result matches | HIR/MIR | folded out of `silk_main` by Clang `-O2` | retained as calls and branches |
| Known provider adaptation | HIR/MIR | folded out of `silk_main` | retained across five WAT calls total |
| Stored Effect wrappers | HIR/MIR | folded out of `silk_main` | retained across twelve WAT calls total |
| Affine Result transport and cleanup | HIR/MIR | same entry calls, branches, and Drop count | same behavior and Drop count, with four additional module calls |
| Suspension/fiber machinery | nowhere | absent | absent |
| Extra execution-time heap allocation solely for composition | nowhere proven | none in entry structure | none beyond the pair's shared runtime needs |

LLVM is sufficient for native hot-path simplification, but it does not eliminate externally visible
specialized helper bodies from the emitted module. Direct Wasm has no corresponding optimizer and
therefore pays both structural and code-size costs. Timings were not collected: these fixtures are
too small for stable distributions, and structural evidence answers the design question directly.

## Recommendation and guards

The shipped constructor/static-dispatch slice normalizes only when all of the following are compiler
facts:

- the Effect runner and callback targets are statically selected;
- the Effect value does not escape the normalized region;
- execution is synchronous under today's runtime (and later is proven not to suspend);
- failure and requirement rows are concrete enough to preserve branch and provider behavior;
- callback evaluation order and invocation count remain unchanged;
- trap behavior remains outside typed failure; and
- ownership proves every moved value and cleanup obligation has exactly one destination.

The pass runs on shared MIR before evaluation and LLVM/direct-Wasm emission. It does not recognize
`Effect.map`, `|>`, the standard-library module, or any declaration spelling. Its proof target is
the generic constructor/call/run shape produced by ordinary source-defined combinators. Full
runner/call/result CFG normalization remains a separate proposal.

When genuine suspension is implemented, rerun this corpus with two additions: one suspending control
whose continuation must survive, and one statically non-suspending case that must remain equal to
this baseline. Until then, a complete-or-suspended ABI or `MaySuspend` specialization would be based
on synthetic evidence and is not justified.

## Limitations

- The direct-Wasm figures describe the current backend's complete emitted module, not a separately
  optimized or linked artifact.
- Assembly text size includes externally visible specialized functions that may not execute from
  `silk_main`; it intentionally exposes code-size retention separately from entry hot-path shape.
- The corpus does not cover dynamic callable selection, escaping Effects, reusable mutable Effects,
  retry loops, or suspension.
- The affine-capture double-reclaim probe needs its own semantic repair and regression before that
  source shape can join the cost corpus.
