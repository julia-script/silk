## Why

Before adding suspension to Silk's source-defined Effect library, we need measured evidence about how today's synchronously completing composition compares with imperative `Result` control flow across both native LLVM and direct Wasm. This establishes the zero-cost baseline that a future complete-or-suspended runner must preserve. Without it, we could either add a compiler pass that LLVM already makes unnecessary or later make low-level programs pay for suspension machinery they cannot reach.

## What Changes

- Add a matched corpus of public Silk programs comparing imperative or `Result`-style control flow with direct, grouped, and stored library-defined Effect pipelines.
- Capture deterministic HIR, MIR, unoptimized and optimized LLVM IR, native assembly and code-size evidence, plus direct-Wasm WAT and binary-size evidence.
- Verify that pipe syntax itself disappears before MIR, separating pipe lowering from any remaining Effect abstraction cost.
- Classify retained costs such as heap allocations, indirect runner calls, wrapper environments, and intermediate Effect values, and verify that the current synchronous representation introduces no scheduler, fiber, continuation, or suspension dispatch.
- Produce a research report recommending one of three outcomes: rely on existing backend optimization, add a shared MIR normalization, or specialize the runner ABI for effects proven not to suspend.
- Keep this change evidence-only. If the measurements justify an optimizer or ABI change, describe that work in a separate follow-up proposal.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

None.

## Impact

This change adds research fixtures, artifact-capture tooling, structural assertions, and a Wayfinder research report. It depends on `make-effects-library-definable` providing the representative source-defined Effect combinators and compiler core to measure. It does not implement or simulate suspension, and it does not change the language, standard-library contract, runtime behavior, or public APIs.
