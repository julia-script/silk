## Why

**Mir.ts** (6,679 lines) mixes data with a ~2,580-line verify, a deterministic encoder, and test fixtures; **ProvisionalMir** and **Mir** re-declare the same suspension vocabulary. **BootstrapEvaluation.ts** (5,939 lines) mixes six concerns and repeats its scalar-arithmetic logic twice and its place traversal four times inline.

## What Changes

- **Split Mir.ts** into **Suspension** (suspension data types), **MirVerification** (verify + per-operation validators), and **MirEncoding** (encode + text helpers); Mir keeps data + accessors. (samples relocation is in the dead-code change.)
- **One shared suspension vocabulary**: Classification, Runner, Completion, and Provider defined once; SuspensionMir projects rather than re-types. Fix the dead operationArguments ternary.
- **Split BootstrapEvaluation.ts** into BootstrapValue/BootstrapTrace, BootstrapArithmetic, BootstrapPlace, BootstrapOsIntrinsics, BootstrapStorage, and BootstrapEffect, keeping executeMachine/evaluate as the coordinator.
- **Deduplicate arithmetic** (callable path vs MIR Binary/checked) and **place traversal** (four walkers with verbatim strings) into one BootstrapArithmetic + walkPlace.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

<!-- none: behavior-preserving refactor (skip_specs) -->

## Impact

Pure refactor. Mir.verify, Mir.encode, and evaluator Outcome/BlockedReason semantics stay byte-identical; the evaluator must remain a faithful oracle for the differential native/wasm checks. skip_specs: true.