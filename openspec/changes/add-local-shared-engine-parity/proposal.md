## Why

Local shared ownership is a runtime ownership contract, so semantic agreement without evaluator,
native, and direct-Wasm agreement would leave cleanup and alias safety target-dependent. This slice
pins the complete observable transition system across all three engines.

Source: [SLP-0002, revision 6](../../../proposals/0002-allocation-backed-local-shared-ownership/proposal.md),
SHA-256 `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`,
realization slice 5 of 6. Depends on the first four SLP-0002 handoff changes.

## What Changes

- Execute caller-funded control-block initialization, clone, access, conflict, and drop in the evaluator.
- Lower the same verified MIR transitions in native LLVM and direct WebAssembly.
- Require count checks before mutation, count/access independence, non-mutating conflict observation,
  and `T` cleanup before allocation release on every engine.
- Pin deterministic observable parity while allowing target-specific byte layouts and reclaim representations.
- Preserve the specified strong-cycle leak and general no-unwind trap behavior.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-mir`: verify target-neutral local-shared operations and reject malformed ownership, provenance, and callback contracts before execution.
- `bootstrap-evaluation`: make evaluation the deterministic local-shared transition and cleanup oracle.
- `bootstrap-backend`: require native LLVM and direct Wasm to realize the verified transition system identically.

## Impact

This affects evaluator values and traces, MIR verification, LLVM lowering, direct-Wasm lowering,
runtime support, and the differential acceptance corpus. It adds no scheduler, garbage collector,
background task, atomics, or target-observable control-block representation.
