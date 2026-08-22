## Why

**Backend.ts** is a 9,207-line file containing an ~8,200-line emitProgram Effect.gen (lines 933–9167) whose ~50 nested closures all capture one builder context, so no lowering is unit-testable in isolation and scalar arithmetic/binary lowering is duplicated byte-for-byte two to three times. The same file also hosts the shared cross-backend contract that **WasmBackend** imports by name; six near-identical lane-pointer helpers coexist; and emitProgram is authored as a generator-returning arrow with its error channel pinned only by tag-string probing. **WasmBackend.ts** is a 6,915-line file whose emitOperation is a 3,628-line god-function with four near-duplicate CleanupPlan walks and triplicated memory-grow/zero logic.

## What Changes

- **Give the shared cross-backend contract one final actor** — `Backend.ts` owns Backend, BackendError, Artifact, CodegenRequest, ControlProvenance, terminationOf, formatModuleViolations, symbolFor, suspensionPointKey, and line-table helpers. `LlvmBackend.ts` owns only LLVM lowering, all callers import the correct actor directly, and no compatibility re-export remains. Target-neutral MIR linearization moves beside Mir.
- **Split Backend emitProgram** into NativeType / NativeDeclare / NativeDebug / NativeFunction / NativeCall / NativeArith / NativeAggregate / NativeOperation / NativeControl / NativeSuspension, driven by an explicit LoweringContext record.
- **Deduplicate scalar arithmetic** (the byte-identical comparisonPredicates table and checked overflow/div/range blocks) into a single NativeArith seam; merge the six lane-pointer helpers into one.
- **Move the transcendental sin/cos/tan kernel** beside Transcendental.plan.
- **Split WasmBackend.ts** into WasmMemory / WasmCleanup / WasmSuspension / WasmLanes over a WasmEmitContext; each emitOperation case becomes a sibling function.
- **Deduplicate the four CleanupPlan walks** into one emitCleanupWalk, and the memory-grow idiom and zero-constant dispatch into single helpers.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

<!-- none: behavior-preserving refactor (skip_specs); emitted LLVM IR and wasm bytes stay identical -->

## Impact

Pure refactor of both backends. Emitted IR/wasm must stay byte-identical (golden backends + native/wasm differential corpus are the net). skip_specs: true.
