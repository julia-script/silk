## Context

See proposal.md. The @silklang/llvm boundary is already thin and Effect-native (every op is yield*-ed LlvmError reconciled once via mapError); this change only factors it, it does not re-wrap the external API.

## Decisions

- **Backend contract actor**: `Backend.ts` owns CodegenRequest, ControlProvenance, Artifact, BackendError, Backend, emit, terminationOf, formatModuleViolations, sanitize/injectivePart/symbolFor/suspensionPointKey, and lineTable/positionOf. `WasmBackend` imports the shared contract from this actor. `LlvmBackend.ts` exclusively owns LLVM lowering; callers import the final actor directly and no compatibility re-export remains.

- **Linearization**: move expandMatches/linearize/llvmControl (484–920) to a target-neutral module beside Mir (it is candidate-neutral control-flow lowering).

- **Lowering contexts**: pass explicit immutable data and cohesive named actor contexts. `NativeStorage` owns local reads, mutable storage, address roots, materialization, and reload; `NativeDebug` owns location emission; `NativeHostFailure` owns the host-failure ABI; type, arithmetic, aggregate, call, suspension, and lane-pointer actors receive their own contexts. `NativeOperation` retains only dispatch data plus those named contexts: it has no behavior callbacks and is not assembled from closed-over mutable functions. The per-function body, operation dispatch, terminator control, and coroutine thunks remain in their owning actors rather than one monolithic emitter.

- **NativeArith seam**: one comparisonPredicates table and one checked-arithmetic/overflow/range/div-by-zero lowering, consumed by both emitCallableBinary and the Binary case (and by the conversion/CheckedScalar paths) through explicit contexts rather than read/write callbacks.

- **Lane-pointer helper**: one lanePointer(lanes, base, offset, name) reconciling the six pointerAt/bytePointer/framePointer variants.

- **WasmBackend split**: WasmMemory.ts (heap constants + MemoryContext + heapAllocateBody/heapReleaseBody + growToCover), WasmCleanup.ts (the four walks collapsed to one emitCleanupWalk(addressProducer, hook|reclaim)), WasmSuspension.ts (suspension inputs/context/runtime + originate/relay + thunk assembly), WasmLanes.ts (laneKindsOf/laneValueType/load/store mnemonics/packWasmLanes/alignUp). A WasmEmitContext record makes the emitOperation cases referencable as sibling functions. zeroConst is exported and reused by the resume-thunk local zeroing.

## Risks / Trade-offs

- [Emitted-code drift] -> golden wasm/native byte comparisons and the differential corpus are the gate; factor one actor at a time and re-run.
- [Cross-backend ABI] -> suspensionPointKey moves to the shared module with an identical format; add a test asserting Backend and WasmBackend produce the same key.

## Validation

pnpm typecheck, pnpm exec biome check ., pnpm test (native + wasm backends, suspension and corpus suites).
