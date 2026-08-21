## Context

See proposal.md. The @silk-effect/llvm boundary is already thin and Effect-native (every op is yield*-ed LlvmError reconciled once via mapError); this change only factors it, it does not re-wrap the external API.

## Decisions

- **Shared contract module** (e.g. BackendShared.ts, or Artifact.ts + Symbol.ts): move CodegenRequest, ControlProvenance, Artifact, BackendError, Backend, emit, terminationOf, formatModuleViolations, sanitize/injectivePart/symbolFor/suspensionPointKey, and lineTable/positionOf. WasmBackend imports symbolFor/suspensionPointKey/terminationOf/formatModuleViolations/ControlProvenance from here instead of from Backend.ts. Rename Backend.ts to LlvmBackend.ts; BackendRegistry and Pipeline import changes are mechanical.

- **Linearization**: move expandMatches/linearize/llvmControl (484–920) to a target-neutral module beside Mir (it is candidate-neutral control-flow lowering).

- **LoweringContext record**: capture { builder, types, program, layout, lanesFor, packedLanes, declared, mutableStorage } explicitly; each Native* actor is a pure function (ctx, ...) => ... with no closure capture. The per-function body, coerceLane, callValues, emitCallableBinary/emitIntegerConversion, Binary/CheckedScalar, aggregate/cleanup, the operation dispatch switch, terminator switch, and coroutine thunks each become one actor.

- **NativeArith seam**: one comparisonPredicates table and one checked-arithmetic/overflow/range/div-by-zero lowering, consumed by both emitCallableBinary and the Binary case (and by the conversion/CheckedScalar paths) with different read/write wrappers.

- **Lane-pointer helper**: one lanePointer(lanes, base, offset, name) reconciling the six pointerAt/bytePointer/framePointer variants.

- **WasmBackend split**: WasmMemory.ts (heap constants + MemoryContext + heapAllocateBody/heapReleaseBody + growToCover), WasmCleanup.ts (the four walks collapsed to one emitCleanupWalk(addressProducer, hook|reclaim)), WasmSuspension.ts (suspension inputs/context/runtime + originate/relay + thunk assembly), WasmLanes.ts (laneKindsOf/laneValueType/load/store mnemonics/packWasmLanes/alignUp). A WasmEmitContext record makes the emitOperation cases referencable as sibling functions. zeroConst is exported and reused by the resume-thunk local zeroing.

## Risks / Trade-offs

- [Emitted-code drift] -> golden wasm/native byte comparisons and the differential corpus are the gate; factor one actor at a time and re-run.
- [Cross-backend ABI] -> suspensionPointKey moves to the shared module with an identical format; add a test asserting Backend and WasmBackend produce the same key.

## Validation

pnpm typecheck, pnpm exec biome check ., pnpm test (native + wasm backends, suspension and corpus suites).