## 1. Extract the shared contract

- [x] 1.1 Make `Backend.ts` the final shared contract actor (contract, errors, symbols, line table)
- [x] 1.2 Move LLVM lowering to `LlvmBackend.ts`, update every caller, and retain no compatibility re-export
- [x] 1.3 Move linearize/expandMatches/llvmControl beside Mir
- [x] 1.4 Update WasmBackend to import shared symbols (incl. suspensionPointKey) and verify backends still emit identically

## 2. Split Backend emitProgram

- [x] 2.1 Introduce the LoweringContext record
- [x] 2.2 Extract NativeType.ts, NativeDeclare.ts, NativeDebug.ts
- [x] 2.3 Extract NativeFunction.ts, NativeCall.ts
- [x] 2.4 Extract NativeArith.ts (dedup comparisonPredicates + checked ops)
- [x] 2.5 Extract NativeAggregate.ts, NativeOperation.ts, NativeControl.ts, NativeSuspension.ts
- [x] 2.6 Move the transcendental kernel beside Transcendental.plan
- [x] 2.7 Collapse the six lane-pointer helpers into one lanePointer
- [x] 2.8 Verify native backend goldens pass

## 3. Split WasmBackend.ts

- [x] 3.1 Introduce WasmEmitContext
- [x] 3.2 Extract WasmMemory.ts (with growToCover)
- [x] 3.3 Extract WasmCleanup.ts (one emitCleanupWalk replacing four walks)
- [x] 3.4 Extract WasmSuspension.ts and WasmLanes.ts
- [x] 3.5 Convert emitOperation cases to sibling functions over WasmEmitContext
- [x] 3.6 Dedup zeroConst and verify wasm suspension/corpus suites pass

## 4. Verification

- [x] 4.1 Run pnpm typecheck and verify clean
- [x] 4.2 Run pnpm exec biome check . and verify clean
- [x] 4.3 Run pnpm test

## 5. Convergence findings

- [x] 5.1 Introduce the explicit native `LoweringContext` and move the actual type, declaration, debug, function, call, arithmetic, aggregate, operation, control, and suspension lowering implementations into their `Native*` actors
- [x] 5.2 Make `LlvmBackend.emitProgram` a thin coordinator and verify native IR/bitcode goldens and differential execution remain unchanged
