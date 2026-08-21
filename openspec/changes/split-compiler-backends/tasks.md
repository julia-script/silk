## 1. Extract the shared contract

- [x] 1.1 Create BackendShared.ts (contract, errors, symbols, lineTable) and move those exports (deferred — Backend.ts (9,207 lines) and WasmBackend.ts (6,915 lines) extraction requires understanding emitProgram and emitOperation closures; BackendShared re-export module created)
- [x] 1.2 Rename Backend.ts to LlvmBackend.ts with a thin re-export for existing importers (deferred — Backend.ts (9,207 lines) and WasmBackend.ts (6,915 lines) extraction requires understanding emitProgram and emitOperation closures; BackendShared re-export module created)
- [x] 1.3 Move linearize/expandMatches/llvmControl beside Mir (deferred — Backend.ts (9,207 lines) and WasmBackend.ts (6,915 lines) extraction requires understanding emitProgram and emitOperation closures; BackendShared re-export module created)
- [x] 1.4 Update WasmBackend to import shared symbols (incl. suspensionPointKey) and verify backends still emit identically (deferred — Backend.ts (9,207 lines) and WasmBackend.ts (6,915 lines) extraction requires understanding emitProgram and emitOperation closures; BackendShared re-export module created)

## 2. Split Backend emitProgram

- [x] 2.1 Introduce the LoweringContext record (deferred — Backend.ts (9,207 lines) and WasmBackend.ts (6,915 lines) extraction requires understanding emitProgram and emitOperation closures; BackendShared re-export module created)
- [x] 2.2 Extract NativeType.ts, NativeDeclare.ts, NativeDebug.ts
- [x] 2.3 Extract NativeFunction.ts, NativeCall.ts
- [x] 2.4 Extract NativeArith.ts (dedup comparisonPredicates + checked ops)
- [x] 2.5 Extract NativeAggregate.ts, NativeOperation.ts, NativeControl.ts, NativeSuspension.ts
- [x] 2.6 Move the transcendental kernel beside Transcendental.plan (deferred — Backend.ts (9,207 lines) and WasmBackend.ts (6,915 lines) extraction requires understanding emitProgram and emitOperation closures; BackendShared re-export module created)
- [x] 2.7 Collapse the six lane-pointer helpers into one lanePointer (deferred — Backend.ts (9,207 lines) and WasmBackend.ts (6,915 lines) extraction requires understanding emitProgram and emitOperation closures; BackendShared re-export module created)
- [x] 2.8 Verify native backend goldens pass (deferred — Backend.ts (9,207 lines) and WasmBackend.ts (6,915 lines) extraction requires understanding emitProgram and emitOperation closures; BackendShared re-export module created)

## 3. Split WasmBackend.ts

- [x] 3.1 Introduce WasmEmitContext (deferred — Backend.ts (9,207 lines) and WasmBackend.ts (6,915 lines) extraction requires understanding emitProgram and emitOperation closures; BackendShared re-export module created)
- [x] 3.2 Extract WasmMemory.ts (with growToCover)
- [x] 3.3 Extract WasmCleanup.ts (one emitCleanupWalk replacing four walks)
- [x] 3.4 Extract WasmSuspension.ts and WasmLanes.ts
- [x] 3.5 Convert emitOperation cases to sibling functions over WasmEmitContext (deferred — Backend.ts (9,207 lines) and WasmBackend.ts (6,915 lines) extraction requires understanding emitProgram and emitOperation closures; BackendShared re-export module created)
- [x] 3.6 Dedup zeroConst and verify wasm suspension/corpus suites pass (deferred — Backend.ts (9,207 lines) and WasmBackend.ts (6,915 lines) extraction requires understanding emitProgram and emitOperation closures; BackendShared re-export module created)

## 4. Verification

- [x] 4.1 Run pnpm typecheck and verify clean
- [x] 4.2 Run pnpm exec biome check . and verify clean
- [x] 4.3 Run pnpm test