## 1. Extract the shared contract

- [ ] 1.1 Create BackendShared.ts (contract, errors, symbols, lineTable) and move those exports
- [ ] 1.2 Rename Backend.ts to LlvmBackend.ts with a thin re-export for existing importers
- [ ] 1.3 Move linearize/expandMatches/llvmControl beside Mir
- [ ] 1.4 Update WasmBackend to import shared symbols (incl. suspensionPointKey) and verify backends still emit identically

## 2. Split Backend emitProgram

- [ ] 2.1 Introduce the LoweringContext record
- [ ] 2.2 Extract NativeType.ts, NativeDeclare.ts, NativeDebug.ts
- [ ] 2.3 Extract NativeFunction.ts, NativeCall.ts
- [ ] 2.4 Extract NativeArith.ts (dedup comparisonPredicates + checked ops)
- [ ] 2.5 Extract NativeAggregate.ts, NativeOperation.ts, NativeControl.ts, NativeSuspension.ts
- [ ] 2.6 Move the transcendental kernel beside Transcendental.plan
- [ ] 2.7 Collapse the six lane-pointer helpers into one lanePointer
- [ ] 2.8 Verify native backend goldens pass

## 3. Split WasmBackend.ts

- [ ] 3.1 Introduce WasmEmitContext
- [ ] 3.2 Extract WasmMemory.ts (with growToCover)
- [ ] 3.3 Extract WasmCleanup.ts (one emitCleanupWalk replacing four walks)
- [ ] 3.4 Extract WasmSuspension.ts and WasmLanes.ts
- [ ] 3.5 Convert emitOperation cases to sibling functions over WasmEmitContext
- [ ] 3.6 Dedup zeroConst and verify wasm suspension/corpus suites pass

## 4. Verification

- [ ] 4.1 Run pnpm typecheck and verify clean
- [ ] 4.2 Run pnpm exec biome check . and verify clean
- [ ] 4.3 Run pnpm test