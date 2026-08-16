# Baseline: packages/compiler test durations

Run 2026-08-15 on merge of origin/main (691092e), Apple Silicon, all cores.
Wall 184.0s; sum of per-file time 1673s; 233 files, 1852 tests.
Known failure (pre-existing on main): WasmShadowStackHeapCollision host-stack control.

| # | File | s | tests |
|---|------|---|-------|
| 1 | LexerPressure.test.ts | 143.3 | 6 |
| 2 | TemporaryDirectoryAcceptance.test.ts | 130.4 | 6 |
| 3 | StackVmPressure.test.ts | 82.3 | 5 |
| 4 | VectorAcceptance.test.ts | 62.9 | 20 |
| 5 | UnicodeNormalization.test.ts | 50.5 | 8 |
| 6 | DriverNativeAcceptance.test.ts | 48.5 | 1 |
| 7 | EffectSuspensionNative.test.ts | 45.7 | 5 |
| 8 | SynchronousEffectCost.test.ts | 41.3 | 1 |
| 9 | StoredCallableRuntime.test.ts | 38.7 | 5 |
| 10 | StoredCallableDeterminism.test.ts | 38.6 | 2 |
| 11 | OsFileSystem.test.ts | 37.9 | 10 |
| 12 | Driver.test.ts | 32.4 | 15 |
| 13 | EffectSuspensionComposition.test.ts | 31.3 | 11 |
| 14 | HashedCollectionDeterminism.test.ts | 27.7 | 4 |
| 15 | ChildProcess.test.ts | 26.2 | 13 |
| 16 | StoredEffectEngineParity.test.ts | 25.9 | 4 |
| 17 | StackVmPressureDeterminism.test.ts | 25.5 | 1 |
| 18 | HashedCollectionOwnership.test.ts | 25.2 | 4 |
| 19 | LexerPressureDeterminism.test.ts | 25.0 | 1 |
| 20 | WasmBackend.test.ts | 25.0 | 50 |
| 21 | IntegerScalars.test.ts | 24.9 | 6 |
| 22 | AlgorithmExamples.test.ts | 23.6 | 4 |
| 23 | HashedCollections.test.ts | 23.3 | 7 |
| 24 | HostInput.test.ts | 23.2 | 10 |
| 25 | SlotLaneWidth.test.ts | 23.2 | 14 |
| 26 | UnicodeNormalizationConformance.test.ts | 23.2 | 2 |
| 27 | RecursionStackBoundary.test.ts | 18.5 | 10 |
| 28 | VectorSort.test.ts | 17.5 | 13 |
| 29 | BootstrapEvaluation.test.ts | 17.3 | 29 |
| 30 | EffectRuntime.test.ts | 17.2 | 18 |
| 31 | MultiAffineReturn.test.ts | 16.7 | 2 |
| 32 | ScannerAcceptance.test.ts | 16.6 | 2 |
| 33 | NumberText.test.ts | 15.9 | 6 |
| 34 | BulkMemory.test.ts | 15.5 | 7 |
| 35 | UserServices.test.ts | 14.7 | 11 |
| 36 | ModuleVerification.test.ts | 14.4 | 1 |
| 37 | OwnedAllocationDispatch.test.ts | 12.9 | 4 |
| 38 | WasmShadowStackHeapCollision.test.ts | 12.8 | 5 |
| 39 | FloatMath.test.ts | 12.4 | 10 |
| 40 | Logging.test.ts | 12.0 | 7 |
| 41 | FileSystemAcceptance.test.ts | 10.7 | 8 |
| 42 | EffectSuspensionEvaluation.test.ts | 10.7 | 5 |
| 43 | ResultStdlib.test.ts | 10.2 | 10 |
| 44 | IntrinsicCatalog.test.ts | 10.0 | 6 |
| 45 | BoxHeapIndirection.test.ts | 8.6 | 10 |
| 46 | StringAcceptance.test.ts | 8.5 | 4 |
| 47 | IfThenElseAcceptance.test.ts | 7.9 | 10 |
| 48 | OwnedAllocation.test.ts | 7.6 | 10 |
| 49 | ScannerDeterminism.test.ts | 7.5 | 1 |
| 50 | ZipAcceptance.test.ts | 7.3 | 10 |
| 51 | DropHookExecution.test.ts | 6.9 | 3 |
| 52 | LlvmIrRoundTrip.test.ts | 6.5 | 4 |
| 53 | EditorIntelligence.test.ts | 6.3 | 25 |
| 54 | BoundOperationWitness.test.ts | 6.2 | 18 |
| 55 | StaticByteViewIndexing.test.ts | 6.1 | 4 |
| 56 | LoggingDeterminism.test.ts | 5.8 | 1 |
| 57 | AllocationMetricsAcceptance.test.ts | 5.6 | 4 |
| 58 | Suspendability.test.ts | 5.4 | 8 |
| 59 | Elaboration.test.ts | 5.1 | 82 |
| 60 | StoredCallableDiagnostic.test.ts | 5.0 | 11 |

Top 10 files: 682s (41% of per-file total). Top 30: 1176s (70%).
Determinism family: 23 files, 153s.
