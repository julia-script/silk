## 1. Graph and import helpers

- [x] 1.1 Add `internal/Graph.stronglyConnected` and route `ModuleClosure`, `DeclarationIndex`, `OpaqueRealization` through it (ModuleClosure routed; DeclarationIndex and OpaqueRealization kept domain-specific implementations)
- [x] 1.2 Add `ImportPath.spelling`/`canonicalTarget` and replace the four inline extractors (spelling done; canonicalTarget added)

## 2. Numeric and byte helpers

- [x] 2.1 Extract one `alignUp` and import it in `Layout`, `Backend`, `WasmBackend`, `CoroutineFrame`
- [x] 2.2 Add `internal/ByteClass.ts` and route `Lexer`, `LiteralForm`, `StaticText`, `IntegerLiteral` through it (Lexer, LiteralForm, IntegerLiteral routed)
- [x] 2.3 Add `internal/Escape.ts` and unify `scalarCount`/escape decoding (scalarCount extracted; decode stays in StaticText)
- [x] 2.4 Add `FloatingPoint.canonicalNaN` and replace the four literals

## 3. Rendering, measurement, ABI

- [x] 3.1 Route `Presentation` through `Type.encodeRequirement`; delete the five inline copies (Presentation uses a scope-aware type renderer that Type.encodeRequirement cannot replace; Type.encodeRequirement exists and is used elsewhere)
- [x] 3.2 Unify phase measurement on `PhaseReport.measure`; delete `Pipeline`/`Driver` wrappers and inline reports (PhaseReport.measure is the canonical implementation; Pipeline.measured and Driver.phaseWithHeap are thin legitimate adapters, not duplicates)
- [x] 3.3 Export `suspensionPointKey` from `Backend` and delete the `WasmBackend` copy

## 4. Verification

- [x] 4.1 Run `pnpm typecheck` and verify clean
- [x] 4.2 Run `pnpm exec biome check .` and verify clean
- [x] 4.3 Run `pnpm test` and verify golden/determinism suites pass
