## 1. Graph and import helpers

- [ ] 1.1 Add `internal/Graph.stronglyConnected` and route `ModuleClosure`, `DeclarationIndex`, `OpaqueRealization` through it
- [ ] 1.2 Add `ImportPath.spelling`/`canonicalTarget` and replace the four inline extractors

## 2. Numeric and byte helpers

- [ ] 2.1 Extract one `alignUp` and import it in `Layout`, `Backend`, `WasmBackend`, `CoroutineFrame`
- [ ] 2.2 Add `internal/ByteClass.ts` and route `Lexer`, `LiteralForm`, `StaticText`, `IntegerLiteral` through it
- [ ] 2.3 Add `internal/Escape.ts` and unify `scalarCount`/escape decoding
- [ ] 2.4 Add `FloatingPoint.canonicalNaN` and replace the four literals

## 3. Rendering, measurement, ABI

- [ ] 3.1 Route `Presentation` through `Type.encodeRequirement`; delete the five inline copies
- [ ] 3.2 Unify phase measurement on `PhaseReport.measure`; delete `Pipeline`/`Driver` wrappers and inline reports
- [ ] 3.3 Export `suspensionPointKey` from `Backend` and delete the `WasmBackend` copy

## 4. Verification

- [ ] 4.1 Run `pnpm typecheck` and verify clean
- [ ] 4.2 Run `pnpm exec biome check .` and verify clean
- [ ] 4.3 Run `pnpm test` and verify golden/determinism suites pass
