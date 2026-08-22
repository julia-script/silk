## Context

See proposal.md. Each helper is extracted by taking the most general existing implementation, moving it to a single owner, and routing every other call site through it.

## Decisions

- **`internal/Graph.stronglyConnected(nodes, neighbors)`**: parameterized by key/neighbor projection so the module-name cycle (`ModuleClosure`), inline struct-dependency cycle (`DeclarationIndex`), and opaque-realization cycle all call the same fixed point. Preserve each caller's existing deterministic ordering (sorted canonical roots); keep the per-caller sort in the caller.
- **`ImportPath.spelling(path)` / `canonicalTarget(path)`**: consolidate the "filter segments, require available syntax, join" step; the `/` source-spelling vs `.` canonical form is decided once.
- **`alignUp`**: export from one low-level module (e.g. `internal/Align.ts`) and import into `Backend.ts:1101`, `WasmBackend.ts:228`, `CoroutineFrame.ts:6`.
- **`internal/ByteClass.ts`**: move the classifiers from `Lexer`/`LiteralForm` and `hexValue`/`digitValue` from `StaticText`/`IntegerLiteral`.
- **`internal/Escape.ts`**: single `escapeExtent` (what `scalarCount` computes) derived from the same escape list `StaticText.decode` consumes.
- **`FloatingPoint.canonicalNaN(width)`**: returns `0x7fc00000n`/`0x7ff8000000000000n`; `fromNumber`, `squareRoot`, and the transcendental `Plan` records reference it.
- **`PhaseReport.measure` / `measureEffectInto`**: synchronous measurement keeps synchronous host timing, while Effect measurement reads Effect's `Clock` so TestClock and application clock layers control observations. Pipeline/Frontend and Driver routes call these owners rather than constructing reports or reading an ambient timer inline. Heap source is injected once, not read from `node:process` inline.
- **`suspensionPointKey`**: export from `Backend.ts` and delete the copy at `WasmBackend.ts:1596`.

## Risks / Trade-offs

- [Determinism] → each extraction preserves the exact existing key/ordering; golden byte comparisons guard regressions.
- [Ordering subtlety] → Tarjan component sort order stays per-caller.

## Validation

`pnpm typecheck`, `pnpm exec biome check .`, `pnpm test` (golden/determinism suites are the regression net).
