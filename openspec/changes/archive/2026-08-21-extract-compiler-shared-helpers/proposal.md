## Why

The same low-level algorithm is implemented two to four times across unrelated files, each copy a drift hazard. This change extracts one owner per helper so a fix or determinism change is made exactly once.

## What Changes

- **One Tarjan SCC helper** (`internal/Graph.ts`) replaces `ModuleClosure.cycleFacts`, `DeclarationIndex.stronglyConnected`, and the `OpaqueRealization` copy.
- **One import-path extractor** (`ImportPath.spelling`/`canonicalTarget`) replaces the four hand-rolled `/` vs `.` joins in `ModuleSummary`, `ImportPlan`, `ModuleClosure`, `NameResolution`.
- **One `alignUp`** shared by `Layout`, `Backend`, `WasmBackend`, and `CoroutineFrame`.
- **One ASCII byte-classification module** (`internal/ByteClass.ts`): `isAsciiLetter`/`isDecimalDigit`/`isIdentifierStart`/`isIdentifierContinue`/`hexValue`, used by `Lexer`, `LiteralForm`, `StaticText`, `IntegerLiteral`.
- **One escape-vocabulary module** (`internal/Escape.ts`) owning "which escapes extend a literal" and "what each escape means", closing the `scalarCount`↔`decode` sync risk.
- **One canonical-NaN constant** (`FloatingPoint.canonicalNaN(width)`) replacing the four hardcoded bit patterns in `FloatingPoint` and `Transcendental`.
- **One requirement-member renderer** (`Type.encodeRequirement`) consumed by `Presentation`, deleting five inline copies.
- **One phase-measurement path** (`PhaseReport.measure`) replacing the four re-implementations in `Pipeline` and `Driver`.
- **One `suspensionPointKey`** exported from `Backend` and imported by `WasmBackend`.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

<!-- none: behavior-preserving refactor (skip_specs) -->

## Impact

Pure dedup; no observable behavior change. Touches `ModuleClosure`, `DeclarationIndex`, `OpaqueRealization`, `ModuleSummary`, `ImportPlan`, `NameResolution`, `Layout`, `Backend`, `WasmBackend`, `CoroutineFrame`, `Lexer`, `LiteralForm`, `StaticText`, `IntegerLiteral`, `FloatingPoint`, `Transcendental`, `Type`, `Presentation`, `PhaseReport`, `Pipeline`, `Driver`. `skip_specs: true`.
