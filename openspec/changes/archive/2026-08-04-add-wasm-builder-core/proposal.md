# Add `@silklang/wasm` core builder

## Why

The workspace has an Effect-native LLVM builder but no equivalent for WebAssembly, even though
WebAssembly is a primary target for the project's long-term direction. A standalone,
dependency-free builder that emits both `.wasm` bytes and `.wat` text gives any consumer — including
a future compiler backend — a validated, deterministic way to produce WebAssembly modules without
native tooling at runtime.

This change is deliberately independent of the bootstrap-language roadmap: it is a standalone
library in the same sense `@silklang/llvm` is, sharing its purpose (build module state, emit two
representations) but not its architecture. The design is WebAssembly-native.

## What Changes

- New package `@silklang/wasm` with subpath-exported actor modules, Effect-native errors, and no
  runtime dependency on external tools, the filesystem, or process execution.
- Instructions are plain immutable data (a discriminated `Instr` union); function bodies are
  `ReadonlyArray<Instr>` committed in one operation. Effects exist only at commit boundaries.
- Public values are opaque builder-owned handles; index spaces (types, functions, tables, memories,
  globals, elements, data segments) are computed at emit time, so declaration order and late imports
  never invalidate references.
- Full spec validation: function bodies are validated at define time using the specification's
  stack-typing algorithm (including polymorphic unreachable typing); module-level constraints
  (export-name uniqueness, segment offsets, start-function signature) are validated at emit.
  An emitted module is guaranteed valid.
- A single source-of-truth instruction table (mnemonic, opcode, immediates shape, typing signature)
  drives the constructors, binary encoder, text printer, and validator.
- Optional names on all declarable entities feed both readable `.wat` identifiers and the binary
  `name` custom section.
- Two emitters over one committed state: `WatText.render` returns a `string`, `Binary.encode`
  returns a `Uint8Array`.
- Feature baseline for this change: WebAssembly core 2.0 (multi-value, bulk memory, reference
  types, sign extension, saturating float-to-int, mutable globals) plus tail calls, extended
  constant expressions, and multiple memories.
- Deterministic fixtures verified against a pinned `wasm-tools` oracle (validate, wat↔wasm
  round-trip, byte-identical binary comparison), mirroring the LLVM parity discipline with one
  pinned tool.

Deferred to follow-up changes (in order): SIMD + relaxed SIMD + threads/atomics + memory64;
exception handling (tags, `try_table`, `exnref`) + branch hinting; GC + typed function references.
Permanently out of scope: legacy exception handling, JS-API-only features (JSPI, BigInt
integration, JS string builtins, type reflection), and proposals below phase 4 (compilation hints,
stack switching).

## Capabilities

### New Capabilities

- `wasm-builder-foundation`: one builder as concurrency-safe owner of module state; opaque
  owner-checked handles; typed `WasmError` failure model; optional entity names.
- `wasm-module-declarations`: function types with structural interning; imports and exports;
  functions, tables, memories, globals; start function; element and data segments.
- `wasm-function-bodies`: the `Instr` data model and instruction table for the baseline feature
  set; locals; structured control flow as nested data; define-time full body validation.
- `wasm-output`: deterministic `.wat` text rendering and `.wasm` binary encoding over committed
  state; emit-time index resolution and module-level validation; `name` custom section.
- `wasm-builder-parity`: pinned `wasm-tools` oracle; fixture generation and verification;
  round-trip and determinism guarantees.

### Modified Capabilities

None — the package is new and touches no existing capability.

## Impact

- New workspace package `packages/wasm` wired into pnpm workspace, turbo, CI, and changesets.
- No changes to `@silklang/llvm`, `@silklang/compiler`, or any existing package.
- New dev-time-only dependency on a pinned `wasm-tools` binary for fixture generation and parity
  verification; runtime remains dependency-free apart from `effect`.
