# Add SIMD, atomics, and 64-bit memories to `@silk-effect/wasm`

## Why

The core builder ships the baseline feature set, but the package's declared destination is
everything Chrome ships unflagged. The largest remaining gap is the bulk instruction families —
SIMD, relaxed SIMD, and atomics — plus 64-bit addressing. These four features are mostly
additive table rows and one type-threading change, so landing them together as the first
follow-up keeps the instruction table's growth in one review while the core design is fresh.

## What Changes

- **SIMD**: the `v128` instruction set (~236 instructions) — loads/stores with lane and splat
  variants, lane access with immediate indices, shuffles with 16-byte immediates, `v128.const`,
  and the full arithmetic/comparison/conversion families under the `0xFD` opcode prefix.
- **Relaxed SIMD** (~20 instructions): the standardized relaxed variants, same prefix.
- **Threads**: shared memories (a `shared` flag on memory limits, requiring a maximum) and the
  atomic instruction family (~67 instructions) under the `0xFE` prefix — loads/stores, RMW
  operations, compare-exchange, `memory.atomic.wait/notify`, and `atomic.fence`. Atomic
  accesses require exact natural alignment.
- **memory64**: an address type (`i32` or `i64`) on memories and tables. Address-taking
  instructions (loads/stores, `memory.*`, `table.*`) type against the entity's address type;
  64-bit limits and offsets are validated and encoded accordingly.
- Existing `Instr` constructors, handles, and emitters are extended, not reshaped; committed
  modules from the core baseline emit byte-identical output.
- Parity: oracle feature list gains `simd`, `relaxed-simd`, `threads`, `memory64`; new fixtures
  per family; negative corpus entries per new validator rule.

## Capabilities

### New Capabilities

None — all changes extend existing capabilities.

### Modified Capabilities

- `wasm-function-bodies`: instruction coverage grows by the SIMD, relaxed SIMD, and atomic
  families; validation gains lane-index, shuffle-immediate, atomic-alignment, and address-type
  rules.
- `wasm-module-declarations`: memories accept `shared` and an address type; tables accept an
  address type; 64-bit limits validation.
- `wasm-output`: binary encodings (`0xFD`/`0xFE` prefixes, `v128.const` and shuffle immediates,
  shared/64-bit limits flags) and text renderings for all new instructions and declaration
  forms.
- `wasm-builder-parity`: oracle features extended; fixture inventory gains SIMD, atomics, and
  memory64 modules; negative corpus covers the new rules.

## Impact

- `packages/wasm` only: instruction table, `Instr`, `ValType` (none — `v128` already exists),
  `Memory`/`Table`/`Import` options, validator, both emitters, fixtures, and scripts.
- No new dependencies; the pinned `wasm-tools 1.255.0` oracle already supports all four
  features.
- Public API is extended compatibly; no existing signatures change except optional fields on
  memory/table declaration options.
