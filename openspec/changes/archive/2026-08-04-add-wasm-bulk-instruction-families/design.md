# Design — add-wasm-bulk-instruction-families

## Context

The core builder (archived `2026-08-04-add-wasm-builder-core`) established the extension points
this change relies on: multi-byte opcodes in the instruction table, `ValType` as a tagged union
with `v128` already present, validation split between `Func.define` and emission, and an
explicit oracle feature list. See proposal.md for scope; the four features are additive except
for address-type threading.

## Goals / Non-Goals

**Goals:**

- Grow the instruction table by the SIMD, relaxed SIMD, and atomic families without changing
  its schema; new uniform rows reuse the existing derivation into constructors, encoders, text,
  and validation.
- Thread one address-type parameter (i32 or i64) from memory/table declarations through memarg
  validation, limits checking, and both emitters.
- Byte-stability: modules using only baseline features emit identical output before and after
  this change (existing fixtures must not regenerate).

**Non-Goals:**

- No new index spaces, sections, or handle kinds.
- No `v128` interpretation or constant folding; `v128.const` bytes are caller-supplied.
- No memory64-for-Wasm-JS-API concerns (BigInt boundaries are the consumer's problem).

## Decisions

### 1. New `Instr` variants only where immediates differ

Uniform SIMD and relaxed-SIMD ops (no immediates) join `PlainMnemonic` with `[0xFD, n]`
opcodes — zero new variants, ~200 new table rows. New variants exist only for new immediate
shapes: `V128Const` (16 exact bytes), `Shuffle` (16 lane selectors), `SimdLane` (mnemonic +
lane index), `SimdMemoryAccess` and `SimdMemoryLane` (memarg, optionally + lane), `AtomicAccess`
(memarg with exact-alignment rule), and `AtomicFence`. Rationale: the core's design derives
everything uniform from the table; variants are reserved for genuinely new immediate encodings.
Alternative — one generic `Simd` variant with an immediates union — rejected: it re-creates the
dispatch the table already does, with worse types.

### 2. Atomics as a sibling access table

Atomic memory accesses mirror `memoryAccessOps`: a second table keyed by atomic mnemonic with
opcode (`0xFE` prefix), value type, width, and kind. The validator applies one different rule —
alignment must equal the natural width exactly rather than at most. `atomic.fence` is a plain
row with an immediate zero byte in the encoder.

### 3. Address type on the entity, threaded at validation

`Memory.make`, `Table.make`, and the corresponding imports accept
`{ addressType?: 'i32' | 'i64' }` (default `'i32'`) stored on the entry. The validator asks the
referenced entity for its address type wherever it previously assumed `i32` (memarg addresses,
`memory.size/grow`, bulk ops, table ops); the encoder picks 64-bit limits flags and u64 offsets
from the same field. Memarg `offset` widens to accept `bigint` for 64-bit memories while
remaining `number` for 32-bit ones. Rationale: the address type is a property of the entity in
the spec, so state carries it once and both consumers read it; no instruction-level annotation
is needed. `shared` follows the same pattern (memory entry flag; shared requires a declared
maximum, checked at declaration).

### 4. Oracle features grow in lockstep

`FEATURES` in `oracle.mjs` gains `simd`, `relaxed-simd`, `threads`, `memory64` in the same
change that emits those forms, keeping negative-corpus agreement meaningful: cases the builder
rejects must fail oracle validation *with these features enabled*, so the corpus proves the
builder is not merely lagging the feature set.

### 5. Fixture stability check

Existing fixture files are not regenerated; `fixtures:verify` passing against unchanged
committed bytes is the byte-stability proof for the baseline surface. New feature families get
new fixture modules (`simd`, `atomics`, `memory64`).

## Risks / Trade-offs

- [~280 new table rows are transcription-heavy] → rows are mechanical (mnemonic, opcode index,
  lane shapes); the oracle round-trip catches transcription errors per fixture, and lane/shuffle
  immediates are validated so bad rows fail loudly in tests.
- [wat text syntax for atomics/SIMD/memory64 has corners (e.g. `v128.const i8x16 …` lane
  formats, `memory.atomic.notify` memargs, `i64` limits)] → same empirical loop as core: the
  round-trip oracle check arbitrates; text forms are adjusted to whatever parses to our bytes.
- [offset as `number | bigint` complicates the memarg surface] → validation normalizes early:
  32-bit memories reject bigint offsets above u32; 64-bit memories accept both and store
  canonically.

## Open Questions

- Whether `v128.const` gets convenience constructors (`i8x16`, `f32x4` lane inputs) beyond raw
  bytes can be decided during implementation; the spec requirement is bytes-exact either way.
