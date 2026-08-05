# Design — add-wasm-builder-core

## Context

The workspace already ships `@silk-effect/llvm`, whose conventions this package inherits at the
repository level (Effect architecture rules, subpath exports, `@effect/vitest` testing, fixture
oracles, changesets). Its internal architecture is *not* inherited: WebAssembly's module model —
flat sections, index spaces, structured control flow, a closed instruction set — permits a
substantially simpler, data-first design. See proposal.md for motivation and scope.

The eventual final surface (decided in the explore session) is everything Chrome ships unflagged:
Wasm 3.0 + threads + relaxed SIMD + branch hinting. This change lands the core; three follow-up
changes layer the rest. Design decisions here must not paint those follow-ups into a corner.

## Goals / Non-Goals

**Goals:**

- A wasm-native design where instructions are values and the builder owns only module state.
- Guaranteed-valid output: full spec validation split between define time and emit time.
- One instruction table as the single source of truth for four projections (constructor, binary
  opcode, text mnemonic, typing rule).
- Extension points that let SIMD/atomics/memory64, exception handling, and GC land as additive
  table rows and new handle kinds without reshaping the core.

**Non-Goals:**

- No CFG input or relooper: callers provide structured control flow (the wasm shape).
- No streaming/incremental emission; modules are emitted whole from committed state.
- No text or binary *parsing*; this package only produces the two representations.
- No optimization of any kind (constant folding, dead-code elimination, local coalescing).

## Decisions

### 1. Instructions are plain data, bodies commit atomically

An `Instr` is a frozen tagged value created by pure constructors (no builder argument, no
Effect). A body is `ReadonlyArray<Instr>` plus locals, committed via one effectful
`Func.define`. Rationale: WebAssembly bodies are finite instruction sequences with structured
nesting — a value, not a process. This removes the llvm package's draft/transaction machinery
entirely: an invalid body is rejected as a value; there is nothing to roll back. Alternative
considered: llvm-style effectful emission against a draft body — rejected as ceremony that
WebAssembly's model does not require, and it would prevent callers from composing bodies with
ordinary array logic.

Structured control flow nests as data: `block`/`loop`/`if` variants carry child instruction
arrays and a block type. Branch targets use spec-standard relative depths (plain numbers), not
labels — the future backend emits structured code and computes depths naturally, and depths keep
instruction values context-free.

### 2. Handles in, indices out

Public entity references are opaque builder-owned handles; numeric indices exist only inside the
emitters, computed once per emission in canonical section order (imports first within each index
space, then definitions in declaration order). Rationale: index-space renumbering when imports
are added late is the classic hand-rolled-wasm bug; absorbing it in the emitter removes the whole
class. Ownership checks reuse the repository's established owner-token pattern. Alternative
considered: exposing raw indices with a fix-up pass — rejected; it moves the hardest bookkeeping
onto every caller.

### 3. One instruction table drives everything

Each baseline instruction has exactly one table row: mnemonic, opcode byte(s), immediates shape,
and typing signature (populated stack types in/out, or a reference to one of the small set of
non-uniform typing rules — calls, branches, memory access, parametric operations). The
constructors, binary encoder, text printer, and the uniform part of the validator are all derived
from this table. Rationale: at the final surface (~600 instructions after follow-ups), four
hand-written projections guarantee drift; one table makes wasm-tools parity enforceable and makes
each follow-up change mostly additive rows. Alternative considered: hand-written per-module
switch statements as in the llvm port — workable at 200 instructions, hostile at 600.

### 4. Validation split: bodies at define, module facts at emit

Body validation runs inside `Func.define` using the spec appendix's algorithm (value stack +
control-frame stack, with per-frame unreachable mode for polymorphic typing). It can run there
because every referenced entity is a handle whose type is already known at declaration.
Module-level constraints that are not per-function facts — export-name uniqueness, active-segment
offset typing, start signature, limits — run once at emission, shared by both emitters.
Rationale: errors surface at the earliest decidable point, and emitted modules are valid by
construction. Alternative considered: validate everything at emit — simpler layering but reports
body errors far from their cause; structural-only validation — rejected by explicit decision in
the explore session.

### 5. Emitters are peers over committed state

`Binary.encode` and `WatText.render` read the same committed, already-validated module state and
share the emission-time index resolution. Text output uses `$name` identifiers where names exist
and bare indices otherwise; binary output carries names in the `name` custom section. The parity
requirement (text assembled by the oracle equals our binary bytes) keeps the two projections
provably equivalent.

### 6. Oracle: pinned `wasm-tools`

One pinned `wasm-tools` release provides validation, wat→wasm assembly for round-trip
comparison, and inspection for the name section. Dev-time only, recorded in provenance docs,
mirroring the llvm package's pinned-upstream discipline with a single tool. Negative corpus
verification (builder rejects ⇒ oracle rejects) guards against the builder being *stricter* in
the wrong places or accepting what the spec forbids.

### 7. Package layout

```
packages/wasm/src/
  Builder.ts      module owner: options, gate, state registration
  ValType.ts      value types as plain data (i32|i64|f32|f64|v128|funcref|externref)
  Type.ts         function-type interning
  Import.ts       imported funcs/tables/memories/globals
  Func.ts         declare / define(locals, body) / start
  Table.ts  Memory.ts  Global.ts  Elem.ts  Data.ts  Export.ts
  Instr.ts        instruction constructors + the Instr union (pure data)
  ConstExpr.ts    constant expressions for initializers/offsets (subset of Instr)
  WatText.ts      render(builder) -> string
  Binary.ts       encode(builder) -> Uint8Array
  WasmError.ts    typed failure model
  internal/       instruction table, validator, index resolution, LEB128, state
```

Subpath exports per module, root barrel discouraged — matching repository convention.

## Risks / Trade-offs

- [Typing-rule table rows oversimplify non-uniform instructions] → the table's typing field is a
  closed union: `uniform` rows carry stack types directly; the handful of context-dependent rules
  (branches, calls, parametric, memory) reference named validator procedures. The validator, not
  the table, owns the hard cases.
- [Follow-up features could invalidate core encodings] → the encoder keys everything off table
  rows and section builders; SIMD/atomics add prefixed opcodes (multi-byte opcode support goes in
  now), EH adds a section kind (section framing is generic), GC changes `ValType`
  (ValType is modeled as a tagged union from day one, not a string enum, so heap types slot in).
- [Full spec validation has subtle corners (unreachable polymorphism, select typing, br_table
  arity agreement)] → the negative corpus plus oracle agreement checks are written per validator
  rule, not per bug found; the spec appendix algorithm is followed structurally rather than
  re-derived.
- [Byte-identical fixture comparison is brittle if wasm-tools canonicalizes differently] → the
  committed expected bytes are *our* output checked into fixtures; the oracle asserts validity
  and round-trip equality, never formatting opinions.
- [Names in wat identifiers must be sanitized] → wat `$id` character set is restricted; names
  outside it fall back to indices in text while remaining exact in the binary name section.

## Open Questions

- Whether `ConstExpr` should be a distinct type or a validated subset of `Instr` sequences can be
  settled during implementation; both satisfy the specs as written.
- Exact fixture inventory (how many modules per feature area) is left to the parity tasks.
