## Why

Every raw-storage primitive Silk has works on exactly one element. `Vector` growth therefore moves
its initialized elements one `Slot.take`/`Slot.write` pair at a time, and `Bytes.append` walks a
borrowed byte sequence one `Vector.append` at a time. The lexer pressure study recorded the cost —
"vector growth moves initialized records element by element" — and asked for evidence before adding
a bulk primitive (`examples/language-pressure/lexer/findings.md`). Both engines already have the
instruction the loop is standing in for: LLVM has `llvm.memmove` and `llvm.memset`, and Wasm has
`memory.copy` and `memory.fill`.

## What Changes

- Add `Intrinsic.rawBufferCopy`, an unsafe primitive that moves a caller-proven initialized range
  into raw storage in one transfer. The caller names the source range as a shared slice, which is
  how every other raw-storage operation names a range, and the destination as an exclusive
  `RawBuffer` borrow plus an element offset.
- Define an overlapping source and destination as a correct move: the result is as if the elements
  travelled through an intermediate buffer. There is no runtime overlap check and no diagnostic —
  LLVM lowers the copy to `llvm.memmove` and Wasm to `memory.copy`, both already defined for
  overlap, and the evaluator reads the whole range before it writes any of it.
- Move ownership of every copied element. A structurally Copy element leaves the source range
  readable, which is what duplicating its bytes already means; a move-only element gives its source
  slots up, so reading one afterwards traps.
- Add `Intrinsic.rawBufferFill`, an unsafe primitive that sets a caller-proven byte range of
  `RawBuffer<u8>` to one repeated byte value, lowering to `llvm.memset` and `memory.fill`.
- Trap identically on all three engines when a range runs past the destination buffer or past the
  source slice, matching the bounds behavior of `rawBufferSlot`, `rawBufferRead`, and
  `rawBufferView`.
- Convert `Vector` growth to one bulk move per migration, and add `Vector.appendBytes` so
  `Bytes.append` copies a borrowed byte sequence in one transfer. The public bulk append is
  concrete to `u8` rather than generic: the copy moves its source range, and moving out of a
  borrowed slice is only a copy when the element type is Copy.

## Capabilities

### Modified Capabilities

- `bootstrap-intrinsic-boundary`: admit two unsafe bulk raw-storage primitives, with overlap defined
  as a move rather than left undefined.
- `bootstrap-backend`: require `llvm.memmove`/`llvm.memset` and `memory.copy`/`memory.fill`
  lowering, and identical trapping bounds behavior.
- `bootstrap-evaluation`: evaluate a bulk move as if through an intermediate buffer, and give up the
  moved-from slots of a move-only element.
- `bootstrap-silk-stdlib`: expose the two primitives as `RawBuffer.copy` and `RawBuffer.fill`, and
  use the copy for `Vector` growth and `Bytes.append`.

## Impact

The change affects the intrinsic inventory, HIR and MIR operation identity, MIR verification, the
evaluator, both backends, standard-library source and generated documentation, and acceptance tests.
It adds no safe public bulk API, no resize primitive, no public `free`, no runtime overlap check,
and no change to the per-element primitives.
