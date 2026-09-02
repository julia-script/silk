## Context

Raw storage is reached through ten unsafe primitives, none of which works on a range. The two
consumers that pay for that are `Vector` growth, which migrates an initialized prefix into a fresh
buffer, and `Bytes.append`, which copies a borrowed byte sequence into a vector. Both are ordinary
Silk source over `RawBuffer`, `Slot`, and slices, so a bulk primitive has to fit that vocabulary
rather than introduce a second one.

## Goals / Non-Goals

Goals:

- One bulk move and one bulk byte set, both unsafe, both with the same bounds behavior on every
  engine.
- A defined answer for an overlapping copy, chosen so both backends agree by construction.
- Conversion of the two consumers the evidence names, without widening the safe public surface.

Non-Goals: a safe public bulk API, a resize primitive, a public `free`, a runtime overlap check, a
diagnostic for aliasing, and any change to the per-element primitives.

## Decisions

### The source range is a shared slice, the destination is a buffer plus an offset

`RawBuffer.view` already exists to name a range of raw storage, and a `&[T]` already carries a base
and a length. Naming the source with the vocabulary the library has means the copy serves both
consumers with one signature: `Vector` growth views the old buffer's initialized prefix, and
`Bytes.append` passes the borrowed sequence it was handed, which has no `RawBuffer` to point at.

A second `&RawBuffer<T>` source parameter would name the same range less directly and would leave
`Bytes.append` unable to use the primitive at all, since a caller's `&[u8]` cannot be turned back
into a buffer.

### Overlap is a defined move

The result of a copy whose source and destination ranges overlap is as if the elements travelled
through an intermediate buffer. LLVM lowers to `llvm.memmove` and Wasm to `memory.copy`, both of
which already define overlap, and the evaluator reads the whole range before writing any of it. No
engine checks for overlap and no diagnostic reports it, so the primitive has no undefined edge and
no cost on the non-overlapping path.

Silk's borrow rules do not let one buffer supply both a shared source borrow and an exclusive
destination borrow at the same call, so a source program cannot form the aliasing pair today. The
contract is stated at the level where it is observable — the shared MIR the three engines consume —
and the acceptance test forms the aliasing pair there.

### A Copy element keeps its source, a move-only element gives it up

The copy transfers ownership of every element in the range. For a structurally Copy element type
that transfer leaves the source readable, because duplicating those bytes duplicates no ownership;
this is what the byte-level backends do for every element type, and it is what lets `Bytes.append`
copy out of a sequence its caller keeps. For a move-only element type the moved-from slots are given
up, so reading one afterwards traps in the evaluator, which is the engine that tracks per-slot
initialization. Lowering records the classification on the operation so verification, evaluation,
and both backends decide the same way.

### The public bulk append is concrete to `u8`

`Vector.appendBytes` takes `&[u8]` rather than a generic `&[T]`. A generic version would let safe
code move a move-only element out of a borrowed slice, which the caller still owns. `u8` is Copy, so
the moved-from range stays valid and the public function is sound. The generic bulk move stays where
its precondition can be proved: inside the private growth path.

### Fill is byte-typed

The set primitive works on `RawBuffer<u8>` with a byte offset, byte length, and byte value. A
generic element-typed fill would either repeat a value it cannot copy or reinterpret element storage
as bytes, and neither is needed by the consumers this change converts.

## Risks / Trade-offs

- The copy can be handed a source range whose elements the caller has not given up. It is unsafe and
  the caller proves the precondition, exactly as `RawBuffer.view` and `Slot.take` already require.
- A source-level program cannot yet exercise the overlap contract, so the aliasing test forms the
  pair on lowered MIR. If the borrow rules later admit a within-buffer bulk move, the contract does
  not change.

## Open Questions

None.
