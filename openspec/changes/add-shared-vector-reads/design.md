## Context

`Vector.get` currently obtains a Copy value by replacing `self.storage`, moving the `Full<T>` and
`RawBuffer<T>` through helper functions, projecting an exclusive `Slot<T>`, copying it, and restoring
the entire storage union. The sequence is allocation-free but requires `&mut Vector<T>` and makes a
read appear to mutate ownership state. Shared match patterns already retain shared-access provenance
for their field bindings; the missing seam is below the standard library.

`Slot<T>` deliberately represents an exclusive lexical projection into raw storage. Its API includes
write, take, copy, and drop, so making Slot itself shareable would weaken the capability boundary or
require a second slot type. The independent structural-union copy failure occurs after slot
projection and is not evidence that shared sequence reads need union-specific behavior.

## Goals / Non-Goals

**Goals:**

- Make an in-bounds `Vector<T>` read for a supported Copy element a zero-allocation shared operation.
- Preserve one general raw-storage operation beneath ordinary Silk library code.
- Keep verifier and engine behavior explicit, target-aware, and deterministic.
- Use the pressure programs to prove the surface is sufficient for real consumers.

**Non-Goals:**

- References, slices, views, iterators, or element mutation through Vector.
- Structural-union element copies, including the VM's combined event union.
- Runtime borrow tracking, initialization bitmaps, concurrency, suspension, or synchronization.
- Changing the existing trap-based checked-access contract.

## Decisions

### Add a direct shared raw-buffer read, not a shared Slot

The intrinsic surface gains the conceptual operation
`RawBuffer.read<T>(buffer: &RawBuffer<T>, index: usize) -> T`. It is unsafe because the caller still
owns the proof that the addressed element is initialized. Semantic instance validation requires a
canonical non-union Copy element and a shared buffer reference. MIR carries buffer, index, element,
result, and source provenance, and engines perform the existing checked stride calculation followed
by a value load.

A second `SharedSlot<T>` was considered. It would add a capability whose only current legal action is
copy, complicate loan escape rules, and create an attractive but premature base for reference-returning
collections. Reusing `Slot<T>` was rejected because write/take/drop require exclusivity.

### Implement Vector observation entirely in Silk

`Vector.get` changes to accept `&Vector<T>`, checks `length`, shared-matches `storage`, and calls the
new raw read on the `Full<T>.buffer` binding. The compiler still knows only raw storage; it gains no
Vector operation, layout, or growth rule. The old `Taken<T>`, `copyAt`, and `copySlot` restoration path
is deleted.

### Reject structural unions at intrinsic instantiation

The initial supported boundary is a Copy type whose canonical form is not a structural union. This
includes the nominal Copy records used by the lexer and the VM's separated step and diagnostic
vectors. The compiler emits the normal invalid-intrinsic-instance diagnostic for move-only and union
elements before lowering.

Allowing unions opportunistically was rejected: current `Slot.copy` loses result provenance for
structural unions, and bypassing that defect in a new operation would create inconsistent Copy
semantics. A following proposal can repair the common provenance model and then remove this narrow
restriction coherently.

### Prove absence of read-side ownership work

Tests inspect MIR and evaluator traces as well as returned values. A shared read must contain no
Slot projection, allocation, initialization transition, move of the buffer, or cleanup operation.
LLVM and Wasm parity cases read through multiple shared aliases, cover bounds traps, and drop or move
the vector afterward.

## Risks / Trade-offs

- **The unsafe read trusts initializedness.** → Keep it unsafe and reachable from safe code only
  through Vector's length invariant; do not add a bitmap or runtime branch.
- **A new intrinsic duplicates part of `Slot.copy` lowering.** → Share internal layout/load helpers
  where they remain actor-local, while keeping the public capability distinction explicit.
- **The temporary union exclusion is visible at generic instantiation.** → Diagnose it before MIR,
  document it as a focused boundary, and remove it only with the structural-union provenance repair.
- **Shared pattern lowering may expose a latent nested-borrow defect.** → Add a focused shared-field
  raw-buffer test before rewriting Vector and repair only general shared-pattern provenance if needed.

## Migration Plan

Land the intrinsic and cross-engine parity first, then change the standard-library signature and
remove the restoration helpers. Update all call sites to pass shared borrows, rewrite pressure
harnesses and findings, run the complete repository gates, sync the delta specs, archive the change,
and merge it to main. Because Silk is unreleased, no compatibility shim or deprecated overload is
kept.
