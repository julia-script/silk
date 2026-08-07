## Context

See `proposal.md` for motivation and the delta specs for observable requirements. The current
compiler already publishes target-aware layouts, lexical ownership and cleanup facts, a structured
MIR DAG, evaluator execution, native LLVM, direct WebAssembly, and runtime slices. It does not yet
execute `flow fn`, typed failure rows, capability requirements and provision, named scope wrappers,
or source-visible pointer-sized integers.

Wayfinder issues 01, 02, 03, 07, and 08 jointly constrain allocation: ordinary `fn` is pure;
`OutOfMemory` is an abortive typed failure; allocator access is a capability-role requirement;
`Scope.scoped` is a flow wrapper rather than an allocation-specific block; and reclaim must retain
its original provider rather than resolve an ambient allocator later. Implementing allocation before
those general foundations would necessarily hard-code allocator behavior into the compiler.

## Goals / Non-Goals

**Goals:**

- Define the allocation change that can be implemented once its general language prerequisites are
  archived, without reopening its memory model.
- Give unsafe Silk code the minimum byte ownership, typed slot projection, and cleanup hooks needed
  to implement `Vector<T>` and other owned containers entirely in Silk.
- Keep scope lifetime, allocator policy, element cleanup, and target layout as separate concepts.
- Preserve one compiler-published target-aware structured DAG through evaluator, native, and Wasm.

**Non-Goals:**

- Implement flow functions, general typed failures and handlers, capability declaration/provision,
  service roles/witnesses, named scope wrappers, or `Usize`; those land as preceding OpenSpec changes.
- Implement `Vector<T>`, an arena allocator, allocation metrics, primitive resize, implicit zeroing,
  bulk byte operations, stored slices, public `free`, or general finalizers.
- Prove initializedness or non-aliasing for arbitrary runtime slot indices in safe code.
- Define a stable external allocator or runtime ABI.

## Decisions

### 1. Land five explicit changes rather than one allocator-shaped shortcut

Implementation order is:

1. ✅ `add-usize-scalar` — archived 2026-08-07
2. ✅ `add-flow-functions-and-typed-failures` — implemented; archive after final verification
3. `add-capability-requirements-roles-and-provision`
4. `add-named-scope-wrappers-and-cleanup`
5. `add-scoped-allocation-primitives`

The first four are hard prerequisites. Allocation uses their public compiler representations and
does not duplicate them. `Usize` precedes `Layout` so permanent allocation arithmetic never inherits
the runtime-slice ticket's temporary `I32` length. Flow and failure support makes `OutOfMemory` an
ordinary typed failure rather than a trap or result-data special case. Capability slots and witness
dispatch give reclaim tickets an origin without allocator-kind switches. Scope wrappers provide the
actual destination lifetime and runtime LIFO cleanup stack.

Putting all five in this change was rejected because each prerequisite is independently observable
across every compiler phase and backend. Passing an allocator as an ordinary explicit parameter was
rejected because it contradicts the accepted requirement-row model. Allocator-specific hidden state
was rejected because users could not build equivalent capabilities and providers.

### 2. Reuse `Scope.scoped`; add no allocation-specific scope syntax

Allocation receives the active destination scope selected by the established wrapper semantics.
Provider wrapper order determines lifetime: the allocator provider must be acquired outside, or
otherwise proven to outlive, the destination scope. The opposite order is rejected before lowering.
Functions that use an enclosing scope propagate that requirement; a function may instead wrap its
own flow in `Scope.scoped` and consume every tied value within the wrapper.

A `scope name { ... }` statement was rejected because Wayfinder already settled scopes as flow
wrappers whose order composes with provision and handling.

### 3. Separate byte layout from typed slot layout

`Layout` is the Copy pair of validated target-width byte size and power-of-two alignment.
`SlotLayout<T>` retains a concrete canonical `T`, runtime logical count, element alignment, aligned
stride, and checked total `Usize` byte size. The total is `stride * count` with checked arithmetic;
count zero and zero-sized elements produce byte size zero while retaining count, alignment, stride,
and type identity.

The compiler derives `SlotLayout<T>` from its selected target layout for `T`; a backend never asks
LLVM, the host allocator, or WebAssembly to decide the stride. Treating arbitrary `Layout` as proof
of typed storage was rejected because byte size and alignment do not retain element type, count, or
zero-sized bounds.

### 4. Make `Allocation` an affine byte owner with a stable private cleanup control block

Successful acquisition atomically creates physical storage, one logical allocation identity, a
private control block, and one scope cleanup record. The control block contains the originating
provider context and release witness, layout facts, active state, and stable registration identity.
It is privileged runtime capability data, not a stored Silk borrow or forgeable source value.

Moving `Allocation` transfers the affine handle without moving its record. Explicit or lexical owner
cleanup releases bytes through the original witness and disarms the record. Scope closure walks
records in reverse acquisition order and skips disarmed entries. A failed acquisition creates none
of these. Zero-byte allocations still receive distinct logical control blocks even when physical
addresses are shared or absent.

Re-resolving the current allocator role at drop was rejected because provision may have changed.
Registering cleanup after allocation was rejected because registration would introduce a second
failure or a leak window. Letting both static drops and scope closure call release independently was
rejected because exact-once behavior would depend on backend luck.

### 5. Keep runtime slot invariants explicitly unsafe

Unsafe slot selection combines a live `Allocation`, its exact `SlotLayout<T>`, and a checked runtime
index to create a lexical exclusive `Slot<T>` place. The compiler verifies canonical type/layout
provenance, bounds-check ordering, borrow lifetime, and that the allocation is not moved or released
while the place is live. Slot places cannot be copied, stored, returned, or cross a safe boundary.

Unsafe Silk code is responsible for ensuring runtime-selected slots do not alias and are initialized
before reading, moving, or dropping their `T`. The evaluator may track logical contents to diagnose
violated fixtures, but this is not a promised runtime bitmap. MIR verification does not pretend to
solve dynamic indexed typestate.

Compiler-proven per-slot `Uninit<T>` transitions were rejected because a vector's initialized prefix
and indices are runtime values. A runtime bitmap was rejected as permanent cost and hidden policy.
A compiler-owned initialized buffer was rejected because it would make the first collection
primitive compiler magic.

### 6. Add one restricted drop hook for affine structs

An affine nominal struct may declare one compiler-invoked drop hook. It receives an exclusive borrow
of `self`, is statically infallible, non-allocating, requirement-free, cannot move from or replace
`self` or its fields, and cannot retain a borrow. The hook runs once before ordinary recursive field
cleanup. It is not callable as public `free` and cannot register a capturing finalizer.

This is the minimum mechanism that lets `Vector<T>` own an `Allocation`, length, and capacity while
dropping initialized elements `0..<length` before the allocation field releases bytes. During
fallible construction, an ordinary affine guard updates its initialized-prefix length after each
successful write; its same restricted hook rolls back the prefix before byte release.

Having `Allocation` guess which bytes contain live `T` values was rejected because it requires a
bitmap or collection-shaped prefix policy. Derived field cleanup alone was rejected because raw
storage hides the elements from the compiler. Restricting vectors to cleanup-free `T` was rejected
because it does not support compiler data.

### 7. Use the evaluator as the semantic oracle, then realize the selected contract per target

The evaluator models provider identities, allocation ordinals, logical allocation records, slot
contents, cleanup control blocks, and ordered events without using JavaScript object identity or GC.
A deterministic test provider fails one chosen allocation ordinal, enabling a complete failure sweep
and same-process reuse check.

Native lowering calls the compiler-versioned system allocation/release shim through the established
service witness ABI. Wasm lowering owns a heap region separate from existing private stack frames,
uses checked aligned growth, converts `memory.grow` failure to typed `OutOfMemory`, and realizes the
same private reclaim witness/control-block semantics. Both consume compiler-planned word, layout,
failure, service-slot, and cleanup shapes. Neither backend can substitute a trap for OOM, choose a
different stride, synthesize a provider, or use host addresses as logical identity.

Arena policy and metrics are deferred. One system provider plus one deterministic failure provider
is enough to prove the abstraction seam.

### 8. Acceptance proves rollback and exact cleanup, not merely successful bytes

The canonical owner fixture allocates runtime-counted slots for a move-only aggregate, initializes a
prefix while updating its guard length, consumes the completed values, and exits through success,
typed failure, early return, `break`, and `continue`. Failure injection covers every allocation and
initialization ordinal. Every engine must show hook-driven element cleanup before byte release,
reverse acquisition order for records in regions actually exited, no record for a failed request,
explicit drop followed by skipped scope fallback, zero-sized logical identity, and successful reuse
after each failure.

`/labs` exposes this one fixture and a focused exhaustion fixture across coordinated projections;
there is no standalone allocation inspector.

## Risks / Trade-offs

- [Risk] The allocation proposal appears ready while prerequisite changes are not archived. → Keep
  the prerequisite sequence explicit here and in the project roadmap; do not invoke apply for this
  change until all four foundations are complete.
- [Risk] Unsafe slot code can violate initializedness or aliasing. → Keep the boundary lexical and
  narrow, verify provenance/bounds/lifetime, exercise construction guards adversarially, and never
  expose slots to safe owned code.
- [Risk] Drop hooks grow into arbitrary finalizers. → Enforce the closed restrictions in semantic,
  ownership, HIR, and MIR verification and reject failures, requirements, allocation, moves from
  `self`, capture, and manual invocation.
- [Risk] Static owner cleanup and dynamic scope records double-release. → Make the stable active/
  disarmed control block the single runtime authority and verify every consuming path.
- [Risk] Wasm heap storage collides with static data or slice frames. → Publish one target memory
  partition and checked alignment plan before emission and test nested calls plus memory growth.
- [Trade-off] The first allocation change cannot start immediately. → The split prevents four
  foundational language features from being smuggled in as unreviewable allocator exceptions.

## Migration Plan

Archive the four prerequisite changes in the order above. Then implement this change as one vertical
memory capability: layouts and source operations; semantic and ownership rules; HIR/instances;
target layouts; structured MIR and verifier; evaluator oracle; native system provider; Wasm heap;
three-engine failure sweeps; unified inspector. Existing allocation-free programs remain on their
byte-identical paths wherever eager construction of new layout or backend types is unnecessary.

If three-engine typed-failure or cleanup parity cannot be achieved, remove the allocation, slot, and
drop-hook surface together. The archived prerequisite features remain independently useful and are
not part of this rollback boundary.
