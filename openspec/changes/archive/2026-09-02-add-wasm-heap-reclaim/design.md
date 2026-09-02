## Context

The Wasm backend's heap is a bump pointer over one private memory. Allocation aligns the pointer up,
grows memory when the request runs past what is mapped, and never lowers the pointer again. Release
emits nothing: the backend's release lowering only walked Drop hooks, and its hook-traversal
predicate has no `AllocationCleanup` case at all, so a bare allocation drop produced no instructions
whatsoever. The native LLVM backend, given the same MIR, calls libc `free` with the allocation's
`$context` lane.

The compiler-planned `Allocation` value already has six lanes — `$base`, `$bytes`, `$alignment`,
`$reclaim`, `$context`, `$active` — and LLVM already treats `$context` as the pointer it hands to
`free`. Wasm wrote the constant `0` there because it had nothing to record.

## Goals / Non-Goals

Goals:

- A released block becomes available to a later request, so an allocate-and-drop loop keeps a
  bounded heap for arbitrary interleaved patterns.
- Alignment guarantees are exactly what the bump allocator gave: a payload satisfies the requested
  power-of-two alignment.
- Reclaim stays driven by the owner that consumed the reclaim ticket, at the point ownership ends.

Non-goals: compaction, defragmentation, a moving allocator, coalescing adjacent free blocks, a
public `free`, or any change to the `Allocator` service contract.

## Decisions

### Decision: a size-class free list, not a LIFO bump unwind

Two allocators satisfy "repeated acquire and release cycles keep a bounded heap" while staying
inside the non-goals. Rolling the bump pointer back when the released block is the most recent one
costs almost nothing and needs no per-block metadata, but it bounds only nested allocation: in an
interleaved pattern, releasing anything but the newest block reclaims nothing. A size-class free
list needs a header per block, and bounds arbitrary interleavings.

The free list is chosen. A self-hosted compiler's allocation traffic is interleaved — an AST node
outlives the token buffer that produced it — so a bound that holds only for stack-shaped programs
would not survive the first real workload. The acceptance suite pins this directly with a non-LIFO
allocate-and-drop loop, kept separate from the count-parity test.

### Decision: `$context` carries a block-header address

Every block is a 16-byte header followed by its payload, so a payload always begins exactly one
header past its block and release recovers the block by subtraction. The allocation's `$context`
lane carries that header address, which is what the LLVM backend already does with the same lane for
libc's benefit. This is the change that needs a spec amendment: `bootstrap-owned-allocation` said
the lane holds "everything required for infallible release" without saying whether a backend may
make that a pointer into its own heap.

The lane stays compiler-private. It has no Silk-visible name, no Silk-visible type, and no
construction path; a program can neither read it nor forge one.

### Decision: classes are powers of two, with one irregular list

Payload capacities are `1 << (4 + index)` from 16 bytes up to 1 GiB, giving 27 classes. The free
list heads live in a fixed table at the base of the heap region, which wasm memory already zeroes,
so a head is indexable from a class computed at run time — a wasm global cannot be.

Every classified block starts 16-byte aligned and so has a 16-byte-aligned payload, which is what
lets a block be reused by any later request in its class without re-checking alignment. A request
whose alignment exceeds 16 bytes, or whose size exceeds the largest class, is served from a single
irregular list whose head is measured against the request before it is reused. `Layout` admits any
power-of-two alignment at run time, so the irregular path is a correctness requirement, not a
convenience.

### Decision: hooks first, then reclaim

The Wasm hook walk works from byte offsets into a frame materialization, because a Drop hook takes
`&mut self` and can write through it. The reclaim walk works from the owner's lanes, because that is
where `$context` lives. Rather than fuse them, release runs the existing hook walk, which reloads
the owner's slots afterwards, and then the reclaim walk over those reloaded slots. A hook therefore
observes the block before it is released and can still hand back a different one, which is the same
order the LLVM backend's single lane-driven walk produces.

A null header is a no-op on release. That is what lets a union's conditional cleanup select an
inactive case's reclaim context to zero rather than branch around the call — the shortcut the LLVM
backend already takes through libc `free`, whose contract also ignores null.

## Risks / Trade-offs

Adjacent free blocks are never coalesced, so a program that allocates a large block, releases it,
and then allocates many small ones holds the large block's capacity in its own class rather than
splitting it. The heap stays bounded for repeated patterns, which is the property claimed;
worst-case fragmentation across changing size mixes is not bounded, and buying that back means
either splitting and coalescing or a moving allocator, which the issue puts out of scope.

The free-list head table sits at the base of the heap region, which the shadow stack can already
run into on deep enough recursion — the heap began at that same address before this change, so the
collision is not new, but the table is now the first thing such an overflow would reach. Bounding
the shadow stack is a separate concern from bounding the heap and is not addressed here.

The private memory is now exported unconditionally rather than only when a host write needs it. That
is how a host observes the heap at all — including how the acceptance test reads `memory.size` — and
it matches the existing rule that every function is exported so the artifact is directly
instantiable for inspection.
