## Why

The Wasm backend allocates but never reclaims. Allocation is a bump pointer over `memory.grow`, and
the release path emits nothing at all, so a program that acquires and drops in a loop grows the Wasm
heap without limit while the same program on native LLVM holds a flat peak through libc `free`.

Two things kept that from being a simple bug fix.

- No spec states a Wasm reclaim policy. `bootstrap-backend` says only that "physical reclamation
  policy may differ" between the backends, which is true and says nothing about whether a heap must
  stay bounded. The single record of the shortcut was a code comment.
- A free list needs somewhere to put per-block metadata. `bootstrap-owned-allocation` requires an
  `Allocation` to carry "private unforgeable active reclaim authority containing everything required
  for infallible release" but never says whether a backend may represent that authority as a pointer
  into its own heap. The Wasm backend wrote the constant `0` into that lane precisely because it had
  nothing to say there.

Both are answered here so the implementation has a contract to be checked against rather than a
comment to be trusted.

## What Changes

- State that a backend's reclaim authority is opaque backend-chosen data, and that representing it
  as the address of a backend-private block header is one admitted representation. The authority
  stays compiler-private and unforgeable either way: no Silk program can read, write, or construct
  it, and no public `free` appears.
- State the Wasm reclaim policy: released storage returns to the heap that issued it, and repeated
  acquire and release cycles keep a bounded heap for arbitrary interleaved patterns, not only for
  nested ones. Bounding only stack-shaped allocation would be met by rolling a bump pointer back on
  the most recent block, which is not what a self-hosted compiler's allocation traffic looks like.
- Keep reclaim ownership-driven. No scheduler, collector, background task, compaction, or moving
  allocator, and no change to the `Allocator` service contract.

## Capabilities

### Modified Capabilities

- `bootstrap-owned-allocation`: admit a block-header address as one representation of the private
  reclaim authority an `Allocation` carries.
- `bootstrap-backend`: require the WebAssembly heap to reclaim released blocks and keep a bounded
  heap under arbitrary interleaved acquire and release, while count parity with native LLVM — which
  already holds — stays a separate property from memory parity.
