## 1. Wasm Heap

- [x] 1.1 Lay out the heap as a free-list head table followed by a bump region of 16-byte-header
      blocks, and start the heap pointer past the table.
- [x] 1.2 Synthesize the allocator: serve from the head of the matching size class when it fits,
      otherwise carve a block off the bump region and grow memory only when it runs past what is
      mapped, answering zero on a request that cannot be served.
- [x] 1.3 Synthesize the release: push one block onto the head of its size class, treating a null
      header as a no-op so a union's conditional cleanup can select an inactive context to zero.
- [x] 1.4 Serve requests whose alignment exceeds a block's own 16 bytes, or whose size exceeds the
      largest class, from the irregular list, measuring its head against the request before reuse.

## 2. Reclaim Lowering

- [x] 2.1 Add the `planReclaims` predicate the Wasm backend was missing beside `planHasHook`, so a
      cleanup plan that reclaims without invoking a hook is no longer invisible.
- [x] 2.2 Write the block-header address into the `$context` reclaim-authority lane at `Allocate`
      instead of the constant `0`.
- [x] 2.3 Walk a cleanup plan's lanes to release every block it still owns — allocation, raw buffer,
      hook inner, struct field, array element, effect environment slot, and tag-guarded union case —
      and do the same from an address for a slot drop.
- [x] 2.4 Run the reclaim walk after the existing hook walk at every release site, and delete the
      no-op comment the shortcut was recorded in.

## 3. Acceptance

- [x] 3.1 Assert an interleaved non-LIFO allocate-and-drop loop keeps the final `memory.size` under
      a fixed limit, and that a tenfold cycle count costs no extra page.
- [x] 3.2 Assert Wasm and native LLVM report the same release count, kept separate from the heap
      bound.
- [x] 3.3 Confirm the heap-bound assertions fail on the bump allocator and that the count-parity
      assertion passes on it, so the two tests are pinning different properties.

## 4. Specification

- [x] 4.1 Admit a block-header address as one representation of an allocation's private reclaim
      authority in `bootstrap-owned-allocation`.
- [x] 4.2 State the Wasm reclaim policy and the count-versus-memory distinction in
      `bootstrap-backend`.
