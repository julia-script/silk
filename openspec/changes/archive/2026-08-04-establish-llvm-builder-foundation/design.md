## Context

See proposal.md for motivation and specs/llvm-builder-foundation/spec.md for observable behavior. The package currently exports nothing, uses strict TypeScript and Effect 4, and has no compatibility surface. The upstream implementation assumes Zig allocators, packed integers, compile-time reflection, and mutable array-backed tables; those mechanisms cannot be copied directly into TypeScript. LLVM bitcode is a little-endian stream of 32-bit words whose records are described by abbreviations and nested blocks.

## Goals / Non-Goals

**Goals:**

- Establish the public data-first Effect conventions that all later actors follow.
- Keep mutable compiler state private, deterministic, owner-safe, and fiber-safe.
- Implement the generic bitstream format once and prove it through a minimal module.
- Make exact bytes and wide integers first-class instead of retrofitting them later.

**Non-Goals:**

- Expose raw bitstream records or the translated ir.zig schema publicly.
- Infer a host LLVM target, write files, invoke compiler tools, or bind native LLVM.
- Model every low-level arithmetic or bit operation as a separately composed Effect.
- Add useful declarations, function bodies, or debug metadata beyond a valid empty module.

## Decisions

### Use an explicit opaque Builder value

Builder.make returns an opaque data value through Effect. Sibling operations take the builder first and use Function.dual where a pipeable form improves composition. The builder is not a global Context service: explicit ownership makes multiple simultaneous modules, tests, and cross-builder validation straightforward.

Alternative considered: provide one Builder service in the Effect environment. This obscures which module owns a handle and makes nested or concurrent module construction harder to type and test.

### Serialize mutations behind one builder gate

Each builder owns a single-permit Effect semaphore and private mutable state. Public mutations acquire the permit, validate all inputs, apply one synchronous state transition, and release the permit. Internal actor functions receive already-locked state so nested operations never reacquire the gate. Read-only snapshots used by renderers are taken under the same gate.

Alternative considered: immutable copies in Ref. Copying growing interning tables for every instruction is prohibitively expensive. Ungated mutable tables would make fiber interleavings unsafe.

### Separate public handles from compact internal indices

Public handles are opaque records carrying an unforgeable owner token and a compact index. Internal tables store numeric indices only. Every public boundary checks the owner token before looking up an index. This trades a small boundary allocation for deterministic rejection of cross-module misuse; hot internal loops do not allocate handle objects.

Alternative considered: branded numbers. Branding is erased at runtime and cannot detect a same-index handle from another module.

### Treat bytes and bigint as canonical representations

A ByteString actor owns immutable bytes and offers explicit UTF-8 construction. LLVM-facing names and strings use bytes internally. Values wider than the safe JavaScript integer range use bigint; number conveniences validate that the input is an integer within the safe range.

### Implement Bitstream as a private deep module

Bitstream owns a growable array of unsigned 32-bit words, the pending bit buffer, block stack, and backpatch positions. It accepts number for validated small fields and bigint for arbitrary VBR input, writes words through unsigned arithmetic, and materializes the final Uint8Array with explicit little-endian DataView writes. The tight write loops are synchronous internals inside one Effect operation.

Alternative considered: stream bytes directly. Block lengths require backpatching at 32-bit word offsets, so a word buffer is simpler and matches the format.

### Replace Zig reflection with declarative record schemas

The translation of ir.zig is a private set of readonly block and record descriptors. Each descriptor declares its record code and operand encodings; a typed adapter supplies values and module-dependent bit widths. The schema is data, while Bitstream owns all encoding behavior.

Alternative considered: one hand-written encoder per record. That duplicates abbreviation logic and makes parity auditing difficult.

### Keep outputs data-only

IrText.render returns a string and Bitcode.encode returns Uint8Array. Neither owns filesystem behavior. This keeps the package platform-neutral and avoids adding a platform runtime dependency.

### Use checked-in differential fixtures

Development scripts may use the pinned Zig source and LLVM command-line tools to generate and validate fixtures, but normal package tests consume checked-in fixtures and never require those tools. A separate compatibility command runs llvm-as, llvm-dis, the verifier, and llvm-bcanalyzer with the pinned CI toolchain.

## Risks / Trade-offs

- [Effect 4 is currently beta] → Keep Effect use behind actor boundaries and verify against the locked workspace version on every change.
- [Public owner-bearing handles allocate objects] → Keep numeric indices internally and benchmark boundary allocation before considering a compact representation.
- [JavaScript bitwise operators are signed 32-bit] → Normalize every word to unsigned and use bigint where shifts may exceed 31 bits.
- [Concurrent operation order depends on scheduling] → Guarantee atomicity and determinism for committed order, not scheduler-independent order for racing mutations.
- [Pinned upstream code can drift] → Store the commit, source URLs, and source hashes beside the fixtures and update them only through the parity workflow.

## Migration Plan

There is no existing LLVM API to migrate. Introduce the error, byte-string, builder, text, and bitcode actors with explicit root namespaces and deep exports; update the release-candidate test from an empty export list to the new contract. Rollback consists of removing the new exports and files before any published non-zero release.
