## Why

Silk raw pointers can name record types, but ordinary structs intentionally promise no layout to C. That prevents foreign functions such as `clock_gettime` from safely filling a record that Silk can then read, blocking the next foreign-function and standard-library migration work.

## What Changes

- Add `[pub] extern "C" struct` as the explicit, non-generic C-layout record declaration.
- Retain the ABI on struct facts and validate that every field has a supported C object representation.
- Give C-layout records declaration-order C aggregate size, alignment, offsets, and padding for the selected target while preserving ordinary Silk construction, field access, ownership, and visibility.
- Keep aggregates excluded from foreign signatures by value and keep raw pointers admitted without inspecting their pointee; the C-layout marker is the guarantee that native code may interpret record fields.
- Add focused parser, formatter, semantic, layout, LLVM/native, and `clock_gettime` acceptance evidence plus reference and diagnostic documentation.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax`: recognize the ABI-bearing struct declaration form without weakening declaration recovery.
- `bootstrap-struct-types`: record and validate non-generic C-layout struct declarations and their field subset.
- `bootstrap-semantic-facts`: retain the C-layout ABI as a declaration-owned semantic fact.
- `bootstrap-module-semantic-surface`: preserve the C-layout marker across public module-surface encoding and import.
- `bootstrap-target-layout`: expose the selected target's C aggregate placement for C-layout records.
- `bootstrap-foreign-functions`: define record-pointee interoperability while preserving opaque raw-pointer admission and by-value exclusion.
- `bootstrap-backend`: preserve the target layout when native code reads or writes a C-layout record through a pointer.
- `bootstrap-diagnostics`: publish stable diagnostics for invalid C-layout declarations and fields.
- `silk-source-formatting`: format C-layout declarations canonically and idempotently.

## Impact

The change affects compiler syntax and formatting, declaration facts and completion, target layout planning, foreign ABI metadata, native lowering evidence, compiler tests/corpus, OpenSpec main specifications, and the language reference. It adds no compiler-known standard-library actor, runtime shim, or compatibility path.
