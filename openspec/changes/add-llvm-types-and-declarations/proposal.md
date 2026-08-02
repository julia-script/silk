## Why

A minimal bitstream cannot represent useful programs until the builder can intern LLVM types and describe module-level entities. This change supplies the stable module vocabulary on which function bodies and metadata depend.

## What Changes

- Add data-layout parsing and queries, explicit target triples, alignments, address spaces, calling conventions, linkage, visibility, storage, and thread-local models.
- Add structurally interned primitive, integer, pointer, function, vector, array, anonymous structure, named structure, opaque, and target-extension types.
- Add attributes and function-attribute groups with deterministic canonicalization.
- Add exact constants, including arbitrary-width integers, raw floating-point bit patterns, aggregates, strings, null, zero, undef, poison, and supported constant expressions.
- Add globals, variables, aliases, function declarations, and their mutation/query operations.
- Extend textual IR and bitcode output for every declaration introduced here and validate the output with LLVM tools.

## Capabilities

### New Capabilities

- `llvm-module-declarations`: Module types, constants, attributes, and global declarations with valid LLVM serialization.

### Modified Capabilities

None.

## Impact

This adds public actor modules such as `DataLayout`, `Type`, `Attribute`, `Constant`, `Global`, `Variable`, `Alias`, and `Function`, plus the corresponding package subpath exports. It depends on `establish-llvm-builder-foundation`. Host-target inference is intentionally excluded: callers provide a target triple and data-layout string, with any convenience target table remaining explicit and platform-neutral.
