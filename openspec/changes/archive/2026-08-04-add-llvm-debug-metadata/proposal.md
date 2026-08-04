## Why

Production compiler output needs source locations and debug descriptions in addition to executable instructions. LLVM metadata also introduces distinct interning, forward-reference, attachment, and stripping semantics that warrant an isolated change.

## What Changes

- Add metadata strings, tuples, constants, named metadata, distinct nodes, optional references, and resolvable forward references.
- Add debug files, compile units, subprograms, lexical blocks, locations, basic and composite types, derived types, subroutine types, enumerators, subranges, expressions, and local/global variables.
- Attach debug locations and metadata kinds to globals and instructions while preserving deterministic metadata numbering.
- Add explicit stripped and debug-preserving builder modes with no dangling metadata references in either mode.
- Extend textual IR and bitcode output for metadata blocks, attachment blocks, and named metadata, with LLVM validation and Zig differential fixtures.

## Capabilities

### New Capabilities

- `llvm-debug-metadata`: Interned LLVM metadata, debug information, attachments, forward references, and stripping.

### Modified Capabilities

None.

## Impact

This adds a public `Metadata` actor and expands `Builder`, `Global`, `FunctionBody`, `IrText`, and `Bitcode`. It depends on `add-llvm-advanced-ir` so metadata attachments can cover the final instruction surface without later lifecycle redesign.
