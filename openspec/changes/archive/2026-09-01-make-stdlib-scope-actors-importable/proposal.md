## Why

Documentation currently relies on namespace aliases such as `import silk.raw_buffer as RawBuffer`
to qualify standard-library operations. Replacing those aliases with selected imports exposes an
inconsistent library surface: actor-backed modules work, while intrinsic-backed and utility
modules reject the preferred scoped name as a missing member.

## What Changes

- Add ordinary zero-data scope actors to nonprimitive standard-library modules that currently need
  a namespace alias solely to expose their preferred operation qualifier.
- Preserve existing example qualifiers such as `RawBuffer`, `Slot`, `Format`, `Hash`, `Metrics`,
  `Unicode`, and `UnicodeTables` while replacing their namespace imports with selected scope-actor
  imports.
- Keep primitive modules such as `u8`, `u32`, and `usize` as unaliased namespace imports.
- Retain a namespace alias only when an example intentionally groups multiple independent actors
  beneath one local qualifier and no single scope actor represents that surface.
- Make generated standard-library reference pages show the exact selected scope-actor import for
  nonprimitive modules and the unaliased namespace import for primitives.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-silk-stdlib`: nonprimitive operation modules expose an ordinary importable scope actor
  wherever their preferred public qualifier is not already a declaration.
- `silk-documentation-model`: generated module pages render the canonical import form for primitive
  namespaces and nonprimitive scope actors.

## Impact

This changes ordinary Silk declarations in affected standard-library modules, their generated
embeddings and integrity hashes, documentation rendering, doctest examples, and source-position
goldens. It adds no compiler privilege, intrinsic, runtime representation, compatibility alias, or
backend behavior.
