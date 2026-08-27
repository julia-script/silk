## Why

`@silklang/llvm` is currently blank, so the port needs a stable Effect-native foundation before LLVM concepts are added. Establishing the builder lifecycle, binary representation, upstream baseline, and differential test oracle first prevents later changes from inheriting accidental Zig-shaped APIs or unverifiable bit encodings.

## What Changes

- Pin the semantic baseline to Zig master commit `6db520a4cd1ce2391c79d0d55b2b2d5297e133a3` and record the provenance of the derived implementation.
- Introduce an opaque, deterministic Builder session whose public mutations and validation are expressed with `Effect.fn` and the repository-wide `SilkError`.
- Define byte-oriented names, branded module-owned handles, wide-integer conventions, builder lifecycle rules, and safe behavior for concurrent fibers.
- Add the private LLVM bitstream primitives required by `bitcode_writer.zig`: fixed-width fields, VBR fields, Char6, blobs, abbreviations, nested blocks, alignment, and length backpatching.
- Encode and render a minimal valid LLVM module, returning `Uint8Array` bitcode and textual IR without filesystem or native LLVM dependencies.
- Add the differential validation harness used by all later changes, including Zig fixtures and LLVM tool validation.

## Capabilities

### New Capabilities

- `llvm-builder-foundation`: Effect-native builder sessions and minimal deterministic LLVM text and bitcode output.

### Modified Capabilities

None.

## Impact

This creates the first public API and subpath exports in `packages/llvm`, adds package tests and derived-code attribution, and establishes internal bitstream and record-schema modules. The package retains `effect` as its only runtime dependency and does not require Zig or LLVM in production; Zig and LLVM command-line tools are development test oracles only. Every later LLVM proposal depends on this change.
