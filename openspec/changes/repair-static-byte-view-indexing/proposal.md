## Why

Static byte literals can be passed whole to output operations, but indexing one currently lowers a
slice selector onto a non-slice evaluator value. That makes committed bytes unusable by ordinary
algorithms and blocks static text from serving as a reliable foundation for later String work.

## What Changes

- Make immutable static byte views support ordinary checked indexing and length observation without
  allocation.
- Preserve one canonical slice-like representation from semantic facts through HIR, MIR,
  evaluation, native LLVM, and direct WebAssembly.
- Reject malformed MIR that applies a byte-view selector to an incompatible root while accepting
  valid static-view reads.
- Rewrite CRC-32 to consume an actual `b"..."` literal rather than a substitute fixed byte array,
  keeping its checksum and three-engine parity unchanged.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-static-text`: Specify checked indexing and length access for immutable static byte
  views.
- `bootstrap-mir`: Represent and verify indexed reads from static byte views consistently.
- `bootstrap-evaluation`: Evaluate indexed static bytes with deterministic bounds behavior.
- `bootstrap-backend`: Emit equivalent native and WebAssembly loads from static data.
- `bootstrap-algorithm-examples`: Require CRC-32 to process committed static bytes directly.

## Impact

- Affects static-literal elaboration/lowering, MIR place verification, evaluator values, both
  backends, static-text tests, and the CRC-32 example.
- Introduces no allocation and does not define an owning String representation.
