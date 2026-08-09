## Why

Programs need stable text and byte constants before they can produce observable output, but quoted literals should not prematurely fix Silk's eventual owning `string` representation. Static immutable data provides the narrow bridge while leaving target-native String integration open.

## What Changes

- Add lossless quoted UTF-8 text and byte-string literal syntax with deterministic escape diagnostics.
- Decode literals to immutable program-lifetime data with `usize` byte lengths and no allocation.
- Carry target-neutral static data through HIR, MIR, layout, evaluation, native emission, and WebAssembly emission.
- Expose immutable byte views without defining allocation, mutation, concatenation, or the public layout of an owning `string`.

## Capabilities

### New Capabilities

- `bootstrap-static-text`: Immutable static UTF-8 and exact byte literal values independent of the future owning String model.

### Modified Capabilities

- `bootstrap-syntax`: Parse text and byte-string tokens, escapes, and recovery losslessly.
- `bootstrap-hir`: Carry decoded static-data identities and logical views.
- `bootstrap-mir`: Represent deterministic static data and immutable byte views.
- `bootstrap-target-layout`: Plan target placement and `usize` lengths without exposing a String ABI.
- `bootstrap-evaluation`: Evaluate exact static bytes and sharing-independent identity.
- `bootstrap-backend`: Emit equivalent native and WebAssembly static data.

## Impact

This adds literal syntax and static data to the complete compilation pipeline and backend artifacts. It deliberately does not add an owning or growable String, formatting, interning guarantees, or target-native JS-string representation.
