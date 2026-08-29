## Why

The LLVM data-layout parser rejects valid aggregate-alignment components and discards their semantics, so structure layout can disagree with LLVM about ABI alignment, tail padding, and allocation size. Supporting the pinned LLVM 22.1.8 aggregate rule closes that target-compatibility gap while keeping packed structures and arrays unchanged.

## What Changes

- Parse and retain the final `a:<abi>[:<preferred>]` component, including LLVM's zero/default semantics and validation rules.
- Expose aggregate ABI and preferred alignment on `DataLayout` while preserving the original data-layout bytes for exact rendering.
- Apply the aggregate ABI minimum to unpacked anonymous and named structures, including zero-sized structures and tail padding.
- Add pinned-oracle coverage for absent, zero, nonzero, repeated, and malformed aggregate rules and for affected and unaffected aggregate type layouts.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `llvm-module-declarations`: Define supported aggregate-alignment parsing, observable alignment queries, validation, and structure-layout behavior against the pinned LLVM semantics.

## Impact

- Public `@silklang/llvm/DataLayout` values gain an observable aggregate-alignment specification.
- `DataLayout.parse` validation and `Type.sizeOf`/`Type.alignmentOf` results change for layouts with a nonzero aggregate ABI minimum.
- `packages/llvm/src/DataLayout.ts`, `packages/llvm/src/Type.ts`, their existing tests, and the `llvm-module-declarations` specification are affected.
