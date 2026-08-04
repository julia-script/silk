## Why

A start-to-finish compiler tutorial should show how failures retain phase ownership and how LLVM output choices differ. Learners also need to see that failed body construction is transactional rather than leaving partial IR.

## What Changes

- Add guided lexical, parsing, resolution, arity, and LLVM body-validation failure exercises.
- Render phase-specific diagnostics with source spans or `LlvmError.operation`.
- Explain transactional `Function.buildBody` failure and retry behavior.
- Add a short `Bitcode.encode` variation and distinguish bitcode from bytecode and executables.
- Verify bitcode determinism and magic bytes without making bitcode the primary path.

## Capabilities

### New Capabilities

None. This change adds documentation and tutorial-example material, so the change opts out of behavioral specs with `skip_specs: true`.

### Modified Capabilities

None.

## Impact

Adds tutorial diagnostics, failure fixtures, bitcode explanation, and tests. It consumes existing typed-error and bitcode APIs.

