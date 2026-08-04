## Why

Core SSA operations cover simple functions but not the memory, vector, atomic, intrinsic, and low-level operations needed by a real compiler backend. Completing these instruction families as one vertical change keeps their type rules and binary encodings synchronized.

## What Changes

- Add alloca, load, store, GEP, structured GEP, aggregate construction, and address-space-aware memory operations.
- Add vector extraction, insertion, shuffle, splat, and the remaining aggregate operations.
- Add volatile and atomic loads/stores, fences, compare-exchange, atomic read-modify-write, synchronization scopes, orderings, and alignments.
- Add indirect branches, varargs, inline assembly, tail-call kinds, fast-math/no-wrap/exact flags, and remaining supported instruction variants.
- Add intrinsic signature resolution and conveniences for memory intrinsics while retaining an explicit declaration API.
- Complete the corresponding constant expressions, textual forms, bitcode records, validation rules, and differential fixtures.

## Capabilities

### New Capabilities

- `llvm-advanced-ir`: Advanced memory, aggregate, vector, atomic, assembly, and intrinsic IR construction and serialization.

### Modified Capabilities

None.

## Impact

This expands `FunctionBody`, `Constant`, `Type`, `Attribute`, `IrText`, and `Bitcode`, and may add small value actors for memory-access and fast-math settings. It depends on `add-llvm-core-function-bodies`. Per-instruction and per-bit inner loops may use documented imperative mutation behind `Effect.fnUntraced` only where the implementation identifies them as performance-critical.
