## 1. Add advanced configuration actors

- [x] 1.1 Implement MemoryAccessKind, SyncScope, AtomicOrdering, and atomic operation value actors with closed validated representations.
- [x] 1.2 Implement FastMath and integer wrap/exact flag actors with combinators and exhaustive text/bit mappings.
- [x] 1.3 Implement tail-call, GEP, inline-assembly, and memory-operation option records with smart constructors.
- [x] 1.4 Add invalid flag combination, ordering relation, default value, and formatting tests.

## 2. Implement memory and address instructions

- [x] 2.1 Implement alloca and in-alloca with allocation type, count, alignment, and result pointer validation.
- [x] 2.2 Implement normal and volatile load and store with explicit value types, address spaces, and alignments.
- [x] 2.3 Implement semantic GEP traversal, in-bounds and in-range settings, structured GEP convenience, and result pointer derivation.
- [x] 2.4 Add nested aggregate, vector index, address-space, invalid path, and alignment fixtures.

## 3. Implement aggregate and vector instructions

- [x] 3.1 Implement aggregate construction from validated field values.
- [x] 3.2 Implement extract-element and insert-element with scalar index and vector child validation.
- [x] 3.3 Implement shuffle-vector and splat-vector for fixed and scalable vectors with exact masks.
- [x] 3.4 Complete extract-value and insert-value behavior for every aggregate shape supported by the pinned builder.
- [x] 3.5 Add representative fixed-vector, scalable-vector, nested aggregate, poison-mask, and invalid-shape tests.

## 4. Implement atomic operations

- [x] 4.1 Implement atomic load and store with minimum ordering and alignment validation.
- [x] 4.2 Implement fence with synchronization-scope and ordering validation.
- [x] 4.3 Implement compare-exchange and weak compare-exchange with success/failure ordering checks and canonical result type.
- [x] 4.4 Implement every pinned atomic read-modify-write operation with pointer/value and ordering validation.
- [x] 4.5 Add legal ordering matrix, illegal failure ordering, volatile, weak, scope, alignment, and result-type tests.

## 5. Complete advanced calls and instructions

- [x] 5.1 Implement indirect branch, vararg access, and remaining supported terminator or instruction variants.
- [x] 5.2 Implement inline assembly constants and calls with exact assembly bytes, constraints, dialect, side effects, alignment-stack, and unwind settings.
- [x] 5.3 Complete tail, must-tail, and no-tail calls plus all supported fast-math call variants.
- [x] 5.4 Apply no-wrap, exact, in-bounds, weak, volatile, and fast-math settings to every compatible instruction and reject incompatible settings.
- [x] 5.5 Add fixtures covering each remaining pinned instruction tag and flag combination independently.

## 6. Implement intrinsic resolution

- [x] 6.1 Translate the pinned intrinsic inventory into a static typed catalog of names, signature recipes, overload matches, and attributes.
- [x] 6.2 Implement intrinsic signature instantiation and canonical function declaration reuse.
- [x] 6.3 Implement typed call conveniences for memcpy, memmove, memset, assume-cold, and other conveniences present in the pinned builder.
- [x] 6.4 Add inventory, overloaded name, signature, attribute, invalid overload, and canonical reuse tests for the intrinsic catalog.

## 7. Complete advanced constants and serialization

- [x] 7.1 Implement the remaining supported constant casts, binary expressions, GEP variants, block addresses, local equivalents, no-CFI, and assembly expressions.
- [x] 7.2 Add private record descriptors and adapters for every advanced instruction and constant expression.
- [x] 7.3 Extend IrText with exhaustive advanced operation, option, flag, intrinsic, and constant-expression rendering.
- [x] 7.4 Extend Bitcode with advanced function records, atomic info fields, inline assembly, operand bundles, and constant records.
- [x] 7.5 Add pinned Zig differential fixtures for every advanced record and exact-bit constant case.
- [x] 7.6 Add LLVM round trips combining memory, vectors, atomics, intrinsics, varargs, and inline assembly.

## 8. Publish and verify advanced APIs

- [x] 8.1 Add explicit Intrinsic, MemoryAccess, FastMath, and any other public settings actor subpath exports and root namespaces.
- [x] 8.2 Document advanced memory, vector, atomic, intrinsic, and inline-assembly examples and their validation failures.
- [x] 8.3 Run pnpm typecheck, pnpm exec biome check ., and pnpm test in that order and resolve all change-related failures.
- [x] 8.4 Run pnpm check and pnpm release:candidate and record the successful advanced-IR handoff.
