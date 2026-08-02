## Context

Core function bodies establish instruction storage, validation, numbering, and serialization. This change extends those seams to the remaining low-level operations in the pinned builder. The main complexity is the interaction among data layout, pointer address spaces, operation flags, atomic ordering, overloaded intrinsic signatures, and specialized bitcode records.

## Goals / Non-Goals

**Goals:**

- Complete the executable and constant-expression operation families supported by the pinned builder.
- Express configuration as validated data rather than combinatorial method names.
- Preserve exact memory semantics, flags, types, and record encodings.

**Non-Goals:**

- Add operations absent or marked TODO in the pinned Zig baseline.
- Automatically optimize, fold, or legalize caller-provided LLVM IR.
- Generate intrinsic tables from Zig during package build or at runtime.

## Decisions

### Use configuration actors for orthogonal flags

MemoryAccess, FastMath, and related small actors define immutable validated settings such as volatility, alignment, synchronization scope, ordering, no-wrap, exactness, tail kind, and GEP kind. FunctionBody operations accept those values rather than exposing one function for every flag combination. Encoder adapters map semantic settings to pinned record tags and bit fields.

Alternative considered: mirror Zig's combined instruction tags publicly. That makes invalid combinations hard to prevent and multiplies the API surface.

### Centralize type rules with the owning operation family

Memory rules remain in MemoryAccess, intrinsic signatures in Intrinsic, constants in Constant, and instruction construction in FunctionBody. Shared facts are exported as narrow actor queries rather than a generic validation helper module. Each operation validates before appending to the draft.

### Traverse aggregate types semantically for GEP

GEP construction walks the declared source type using exact constant indices where LLVM requires them and derives the result pointer address space from the base. The operation records the semantic source type and indices; text and bitcode adapters independently encode that record. Structured GEP is a convenience over the same validated path.

### Encode atomic legality as data

Ordering enums retain their LLVM ordering relation. Constructors validate permitted success/failure pairs, operation-specific minimum ordering, pointer/value compatibility, and alignment. Compare-exchange returns the canonical two-field aggregate type through normal type interning.

### Maintain a static typed intrinsic catalog

Intrinsic contains pinned names, signature recipes, overload relationships, and canonical attributes as readonly TypeScript data. Resolution instantiates a recipe through existing type and attribute actors and reuses the global function declaration. A development audit compares the catalog inventory to pinned Zig, but production build and runtime never execute Zig.

Alternative considered: generate source during every build. That would make package builds depend on Zig and obscure reviewed API changes.

### Keep assembly as exact bytes

Assembly and constraints use ByteString, with options represented independently. Text rendering applies LLVM escaping at the edge; bitcode uses the original bytes. No parsing beyond constraints enforced by the pinned builder is attempted.

### Measure only completed hot loops

Construction and encoding stay traced by default. Only the per-instruction dispatch or per-word bit loop may become Effect.fnUntraced, and only after a benchmark demonstrates tracing overhead on a representative module.

## Risks / Trade-offs

- [The intrinsic catalog is large and drift-prone] → Keep it declarative, exhaustively audited, and covered by generated development fixtures.
- [LLVM atomic constraints evolve] → Pin behavior to the compatibility baseline and record changes through the upstream-update process.
- [Flag records can admit meaningless combinations] → Provide smart constructors with private representations instead of arbitrary object literals.
- [Advanced fixtures become combinatorial] → Cover each independent dimension plus selected interactions, then rely on parity inventory rather than a Cartesian product.

## Migration Plan

Apply after add-llvm-core-function-bodies. Add settings actors first, then memory, aggregate and vector operations, atomics, calls and assembly, constants, and the intrinsic catalog. Each family must pass text, bitcode, differential, and LLVM validation before its tasks are complete. Rollback leaves core function construction intact.
