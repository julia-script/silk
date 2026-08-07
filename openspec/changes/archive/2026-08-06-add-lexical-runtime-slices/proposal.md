## Why

Silk functions can currently consume only arrays whose length is part of their type, so the
accepted compiler-shaped fold still bakes one cardinality into its contract and backend calling
shape. Lexical runtime slices are the first memory capability needed to borrow source-dependent
input without introducing allocation, public pointers, or collection behavior.

## What Changes

- Add explicit shared `&[T]` and exclusive `&mut [T]` parameter types for ordinary functions.
- Add explicit call-scoped whole-array borrows and compatible call-scoped reborrows, with no
  implicit array decay, escaping borrows, or standalone slice bindings in the first slice.
- Add runtime slice length and checked indexed-place behavior, including write-through replacement
  through exclusive slices and rejection of moves from borrowed non-Copy elements.
- Track root-attached lexical loans through semantic facts, ownership, HIR, and the structured MIR
  DAG without emitting runtime borrow tokens.
- Give logical slices one compiler-owned, target-aware address-and-length layout and calling shape;
  keep the address lane internal rather than exposing a safe Silk pointer type.
- Add authoritative addressable backing storage for borrowed arrays in the evaluator, native LLVM,
  and direct Wasm lowering, including private Wasm linear-memory frames for address-taken locals.
- Generalize the canonical coverage fold to one shared-slice function called with different array
  lengths, add exclusive write-through acceptance, and expose the new facts and representations in
  the unified `/labs` inspector.
- Keep source-visible length as `I32` for this bootstrap change, with non-negative representability
  enforced and an intentional later breaking migration to pointer-width `USize`.

## Capabilities

### New Capabilities

- `bootstrap-runtime-slices`: Explicit whole-array shared and exclusive lexical slices, their safe
  operations, call-scoped lifetime boundary, and runtime behavior.

### Modified Capabilities

- `bootstrap-syntax`: Parse and recover slice parameter types and explicit borrow expressions.
- `bootstrap-semantic-facts`: Publish resolved slice types, borrow/reborrow facts, dynamic lengths,
  and borrowed indexed places without fabricating unavailable operations.
- `bootstrap-ownership`: Enforce root-attached shared and exclusive loans, call overlap, non-escape,
  and cleanup ordering across every structured exit.
- `bootstrap-hir`: Retain typed slice formation, access mode, source-place provenance, length, and
  borrowed-place operations.
- `bootstrap-instances`: Treat slice element types canonically while keeping array length out of a
  slice-taking function's instance key.
- `bootstrap-mir`: Represent slice loans and operations in the target-aware structured control DAG
  and verify their provenance, types, bounds, and region endings.
- `bootstrap-target-layout`: Plan one target-specific slice representation and heterogeneous
  address-plus-length calling shape before backend emission.
- `bootstrap-evaluation`: Evaluate slices through stable backing-place identity and preserve
  exclusive write-through behavior.
- `bootstrap-backend`: Materialize address-taken arrays and lower the compiler-planned slice shape
  consistently in native LLVM and direct WebAssembly.
- `bootstrap-compiler-driver`: Require multi-length shared-slice and exclusive write-through parity
  across logical evaluation, native execution, and Wasm execution.
- `bootstrap-syntax-inspector`: Show slice syntax, semantic loans, ownership regions, HIR, layout,
  MIR, evaluation, and backend realization in the unified workbench.

## Impact

- Compiler syntax, semantic types, ownership analysis, HIR, instance discovery, target layout, MIR,
  verifier, evaluator, native backend, and Wasm backend under `packages/compiler`.
- Backend calling-lane APIs become heterogeneous so native addresses are not represented as `I32`;
  emitted Wasm gains a private deterministic frame strategy for address-taken fixed arrays.
- Compiler fixtures and three-engine determinism tests, including the existing algorithmic coverage
  fold and focused exclusive-slice programs.
- `/labs` presets and unified inspector projections under `apps/docs`.
- No allocator, owned dynamic sequence, raw-pointer source API, bulk memory primitive, range,
  subslice, iterator, `flow fn` capture, general `&T`, or standalone slice-local capability is added.
