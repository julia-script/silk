## Why

Silk can now express nominal aggregate values, but it still has no inline indexed storage. Fixed-size
arrays and checked indexing are the smallest next slice that exercises repeated layout, dynamic place
projection, element-derived ownership, and bounds behavior before mutation and loops depend on them.

## What Changes

- Add lossless, recoverable array types such as `Array<I32, 4>`, array literals such as
  `[10, 20, 30, 40]`, and repeated postfix indexing such as `matrix[row][column]`.
- Make the non-negative decimal length part of the canonical array type; infer non-empty literal
  element type and length where context permits, require context for empty literals, and diagnose
  element or length mismatches without discarding independent facts.
- Treat indexing as a canonical place projection. Checked indexing requires `I32`, traps at runtime
  for negative or out-of-bounds dynamic indices, and diagnoses statically known out-of-bounds
  literals before lowering.
- Derive array Copy/move-only classification and cleanup from the element type. Copy elements may be
  read without consuming the owner; a Copy leaf may be projected through an indexed aggregate place;
  moving an individual non-Copy element remains a rejected partial move.
- Extend reachability, target-aware inline layout, compiler-owned calling shapes, MIR, evaluation,
  native LLVM emission, and direct WebAssembly emission for zero-length, scalar, nested, and
  struct-element arrays.
- Expose array syntax, canonical types, element facts, ownership, layout/ABI paths, MIR, bounds
  outcomes, traces, and artifacts through the analysis facade and unified `/labs` workbench.
- Keep mutable bindings, element assignment, loops, slices and ranges, repetition literals, general
  constant expressions, borrowing, unsafe unchecked indexing, pointers, unions, and matching out of
  this change.

## Capabilities

### New Capabilities

- `bootstrap-fixed-arrays`: Canonical fixed-size array types and values, complete literals, checked
  indexed places, element-derived ownership and cleanup, and deterministic runtime realization.

### Modified Capabilities

- `bootstrap-syntax`: Parse and recover array type constructors, array literals, and repeated postfix
  index projections losslessly.
- `bootstrap-name-resolution`: Resolve nested array element types and canonicalize literal decimal
  lengths in field and function contracts.
- `bootstrap-semantic-facts`: Publish immutable array literal mappings, inferred or contextual array
  types, indexed-place facts, bounds knowledge, and explicit unavailable states.
- `bootstrap-hir`: Represent canonical logical array types, complete array construction, and checked
  index-place projection with exact provenance.
- `bootstrap-ownership`: Derive Copy and move-only array behavior from the element type, reject
  partial element moves, and plan index-order recursive cleanup.
- `bootstrap-instances`: Discover reachable array types and their recursively required element types
  from contracts, construction, indexing, and cleanup.
- `bootstrap-target-layout`: Plan inline repeated-element layouts and compiler-owned aggregate lanes
  whose canonical paths include array indices.
- `bootstrap-mir`: Carry logical array types and explicit complete construction, checked index, place
  projection, move, call, return, and drop operations through verified MIR.
- `bootstrap-evaluation`: Evaluate immutable array values and checked indexed places with precise
  traps and deterministic traces.
- `bootstrap-backend`: Realize the shared repeated-element layout and lane plan in native LLVM and
  direct WebAssembly output without backend-selected array policy.
- `bootstrap-compiler-driver`: Compile and differentially verify zero-length, nested, indexed,
  aggregate-bearing, and invalid array programs across supported targets.
- `bootstrap-analysis-facade`: Publish immutable array type, literal, place, ownership, layout, MIR,
  evaluation, and codegen queries without tooling reconstruction.
- `bootstrap-syntax-inspector`: Inspect array values and checked indexing end to end in the unified
  `/labs` workbench with accessible browser-local presets.

## Impact

The compiler's public type vocabulary, syntax tree, semantic facts, HIR, ownership facts, instance
keys, layout paths, MIR, evaluator values, backend realization, driver corpus, facade, deterministic
encodings, and unified labs will expand. The change introduces no dependency or external ABI and
keeps the bootstrap host entry scalar.
