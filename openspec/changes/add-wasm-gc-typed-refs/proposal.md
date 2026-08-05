# Add GC and typed function references to `@silk-effect/wasm`

## Why

GC with typed function references is the last feature separating the package from its declared
destination — everything Chrome ships unflagged. Unlike every prior change, it is not additive
table rows: it generalizes the type system itself (parameterized reference types, recursive
type groups, subtyping), which is why it was deliberately sequenced last, once every other
feature had hardened the seams it touches.

## What Changes

- **Reference types become parameterized**: a reference type is `ref null? <heaptype>`, where a
  heap type is either abstract (`any`, `eq`, `i31`, `struct`, `array`, `func`, `extern`,
  `exn`, and the bottom types `none`, `nofunc`, `noextern`, `noexn`) or a concrete type handle.
  `funcref`/`externref`/`exnref` remain as canonical shorthands. **BREAKING** for the
  `ValType.RefType` representation (allowed while unreleased).
- **Composite types**: struct types (fields with storage types `i8`/`i16`/valtype and
  mutability) and array types join function types in the type section; types may declare
  supertypes and finality, and mutually recursive types are defined in recursive groups with
  iso-recursive structural canonicalization.
- **Subtyping**: validation replaces exact type equality with the specification's subtype
  judgment across the heap-type hierarchy and declared supertypes.
- **Instructions (~45)**: `struct.*`, `array.*` (including data/elem-segment initializers),
  `ref.eq`, `ref.test`/`ref.cast` (nullable variants), `br_on_cast`/`br_on_cast_fail`,
  `br_on_null`/`br_on_non_null`, `ref.as_non_null`, `ref.i31`/`i31.get_s`/`i31.get_u`,
  `any.convert_extern`/`extern.convert_any`, and typed calls `call_ref`/`return_call_ref`.
- **`ref.func` becomes precisely typed**, producing a concrete `(ref $t)` that subtyping widens
  to `funcref` wherever the baseline behavior expected it.
- Tables, globals, locals, block types, and element segments accept the generalized reference
  types.
- Parity: oracle features gain `gc` and `function-references`; fixtures and negative corpus
  cover composite types, casts, and subtyping; existing fixtures stay byte-identical.

## Capabilities

### New Capabilities

None — all changes extend or generalize existing capabilities.

### Modified Capabilities

- `wasm-module-declarations`: the type-interning requirement generalizes from function types to
  recursive groups of function, struct, and array types with supertypes; typed reference types
  become declarable on tables, globals, and element segments.
- `wasm-function-bodies`: GC and typed-call instruction coverage; validation gains the subtype
  judgment, cast typing, and non-null reference tracking.
- `wasm-output`: type-section encodings for recursive groups, sub types, and composite types;
  parameterized reference-type and heap-type encodings; text forms for all of the above.
- `wasm-builder-parity`: oracle features extended with GC; fixture inventory and negative
  corpus grow accordingly.

## Impact

- `packages/wasm` only, but touching its deepest layer: `ValType`, `Type`, the interning
  tables, the validator's type comparison, and both emitters. All prior feature families are
  regression-gated by byte-stable fixtures.
- The pinned `wasm-tools 1.255.0` oracle fully supports GC; no dependency changes.
- Public API: `ValType` reference construction changes shape; `Type` gains `struct`, `array`,
  and recursive-group definition; `Instr` gains the new constructors.
