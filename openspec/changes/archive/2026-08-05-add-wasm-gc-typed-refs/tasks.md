# Tasks — add-wasm-gc-typed-refs

## 1. Reference-type model

- [x] 1.1 Restructure `ValType.RefType` to `{ nullable, heapType }` with abstract/concrete heap
      types, shorthand constants preserved, and `ref`/`refNull` constructors
- [x] 1.2 Update every consumer the compiler flags (validator, emitters, const exprs, elem/
      table/global declarations) to the new representation; shorthand encodings byte-stable
- [x] 1.3 Type names: optional names on type definitions, name-section subsection 4, text
      identifiers

## 2. Composite types and rec groups

- [x] 2.1 Generalize the type table to canonicalized recursive groups (singleton groups for
      `Type.func`, unchanged public behavior)
- [x] 2.2 Add `Type.struct` and `Type.array` with storage types, mutability, supertypes, and
      finality; supertype structural checks
- [x] 2.3 Add `Type.rec` with forward handles, atomic commit, canonical dedup, and stale-handle
      invalidation; interning tests including equivalent-group dedup

## 3. Instructions

- [x] 3.1 Table rows and `Instr` variants for struct/array/i31/convert instructions
- [x] 3.2 Variants for casts (`ref.test`/`ref.cast` and `br_on_cast*` with reference-type
      immediates), null handling (`br_on_null`/`br_on_non_null`/`ref.as_non_null`), and
      `call_ref`/`return_call_ref`
- [x] 3.3 Generalize `ref.null` to heap-type immediates and `ref.func` to precise typing

## 4. Subtyping and validation

- [x] 4.1 Implement `internal/Subtype.ts` (`matches`) with direct unit tests per hierarchy edge
- [x] 4.2 Swap the validator's equality checks for `matches`; defaultability checks for locals
      and `_default` allocators
- [x] 4.3 Per-instruction typing procedures for GC instructions and casts; unit tests per rule

## 5. Emitters

- [x] 5.1 Type-section encodings (rec/sub/composite with shorthand minimization) and
      parameterized reference/heap-type encodings
- [x] 5.2 Text rendering for type definitions, reference types, and new instructions
- [x] 5.3 Byte-stability: all existing fixtures verify unchanged

## 6. Parity and release

- [x] 6.1 Oracle features + `gc` fixture + exhaustive rows + negative corpus per new rule
- [x] 6.2 README/UPSTREAM updates declaring the Chrome baseline complete; JSDoc; changeset
