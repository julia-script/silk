# Design — add-wasm-gc-typed-refs

## Context

Final change in the `@silk-lang/wasm` series. Unlike the prior additive changes, GC
generalizes the package's deepest structures: the reference-type representation, the type
interning tables, and the validator's notion of type compatibility. Every prior feature family
is regression-gated by byte-stable fixtures, and the oracle (pinned `wasm-tools 1.255.0`)
fully supports GC. See proposal.md for scope.

## Goals / Non-Goals

**Goals:**

- One reference-type representation that makes the baseline shorthands
  (`funcref`/`externref`/`exnref`) ordinary values of the general form, so all existing generic
  code (select typing, `ref.null`, locals, globals) generalizes rather than special-cases.
- Iso-recursive canonicalization as the single identity mechanism for all types — function
  types included — so handle equality remains the package's type-equality primitive.
- The subtype judgment as one internal function (`matches`) swapped in where `ValType.equals`
  was the validator's comparison.
- Byte-stability: modules using only pre-GC features emit identical bytes (the shorthand
  encodings `0x70`/`0x6f`/`0x69` are preserved for the abstract nullable shorthands).

**Non-Goals:**

- No shared-everything-threads, custom descriptors, or any post-3.0 GC extension.
- No host-side allocation or JS-API concerns; the package only describes types and code.
- No optimization of casts or redundant-cast elimination.

## Decisions

### 1. `RefType` becomes `{ nullable, heapType }` with preserved shorthands

```
RefType   = { _tag: 'Ref'; nullable: boolean; heapType: HeapType }
HeapType  = { _tag: 'Abstract'; kind: AbstractKind } | { _tag: 'Concrete'; type: Type.Type }
AbstractKind = 'any' | 'eq' | 'i31' | 'struct' | 'array' | 'func' | 'extern' | 'exn'
             | 'none' | 'nofunc' | 'noextern' | 'noexn'
```

`ValType.funcref` et al. remain exported, now defined as nullable abstract references, and
`ValType.ref(heapType)` / `ValType.refNull(heapType)` construct the general forms. Encoding
special-cases the nullable-abstract shorthands to their one-byte forms, which is exactly what
preserves byte stability. Alternative — keeping legacy variants alongside a new
parameterized variant — rejected: two representations of `funcref` would force equality and
subtyping to normalize everywhere.

### 2. All type definitions live in rec groups; singletons are groups of one

Internally the type table becomes a table of canonicalized recursive groups; `Type.func`
becomes a singleton-group definition and keeps its exact signature and behavior. New surface:

- `Type.struct(builder, { fields, supertype?, final? })`, `Type.array(builder, { field, … })`
  for non-recursive definitions (singleton groups).
- `Type.rec(builder, count, define)` for mutually recursive groups: `define` receives an array
  of `count` forward handles usable inside field/signature definitions and returns the `count`
  definitions; the group commits atomically, canonicalizes, and returns the final handles.

Canonicalization follows the specification: each group is keyed by its closed structural form
(members' shapes with intra-group references replaced by relative indices and external
references by their canonical indices). Equal keys reuse the existing group's handles — which
is precisely today's interning behavior, generalized. Forward handles that escape the `define`
callback are invalidated at commit (owner-checked as stale), so no half-defined type leaks.

### 3. Subtyping is one internal judgment

`internal/Subtype.ts` implements `matches(state, sub, super)` per the specification: numeric
and vector types by equality; references by nullability implication plus heap-type subtyping
(abstract hierarchy edges, concrete-to-abstract classification, concrete-to-concrete via
declared supertype chains — declared, not structural, per the spec). The validator's `popVal`
expectation check and `popVals` call it; everything else (frames, polymorphism) is untouched.
Bottom (`unknown`) stays the validator-local stack concept it already is.

### 4. Validation additions ride existing machinery

- Casts: `ref.test`/`ref.cast` require the cast target and operand to share a hierarchy;
  `br_on_cast`/`br_on_cast_fail` take two reference-type immediates and check both the cast
  relation and the target label per the spec's typing rules.
- `call_ref $t` pops `(ref null $t)` then `$t`'s parameters; `return_call_ref` composes with
  the existing tail-call result check.
- `ref.func` produces `(ref $exact-type)`; existing baseline code keeps validating because
  subtyping widens it to `funcref`.
- Struct/array rules (packed access signedness, mutability, defaultability for `_default`
  allocators, segment-sourced array initializers reusing the data/elem handle checks) are
  ordinary per-instruction procedures.

### 5. Encodings and text

Type section entries emit `rec` (`0x4e`) only for groups larger than one, `sub`/`sub final`
(`0x50`/`0x4f`) only when a supertype list or non-final declaration requires it, and bare
composite forms otherwise — mirroring the shorthand rules the oracle's printer uses. Reference
types emit shorthand bytes when nullable-abstract, else `0x63`/`0x64` + heap type (s33 for
concrete indices). Text renders `(type $t (struct …))` entries — type identifiers become
meaningful for the first time, so types gain optional names (same uniqueness and quoted-id
rules as every other space, name-section subsection 4).

### 6. Parity strategy unchanged

Oracle features add `gc` (typed function references are part of Wasm 3.0's GC feature in
`wasm-tools`; add `function-references` if the pin distinguishes it). New fixtures: `gc`
(structs, arrays, i31, casts, call_ref, a recursive group) plus exhaustive-fixture rows for
the new uniform instructions. Negative corpus covers each new rejection rule. The
"Shorthand references stay canonical" spec scenario is enforced by the existing byte-stable
fixture check.

## Risks / Trade-offs

- [The `RefType` restructure touches every file that pattern-matches `'FuncRef'`] → the tags
  disappear, so the compiler enumerates every affected site; nothing can be missed silently.
- [Canonicalization subtleties (tie-breaking, external vs. intra-group references)] → keyed by
  the spec's closed form; the "equivalent recursive groups" spec scenario plus oracle
  round-trips of modules with duplicate groups verify observable behavior.
- [Subtyping bugs are the highest-consequence class] → `matches` gets direct unit tests per
  hierarchy edge in addition to corpus agreement, the only internal module in the package to
  get its own test file.
- [Validator's `unknown` interacting with subtyping] → `unknown` continues to satisfy any
  expectation, matching the spec's bottom type; no change to polymorphism handling.

## Open Questions

- Whether `Type.rec`'s `define` callback should also allow declaring supertypes between group
  members (needed only for recursive subtype hierarchies) can be settled during
  implementation; the encoding supports it either way.
