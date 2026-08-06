## Context

See [proposal.md](./proposal.md) for motivation and the capability deltas under `specs/` for the
normative behavior.

The compiler now carries built-in and canonical nominal types through syntax, declaration indexing,
HIR, affine ownership, instance discovery, target-aware layout, logical aggregate MIR, evaluation,
and two backends. Struct calling shapes flatten canonical field paths into scalar lanes before MIR,
and both emitters consume that plan. The missing pressure is repetition: the type vocabulary is not
recursive, layout paths contain only fields, HIR has no general place chain, and bounds behavior has
no semantic or MIR representation.

Wayfinder fixes arrays as non-allocating inline aggregates whose length is part of the type, whose
ownership derives from the element, and whose safe indexing checks bounds. Mutation, slices, and
borrows remain later capabilities. That means this slice must support useful Copy reads through
non-Copy aggregate elements without inventing a borrowed-value model or allowing partial moves.

## Goals / Non-Goals

**Goals:**

- Add one recursive canonical type form without turning `Array` into a nominal source declaration or
  introducing general generics or constant evaluation.
- Preserve one place path from HIR through bounds checking and backend realization, including mixed
  array-index and struct-field selectors.
- Derive layout, calling shape, ownership, and cleanup once in compiler-owned actors and consume
  those facts read-only downstream.
- Keep array construction complete, immutable, deterministic, and inspectable across every phase.

**Non-Goals:**

- A general lvalue, reference, borrow, pointer, or memory-address abstraction.
- Assignment, `let mut`, loops, slices, ranges, repetition syntax, or partial initialization.
- A stable external aggregate ABI or array-valued host entry point.
- Optimizing constant arrays, eliminating proven checks, vectorizing, or selecting stack versus
  register storage as language semantics.

## Decisions

### 1. Add a recursive structural `FixedArray` type beside built-ins and nominals

The shared type actor gains `FixedArray { element: Type, length: number }`. Equality, ordering, and
encoding recurse through the element and use the canonical numeric length. `Array` is recognized in
type syntax as a compiler-known constructor, not inserted into module scope; its first argument uses
ordinary type resolution and its second is exactly one non-negative decimal integer within the
bootstrap `I32` range. Source spelling and leading zeros remain in syntax, while the type stores the
normalized number.

This reuses the existing recursive nominal-field model without pretending arrays are nominal or
implementing generic application. Representing `Array<T, N>` as a synthesized declaration was
rejected because declaration identity would either make structurally equal array types unequal or
require an interning namespace unrelated to modules. General constant expressions were rejected
because the constant language and dependency model are not available yet.

### 2. Thread an optional expected type into expression elaboration

Array literals need downward context for `[]`, nested empty literals, return positions, and call
arguments. Expression elaboration therefore accepts an optional expected semantic type while still
publishing the expression's independently discovered facts. A non-empty array with no expected type
infers the first available element type, requires exact equality from every remaining element, and
uses the written count. With an expected array, each element receives the expected element type and
the written count is compared with the canonical length. No coercion, union widening, or caller-based
function inference occurs.

Adding a special empty-array node resolved after HIR was rejected because it would hide element and
context failures from semantic tooling. Inventing a bottom or `Never` element type was rejected
because that would pre-empt the structural-union change and make `[]` silently compatible everywhere.

### 3. Model source indexing as a place chain and lower only complete Copy reads

Semantic facts and HIR retain nested `IndexPlace` and `FieldPlace` steps, each with its root/subject,
selector, current aggregate type, resulting element or field type, access request, bounds mode, and
provenance. This makes `pairs[i].left` one place chain whose final `I32` read is Copy even though the
intermediate `Pair` is not.

MIR does not gain independently storable place locals. Lowering collapses a maximal readable chain
to one `ReadPlace` operation over the root logical aggregate local and an ordered selector path.
Dynamic element selectors carry an `I32` local, canonical length, and trap span; field selectors
carry their canonical field identity. The destination is the final Copy value. Construction and
whole-value calls/moves remain ordinary logical values. A direct non-Copy element read or move is
rejected by ownership and lowers only through the existing unavailable-body trap path.

Materializing each intermediate aggregate as a value was rejected because MIR could accidentally
pass, drop, or duplicate something the language only allowed as a non-consuming place. Introducing
general place locals was rejected as unnecessary machinery before borrows and mutation need them.

### 4. Derive ownership recursively from the element type

The ownership category actor becomes recursive: built-in scalars are Copy, nominal structs remain
move-only, and a fixed array is Copy exactly when its element is Copy. This rule depends on type
identity, not length, so `Array<Token, 0>` remains move-only even though it has no runtime elements.
A whole move transfers one owner. Any consuming selector path into a non-Copy element is a partial
move diagnostic; a selector chain ending in a Copy leaf is a read and leaves the root live.

Cleanup plans retain the whole array owner plus a canonical repeated-element plan rather than an
eager duplicated list. Expansion visits ascending indices and delegates recursively to the element
plan. Treating zero-length arrays as universally Copy was rejected because type behavior would then
depend on a value-level cardinality exception and generic code could not derive ownership from `T`.

### 5. Add repeated layout and a shared selector vocabulary

The layout actor gains `Repeated { element, length, stride }`. Stride is the element size rounded up
to its alignment; total size is checked `stride * length`; array alignment is the element alignment;
zero length has size zero while retaining that alignment. Array layout uses the same memoized type
solver as nominal field layout so arrays inside structs and structs inside arrays cannot drift.
Arithmetic overflow produces an unavailable layout with the original type and target provenance.

Canonical layout paths become ordered `Selector` values: `Field(FieldId)` or `Element(index)`. Calling
shape is represented as a deterministic scalar-leaf tree with a checked lane count and a lazy ordered
iterator, rather than eagerly allocating one record for every repeated leaf. Existing struct shapes
become products in the same tree; arrays become repetitions. Encoders and facade queries expose the
same flattened order and canonical selector paths, while backends never choose their own traversal.

Keeping field-only paths was rejected because numeric ordinals would be ambiguous in nested
struct/array shapes. Eagerly expanding huge type-only arrays during analysis was rejected because a
short source declaration could cause disproportionate compiler memory use before emission.

### 6. Bounds knowledge is semantic; dynamic checking is MIR behavior

Semantic analysis classifies each index as `Proven`, `Invalid`, or `Runtime`. A signed literal can be
decided immediately against the canonical length; invalid constants receive one stable semantic
diagnostic and no HIR value. Every other available `I32` index remains `Runtime`. HIR preserves that
mode. MIR `ReadPlace` carries only required dynamic checks; each check occurs immediately before its
selector is applied and traps with the selector's source span.

Both backends may implement `0 <= index && index < length` with any equivalent target instruction
sequence, including one unsigned comparison, but the MIR and evaluator behavior remain the authority.
Folding proven reads or array literals is an optional later optimization and not part of correctness.

### 7. Evaluation remains logical and immutable

The evaluator adds `ArrayValue { type, elements }`, validating completeness once at construction.
`ReadPlace` walks the immutable root using canonical selectors, checking dynamic indices before
access and returning only the final Copy value. Whole-array moves, calls, returns, and cleanup carry
the logical value unchanged. Trace events record construction, compact selector steps, bounds result,
whole transfer, and cleanup without duplicating large element dumps or using ABI lanes.

Reusing physical lane bundles was rejected for the same reason as structs: evaluation is the
semantic oracle and must not inherit target representation or backend traversal.

### 8. Native and WebAssembly emitters consume the same symbolic shape

Each backend traverses the compiler-owned calling-shape tree to declare physical internal parameters,
results, and locals. Construction stores lane bundles in canonical element order. A dynamic selector
chooses the corresponding element lane group after the required bounds check, then continues through
the remaining selectors. Zero-lane arrays preserve calls and cleanup with no payload. Native code may
use aggregate SSA or scalar temporaries; WebAssembly uses scalar locals and multi-value results. The
choice is local realization, never layout or ABI policy.

Adding backend-specific native arrays or WebAssembly linear-memory arrays was rejected because it
would produce two ABIs and introduce allocation/address machinery before the language exposes it.
WebAssembly GC arrays remain outside the authority boundary.

### 9. The facade and unified workbench expose authoritative array facts

The analysis snapshot gains direct queries for array literals, index steps, canonical array types,
repeated layouts, selector paths, and bounds modes. Existing HIR, ownership, MIR, evaluation, and
codegen queries naturally carry the widened values. The unified `/labs` registry adds one coordinated
array-values view and presets; existing HIR, ownership, layout, MIR, evaluation, and backend panes
learn the new variants. No inspector reconstructs lengths, selectors, or lane order.

## Risks / Trade-offs

- [Large lengths can imply enormous physical signatures] → Keep calling shapes symbolic and lazy,
  compute counts with checked arithmetic, plan only reachable types, and let actual emission cost be
  proportional to the artifact requested rather than the type syntax parsed.
- [Expected-type threading can perturb established scalar diagnostics] → Make expectation optional,
  retain independently discovered facts, and pin old scalar corpora byte-for-byte before adding
  contextual array cases.
- [Place-chain collapse could lose per-step provenance] → Preserve every HIR selector span and copy
  those spans onto the corresponding MIR selector/check records and trace events.
- [Copy derivation and cleanup can disagree for nested arrays] → Centralize both over the same
  recursive type classification and test zero-length, nested, and struct-element arrays together.
- [LLVM and Wasm can disagree on dynamic lane selection] → Consume the same selector and shape actors,
  then run evaluator/native/Wasm parity for in-bounds, negative, upper-bound, and zero-length indices.
- [Array syntax conflicts with comparison tokens in type positions] → Parse angle brackets only under
  the recognized `Array` type constructor and retain ordinary expression precedence elsewhere.

## Migration Plan

This is an intentional pre-release public data-model change with no compatibility layer.

1. Add syntax and recursive type identities while later phases retain explicit unavailable states.
2. Thread expected types through elaboration and add array/place HIR plus ownership classification.
3. Extend the shared layout solver, selector vocabulary, reachability, and symbolic calling shapes.
4. Add array construction and checked place reads to MIR and evaluation.
5. Realize the shared plan in native and WebAssembly emitters and expand differential fixtures.
6. Publish facade queries and unified `/labs` presets, then remove remaining closed-union scalar or
   struct-only assumptions rather than preserving adapters.

Rollback is a normal revert: there is no persisted format or external protocol. Deterministic
encoders, strict verifier failures, and the three-engine corpus make a partial rollback visible.
