## Context

See [proposal.md](proposal.md) for motivation and the delta specs for the complete source contract.
This is a cross-cutting compiler change: today, `Type.Nominal` and declaration facts model structs,
scalar enums are a separate fieldless declaration kind, and `Type.StructuralUnion` models normalized
open alternatives. `Option` and `Result` are currently assembled from wrapper structs and detached
member structs, and several compiler paths manufacture those shapes directly.

The implementation already has most of the required machinery in separate forms:

- struct declaration, field, generic-construction, visibility, ownership, represented-field, and
  layout pipelines;
- structural-union tag, payload, conversion, calling-shape, and active-cleanup pipelines; and
- scalar-enum closed-member lookup and exhaustive-coverage pipelines.

The design composes those mechanisms without treating a variant as a type or flattening it into a
structural union. The repository is green-field: the old Option/Result encodings and abstraction-
shaped intrinsic signatures are migration inputs to delete, not compatibility contracts.

## Goals / Non-Goals

**Goals:**

- Represent a declared union as one canonical nominal type with subordinate variant and field facts.
- Share struct field semantics and structural-union representation machinery without sharing their
  source identities.
- Preserve enough canonical identity through every phase to verify construction, matching, cleanup,
  layout, and backend behavior.
- Keep standard-library `Option` and `Result` ordinary source declarations and remove compiler
  recognition of their names or old representation shapes.
- Keep malformed declarations navigable while ensuring no partial union becomes executable.

**Non-Goals:**

- A public or stable ABI, serialization format, tag value, or representation annotation.
- Raw/C unions, external linkage, tuple variants, variant-local generics, explicit discriminants, or
  automatic nominal-union Copy derivation.
- Variant subtyping, direct whole-union field projection, common-field synthesis, or implicit pattern
  generic inference.
- Compatibility aliases or parallel old/new Option and Result paths.

## Decisions

### 1. The parent is a nominal type; variants are subordinate identities, not types

`Type.Nominal` remains the value-level identity for a complete union application. Its canonical
declaration kind distinguishes a struct, scalar enum, or nominal union. No `VariantType`, detached
nominal, or structural member is created.

Declaration facts add three related identities:

- `UnionFact`, keyed by the ordinary canonical declaration identity;
- `UnionVariantId`, keyed by parent declaration plus variant name, with source ordinal as metadata;
- variant field identity, keyed by variant identity plus field name.

Field algorithms should accept an aggregate-field owner discriminated as either a struct or a union
variant. This preserves one implementation of field uniqueness, visibility, type resolution,
generic substitution, represented storage, and diagnostics while preventing a union field from being
mistaken for a directly projectable parent field.

The variant's source ordinal determines its private tag and cleanup/layout ordering. Its canonical
identity uses parent plus name, so reordering changes the surface and representation plan without
pretending that a same-named variant became a different declaration.

Alternative rejected: synthesize one hidden struct per variant and define the parent as a structural
union. That reproduces the detached identity problem, makes generic parent selection conventional,
and permits normalization to erase variant boundaries.

### 2. Syntax has dedicated union and variant nodes with two-stage constructor resolution

The lexer adds one complete-identifier `union` token. The CST adds a union declaration node, unit and
named-field variant nodes, and a dedicated parent-qualified variant selector used by constructors and
patterns. Named-field declaration bodies must contain at least one field; `{}` is rejected in favor
of the unit spelling.

A constructor is resolved in two stages:

1. Resolve the qualifier through ordinary module scope to a union declaration and bind a contiguous
   explicit parent-argument prefix.
2. Resolve the variant within that declaration, elaborate its supplied fields, and use only those
   field constraints to complete the parent application.

This permits `Option.Some { value: 42 }` and `Result<i32>.Failure { error }`, but not
`Result.Success { value: 42 }` when `E` has no field evidence. Unit constructors must provide the
complete parent application. Patterns also require a complete application, so
`Option<i32>.Some { value }` is valid while `Option.Some { value }` does not infer from the scrutinee.

Parser recovery stays within the current variant. The declaration index retains valid siblings, but
any invalid variant or field makes the parent unavailable for construction, coverage, layout, HIR,
MIR, and execution.

Alternative rejected: infer pattern arguments from the scrutinee or constructor arguments from an
expected result. That would create a second generic-inference policy and contradict existing struct
and call inference boundaries.

### 3. Unions participate in the ordinary declaration and module-surface graph

Header collection indexes unions beside functions, structs, enums, services, interfaces, and other
top-level declarations under the existing cross-kind duplicate policy. It collects parent parameters,
variants, and fields before body analysis, then resolves every field type against the completed
closure-wide scope. The recursive layout dependency graph gains edges from each variant field to its
referenced nominal aggregates.

`ModuleSurface` receives an explicit union record containing declaration kind, visibility, ordered
parameters, ordered variants and kinds, ordered fields, field visibility and types, bounds, and
availability. It excludes source spans and target representation. Changing variant order or any
observable payload contract therefore invalidates dependents; changing a factory body does not.

Analysis and semantic-occurrence projections expose parent, variant, and field facts directly.
Tooling never reconstructs variant ownership from syntax or private numeric tags.

### 4. Variant construction reuses one aggregate-field elaborator

The current struct-literal implementation should be split into a shared aggregate-field elaborator
and thin struct/variant entry points. The shared component owns:

- source-order initializer retention and declaration-order mapping;
- duplicate, missing, unknown, and inaccessible field diagnostics;
- construction authority and non-disclosing private-field fences;
- initializer compatibility and represented-field realization; and
- explicit generic prefixes, field-only suffix inference, conflicts, and completed substitution.

The variant entry point supplies the selected variant's field owner and returns a precise applied
parent union. A field variant requires every declared field exactly once. A unit variant bypasses
field elaboration and is allocation-free.

A parent union has no aggregate field table for expression projection. `value.field` is rejected even
if every variant has an identically named field. Pattern selection is the only safe source operation
that exposes variant fields; its bindings then use ordinary place, borrow, move, write, and cleanup
rules.

Alternative rejected: synthesize common fields across variants. It complicates mutation and active-
payload proof, gives same-spelled fields accidental semantic coupling, and is not struct parity.

### 5. Generic specialization substitutes the parent once and never renormalizes variants

Every variant field refers to parameters owned by the parent union. A complete application is still a
canonical `Type.Nominal` keyed by declaration and ordered arguments. Substitution produces one
ordered specialized variant table for semantic checking, ownership, layout, and lowering.

Unlike `A | B`, that table never deduplicates or flattens. Equal payload layouts, equal field types,
or an uninhabited field do not erase a variant. A `never` field receives the existing zero-sized,
unmaterializable layout fact: the variant keeps its canonical tag and coverage leaf but cannot be
constructed without a valid `never` value. Structural unions inside fields continue to normalize
after substitution.

This model also makes `Option<i32>` and `Option<bool>` distinct roots when both occur in one
structural union.

### 6. Coverage is a set of canonical selection paths

Replace the flat match-member coverage key with a canonical selection path:

```text
SelectionPath = structural root
              | structural root -> applied nominal parent -> variant
```

For a precise nominal-union scrutinee, the parent itself is the root. For a structural union, each
ordinary member is a root; a nominal-union member expands only in the coverage domain to one leaf per
variant. This expansion never changes `Type.StructuralUnion.members`.

Coverage transitions are:

- an unguarded variant pattern removes one leaf;
- an unguarded whole-parent pattern removes its root and all remaining descendant leaves;
- an ordinary exact-member pattern removes its root;
- `_` removes everything; and
- a guarded arm removes nothing.

Diagnostics render fully qualified paths, including applied generic arguments. A guarded affine
variant arm uses the existing provisional arm-binding model: tag tests and guard evaluation do not
commit field moves or cleanup until the guard succeeds, so a false guard leaves the complete value
available to later arms.

HIR retains both the outer structural-member selection and inner variant selection. MIR lowers a
direct variant arm to an outer structural tag test when needed, followed by the nominal tag test.
The verifier rejects a payload projection not dominated by the matching variant decision.

Alternative rejected: require a nested match after selecting the whole parent. It preserves type
identity but adds ceremony and discards the user-approved direct hierarchical matching behavior.

### 7. Ownership is parent-nominal with active-variant cleanup

Interface, operator, Copy, and Drop lookup remains keyed by the applied parent nominal type. A union
is affine unless a valid explicit `impl Copy` applies. Copy validation traverses every variant field
under the declared bounds; it cannot infer conformance from currently reachable fields.

Cleanup planning introduces a nominal-union branch containing one private tag decision and one field
cleanup sequence per variant. It reuses the existing nominal Drop ordering, but traverses only the
active variant's initialized fields. A moved pattern transfers selected fields and retains omitted
fields in that variant's branch-local cleanup. Whole-value movement, structural-union injection,
typed failure transfer, and scope exit preserve one active obligation. Fatal traps keep the existing
no-unwind rule.

Represented callable and Effect fields use their existing concrete realization and storage fences.
Only the active variant owns their captures or environment; an unrealizable field makes the complete
parent application unavailable before MIR.

### 8. Layout has a distinct nominal-union plan built from aggregate payload plans

Internal names must distinguish nominal and structural unions. Use tags such as
`NominalUnionRepresentation`, `NominalUnionCallingShape`, and `NominalUnionCleanup` rather than
overloading existing `Union` records whose meaning is structural.

For each concrete parent application, target planning builds:

```text
NominalUnionLayout
  parent
  private tag representation
  payload offset, size, alignment
  total size, alignment, padding
  variants[]
    variant identity, source ordinal, private tag
    aggregate payload layout
    logical-field-to-fixed-slot calling mapping
```

Each field variant's payload uses the existing declaration-ordered struct field offset and padding
algorithm, including concrete callable and Effect realizations. The enclosing payload uses the
maximum variant size and alignment. The private tag uses the existing deterministic private-tag
width policy and source-order ordinal; no source or external ABI observes it.

Complete non-generic unions enter the nominal layout catalog before runtime reachability, including
unavailable and unused private declarations. Open generics get no speculative layout. Reachable
concrete generic applications receive canonical specialized entries. Mixed struct/union recursion is
checked in one inline dependency graph, with explicit existing indirection as the only cycle break.

The calling shape is a tag lane plus fixed payload slots and a complete mapping from every variant's
logical aggregate lanes. MIR, evaluation, Wasm, and LLVM consume this one plan; backends do not infer
offsets, tag order, or call ABI independently.

### 9. HIR and MIR use explicit nominal-union operations

HIR adds explicit nodes for construction and variant selection. A construction records the applied
parent, variant, specialized declaration-ordered field initializers, source mapping, access facts, and
precise result. A pattern records its complete selection path, bindings, omissions, access mode,
guard, and active cleanup branch.

MIR adds monomorphic operations for:

- constructing a nominal union from one verified variant payload;
- testing/selecting a variant through a verified layout;
- projecting a selected payload field with its variant identity; and
- dispatching active-variant copy/drop behavior.

The MIR verifier checks parent application, variant ownership, field completeness and types,
selection dominance, layout/calling-shape identity, hierarchical coverage completeness, and cleanup
branch correctness. Canonical encoding orders applications, variants, fields, paths, and cleanup by
their canonical keys rather than traversal order.

The evaluator stores semantic parent, variant, and payload identities directly. Wasm and LLVM lower
the verified private tag and payload plan. This keeps evaluator traces readable without making the
numeric tag source-observable.

### 10. Existing recoverable intrinsics become carrier-neutral

The old compiler paths manufacture `Option` and `Result` by spelling and representation shape. They
must be removed with the wrapper encodings. The intrinsic inventory is reshaped without adding a new
source-callable operation.

Checked scalar primitives become generic over an ordinary result carrier `B` and accept two exact
`once fn` constructors conceptually equivalent to:

```text
checked<T, B>(operands, present: once fn(T) -> B, absent: once fn() -> B) -> B
```

The selected callback is invoked exactly once; the unused callable environment is cleaned normally.
Integer wrappers pass `some<T>` and `none<T>`, so public operations still return `Option<T>`, while an
equivalent user wrapper may choose another carrier without compiler registration.

Completed Effect reification similarly becomes a carrier-neutral fold:

```text
effectOutcome<A, E, R, B>(
  protected: once Effect<A ! E ? R>,
  success: once fn(A) -> B,
  failure: once fn(E) -> B,
) -> B ? R
```

`Effect.result` passes ordinary `succeed<A, E>` and `failResult<A, E>` functions. The primitive
preserves lazy timing, access, ownership, cleanup, requirements, and future suspension, but contains
no Result identity.

Unsafe host primitives that only report counts use a `bool` result plus explicit initialized
count/reason/code outputs. Handle-producing file and directory opens cannot use an optional handle
output because `OsHandle` is affine and a failed call cannot initialize it. Those opens instead take
an exact `once fn(OsHandle) -> B` success carrier and `once fn() -> B` failure carrier while retaining
initialized scalar reason/code outputs. Success transfers the new handle and its close obligation only
to the selected callback; failure creates no handle. Ordinary source then constructs Option or domain
data. This removes Option from low-level OS, standard-input, child-process, and process-input contracts
without adding partial initialization semantics.

Alternative rejected: resolve canonical `silk.option.Option` or `silk.result.Result` inside compiler
phases. Even if the lookup used a declaration index, the compiler would still grant library identity
by module/name spelling and would retain the abstraction-shaped privilege this migration is meant to
remove.

### 11. Option and Result migrate atomically after compiler support is complete

Canonical source becomes conceptually:

```silk
pub union Option<T> {
  None,
  Some { pub value: T },
}

pub union Result<A, E> {
  Success { pub value: A },
  Failure { pub error: E },
}
```

The public constructor helpers remain ordinary ergonomic functions because unit variants and
parent-only parameters often require explicit arguments. They return direct variants and add no
representation layer. All combinators, integer wrappers, Effect wrappers, filesystem/process code,
fixtures, examples, doctests, and reference pages migrate in the same change. Detached member
imports, wrapper-field matches, old Type helpers, and backend special cases are deleted.

No intermediate source revision with both representations is a supported endpoint. The implementation
may be developed in compiler-first commits, but the completed change admits only the direct nominal
definitions.

### 12. Diagnostics and verification follow existing evidence tiers

Add stable structured diagnostics for empty unions/variants, duplicate variants, invalid variant
qualifiers, incomplete parent applications, foreign variants, private construction fences, parent
field projection, incomplete hierarchical paths, and invalid/unavailable parent applications.
Reuse existing struct field, generic inference, ownership, visibility, represented-storage, and inline
recursion diagnostics when their payload already expresses the exact cause.

Tests prove each claim at the cheapest layer:

- lexer/parser/formatter and recovery tests for syntax;
- declaration, module-surface, semantic-fact, inference, visibility, projection, and coverage tests
  through `Analysis`;
- evaluator tests for language semantics and cleanup;
- Wasm tests only for representation/codegen claims;
- native-only cases through the shared differential acceptance corpus; and
- deterministic MIR/layout encodings through committed in-process goldens, with fresh-process
  coverage left to the repository's global determinism canaries.

## Risks / Trade-offs

- **[Risk] `union` collides conceptually and internally with structural unions.** → Keep
  `NominalUnion` in compiler identifiers and documentation wherever ambiguity exists; reserve plain
  `Union` internally for the established structural representation.
- **[Risk] Refactoring the large struct-literal analyzer creates behavioral drift.** → Extract the
  aggregate-field engine under existing struct tests before adding the variant entry point; require
  byte-identical existing struct facts and diagnostics.
- **[Risk] Hierarchical matching moves affine fields before a guard commits.** → Represent selection
  paths independently of bindings and retain the current provisional guard transaction; add a false-
  guard affine-payload cleanup test.
- **[Risk] Nested structural and nominal tags are flattened accidentally during optimization.** →
  Keep both identities in HIR/MIR and make verifier dominance/layout checks reject a flattened path.
- **[Risk] Uninhabited payloads produce inconsistent coverage and layout.** → Preserve the declared
  leaf and tag, use the existing zero-sized unmaterializable `never` entry, and require exhaustive
  coverage without permitting construction.
- **[Risk] Carrier-neutral intrinsics increase callable and cleanup pressure.** → Admit only exact
  static `once fn` carriers, reuse represented-callable realization, verify unused-carrier cleanup,
  and keep the operation inventory count unchanged.
- **[Risk] The Option/Result rewrite touches a large source corpus.** → Land it only after compiler
  parity is available, migrate with repository-wide searches and generated-manifest refreshes, and
  reject all stale detached declarations/imports in an explicit removal test.

## Migration Plan

1. Add syntax, declaration identities, field-owner generalization, module surfaces, semantic facts,
   diagnostics, and formatter support while keeping invalid parents non-executable.
2. Add constructor elaboration, generic completion, projection rejection, variant patterns, canonical
   selection paths, and hierarchical exhaustiveness.
3. Add ownership, represented-field realization, layout catalog/calling shapes, HIR, MIR,
   verification, evaluation, Wasm, and LLVM support.
4. Reshape checked-scalar and Effect-outcome contracts around exact carriers, handle-producing opens
   around affine success/failure carriers, and count-producing host operations around primitive status
   plus initialized scalar outputs; update canonical source wrappers and remove direct Option/Result
   construction from compiler code.
5. Replace `option.silk` and `result.silk`, migrate all source callers/tests/docs/fixtures/manifests,
   and delete wrapper structs, detached members, old type helpers, lowering branches, and backend
   assumptions.
6. Run focused semantic and engine tests, then the repository-required `pnpm typecheck`, Biome check,
   test suite, `pnpm check`, and release-candidate verification when package contents change.

There is no data or external ABI migration. Rollback is a whole-change source revert; no compatibility
format is retained or emitted.
