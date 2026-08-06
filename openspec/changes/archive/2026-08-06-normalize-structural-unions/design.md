## Context

See `proposal.md` for motivation and scope. The current compiler has one closed semantic `Type`
actor (`I32`, `Bool`, nominal structs, and fixed arrays), composite declared-type facts, typed HIR
expressions, whole-value affine ownership, deterministic instance discovery, a compiler-owned
target/layout plan, a structured MIR DAG, an immutable logical evaluator, native LLVM emission,
direct WebAssembly emission, and facade-backed `/labs` views.

Unions must extend that spine without introducing a second type identity, a backend-owned tag/layout,
or a cyclic compiler control graph. Wayfinder issue 02 is authoritative: members are nominal,
normalization is structural set algebra, conversion is immediate-contextual and monotonic, `Never`
is the empty union, tags are internal, and narrowing belongs to pattern analysis. Issue 06 requires
layout before MIR and backend-private target realization; its DAG amendment applies to every new
compiler-published relationship.

## Goals / Non-Goals

**Goals:**

- Make every union identity, ordering, conversion, ownership rule, layout, calling shape, and runtime
  value canonical data owned by the compiler.
- Centralize expected-type compatibility so returns, arguments, fields, array elements, and writes
  cannot disagree about injection or widening.
- Preserve one logical active member and complete payload across all representations while keeping
  source spelling and target storage separately inspectable.
- Leave the normalized member/tag facts and provenance required by the following exhaustive-match
  change without preselecting its pattern or control-region model.

**Non-Goals:**

- Pattern syntax, member narrowing, destructuring, guards, exhaustiveness, or match result joins.
- Generic or source-declared transparent aliases, `Option<T>` sugar, failure rows, handlers, or
  typed success/failure returns.
- Niche optimization, public tag numbers, stable C ABI, serialization layout, reflection, arbitrary
  type-level computation, or backend-specific union types in HIR/MIR.
- Stored borrows, partial payload moves, unsafe uninitialized payload construction, or compatibility
  adapters for the unreleased pre-union compiler API.

## Decisions

### 1. Normalize in the `Type` actor and use the normalized value everywhere

Extend the semantic type vocabulary with `Never` and an immutable union containing at least two
canonical nominal members. A single `Type.union` normalization boundary recursively flattens union
inputs, removes `Never`, rejects non-nominal leaves as typed data, deduplicates by canonical nominal
identity, and sorts with one locale-independent code-unit comparator. It returns `Never` for zero
members, the nominal itself for one member, and a union for two or more.

`Type.key`, equality, comparison, encoding, nominal traversal, declaration resolution, diagnostics,
instance keys, layout lookup, and every stable encoder consume this normalized value. Callers cannot
construct a public union object directly. Concrete syntax remains source-ordered in syntax and
declared-type facts; only the semantic outcome normalizes.

Alternative considered: preserve source order in semantic union identity. Rejected because it makes
equivalent types unequal, destabilizes tags/layouts, and forces every later consumer to normalize.

### 2. Parse a low-precedence type union and retain composite recovery facts

Add a lossless `UnionType` concrete node at the lowest precedence of the declared-type grammar, with
parenthesized types available to make nesting explicit. `|` remains a concrete separator, not an
expression operator. `Never` resolves as a built-in type name but has no value syntax.

Declaration analysis adds a source-ordered composite union fact containing every member fact and one
normalized outcome. Resolution attempts all members so one unresolved or invalid member does not hide
independent facts; the aggregate outcome is complete-or-unavailable with one causal diagnostic chain.

Alternative considered: flatten union syntax in the parser. Rejected because tooling must retain
parentheses, duplicates, trivia, damaged members, and exact spans even when semantics normalize them.

### 3. One compatibility relation owns exact use, injection, and widening

Introduce one data-returning compatibility operation over a source semantic type and an immediate
expected type. Its available outcomes are:

- `Exact` when identities already agree;
- `Inject` when a nominal source belongs to the expected union; and
- `Widen` when every normalized source-union member belongs to the expected union.

`Inject` and `Widen` carry a canonical total member map and never mutate the source fact. A target
missing any source member is incompatible; the operation never subtracts, narrows, or consults later
uses. Elaboration invokes this relation only at existing expected-type boundaries: declared returns,
parameters, struct fields, contextual array elements, and assignment destinations. Inference without
an immediate expectation stays precise.

Alternative considered: make union membership a general implicit subtyping relation. Rejected because
it would let later constraints alter earlier inference, obscure ownership transfers, and contradict
the Wayfinder prohibition on general subtyping.

### 4. HIR and MIR use one explicit logical conversion

HIR gains one `UnionConvert` expression whose source remains a precise typed expression and whose
target is a normalized union. The conversion records whether the source is nominal or a narrower
union, its canonical total member map, access mode, expected-context provenance, and source span.

MIR gains the corresponding `ConvertUnion` operation over typed locals. The operation references the
program layout/calling shape and retains member identities plus their resolved source-to-target tag
mapping. Verification recomputes canonical sets from the logical types and rejects duplicate,
unsorted, absent, incomplete, narrowing, local/type, layout, and ownership disagreements. Exact uses
do not emit a conversion.

Both operations live inside existing ordered DAG regions. Union cleanup remains an explicit logical
cleanup case set attached to an owner/drop or replacement plan; it is not expressed as general CFG
edges. Backends may create private branches to remap a dynamic source tag or dispatch cleanup, but
those blocks or branch depths cannot enter MIR.

Alternative considered: lower injection to ordinary aggregate construction and widening to a chain
of conditionals. Rejected because it erases union intent, duplicates tag/layout knowledge, and adds
compiler control regions before matching needs them.

### 5. Ownership treats a union as one complete owner with member-indexed cleanup

Type properties recurse over every normalized member. A union is Copy only if every member is Copy
and cleanup-free; with the currently implemented nominal structs, unions are therefore move-only
unless later Copy conformance makes every member eligible. Owned injection and widening transfer one
complete payload and consume the source under ordinary affine rules. Stored borrows are invalid union
members, and no operation exposes a partial payload move.

Ownership publishes a deterministic `UnionCleanup` case set keyed by canonical member identity. Each
case contains that member's ordinary recursive cleanup; inactive cases do nothing. A union owner has
one obligation, so moves transfer the whole case set and replacement/return/loop cleanup cannot
duplicate it. Lowering embeds the verified case set with `Drop` and owned replacement metadata.

Alternative considered: give every possible member a live cleanup obligation. Rejected because
inactive payload bytes are not initialized values and must never be released.

### 6. Instance discovery follows every member needed by a concrete union

A normalized union appearing in a reachable contract, local, aggregate, array, conversion, or cleanup
plan is one runtime type in the deterministic worklist. Discovery follows all canonical nominal
members for layout and cleanup even when only one injection is observed, because the representation
and calling shape must support every declared member. Equivalent spellings collapse through the type
key before insertion.

Alternative considered: discover only observed injected members. Rejected because it would make
layout depend on control reachability and make separately compiled expectations disagree.

### 7. Bootstrap layout uses a private 32-bit tag and max-member payload storage

The compiler layout planner assigns canonical member ordinals starting at zero after normalization.
For this bootstrap slice the discriminant is one private unsigned 32-bit storage value on every
supported target. Payload alignment is the maximum member alignment, payload size is the maximum
member size, payload offset is the 32-bit tag size rounded up to payload alignment, union alignment
is the maximum of tag and payload alignment, and total size rounds tag-plus-payload storage up to the
union alignment. Padding is deterministic data. Invalid or unavailable member layouts make the union
layout unavailable with the member cause.

The calling-shape tree gains a `SumShape`: one compiler-private 32-bit tag lane, a fixed payload slot
sequence large enough for the largest current member calling shape, and one total mapping from every
member's logical lanes into those slots. Bootstrap executable scalars currently occupy 32-bit lanes,
so smaller members zero-fill unused payload slots deterministically. The representation is private
and intentionally replaceable when the scalar set or ABI work expands; both backends consume the
same plan rather than inferring one.

Alternatives considered: use the smallest possible tag, or let each backend choose a native sum
representation. A fixed 32-bit tag keeps current LLVM/WebAssembly call shapes simple and deterministic;
backend-owned layout would violate the compiler target-layout boundary. Niche optimization remains
deferred.

### 8. Evaluation is the logical oracle; backends realize the same plan

The evaluator adds an immutable `UnionValue` containing the normalized union type, active canonical
nominal member, and complete logical payload. Injection installs a nominal member; widening validates
the source map and changes only the enclosing union type/member tag identity. Calls, returns,
aggregates, arrays, moves, and writes transport the value unchanged. Cleanup dispatches through the
ownership case set. Traces name types and members, not source-observable numeric tags.

Native emission derives private storage/SSA values and any tag-remapping or cleanup blocks from the
layout and verified MIR mappings. Direct WebAssembly emission uses the planned tag/payload lanes and
structured private branches. Both retain compiler provenance and must agree with evaluation through
the differential corpus.

Alternative considered: treat evaluator unions as the physical tag-plus-byte payload. Rejected
because evaluation is the backend-neutral semantics oracle and must not expose target storage.

### 9. The facade and unified workbench expose facts rather than recomputing them

Extend the immutable snapshot with queries for source members, normalized type, compatibility outcome,
member map, ownership, cleanup, discovery, layout, calling shape, HIR/MIR conversion, logical value,
trace, and backend provenance. Add structural-union material to the unified `/labs` registry and
existing coordinated rows; do not create a standalone inspector. Source order and canonical order
are shown separately so normalization stays understandable.

Alternative considered: teach the workbench to parse encoded HIR/MIR or decode backend tags. Rejected
because the analysis facade is the only supported consumer boundary.

## Risks / Trade-offs

- [A fixed 32-bit tag and padded payload can be larger than necessary] → Keep tags private, record the
  full layout in the plan, and defer representation optimization until real workloads justify it.
- [Union calling shapes amplify flattened lane counts for differently sized aggregates] → Gate the
  slice with deterministic lane-count tests and preserve the option for a later indirect ABI chosen
  by the compiler layout phase, not a backend.
- [Expected-type conversion can become scattered across elaboration] → Route every supported expected
  context through one compatibility operation and test each boundary plus exact/no-context cases.
- [Member-specific cleanup may accidentally release inactive storage or duplicate a moved owner] →
  Keep one union obligation with exhaustive canonical cases and test injection, widening, replacement,
  loop transfer, return, trap, and nested aggregate cleanup through evaluation and both backends.
- [Equivalent spelling can leak into stable keys or tags] → Make union construction private, normalize
  before creating semantic facts, and gate every encoder/layout/backend artifact in fresh processes.
- [The following match change may need richer structured selection regions] → Preserve canonical
  members, tags, mappings, access mode, and provenance now while leaving match control design open;
  any future selection relationship must remain DAG-shaped.

## Migration Plan

1. Extend semantic types and declared-type facts, then migrate every exhaustive type consumer and
   stable encoder without compatibility aliases.
2. Add parser/recovery and the centralized compatibility relation; establish semantic and HIR
   injection/widening fixtures before runtime work.
3. Extend ownership, instance discovery, layout, calling shapes, and MIR verification/encoding.
4. Implement evaluator semantics first, then native and WebAssembly realization against the same
   differential fixtures.
5. Add facade queries, unified `/labs` views/presets, corpus coverage, goldens, and release-candidate
   checks; run all repository and strict OpenSpec gates.

Rollback is a normal source revert because the project is unreleased and no persistent data or stable
external union ABI is introduced.

## Open Questions

- The following `match-exhaustively` proposal must choose whether member selection is one dedicated
  structured selection region or a canonical nesting of existing conditionals. Either choice must
  consume the member/tag facts established here and keep compiler-published control acyclic.
