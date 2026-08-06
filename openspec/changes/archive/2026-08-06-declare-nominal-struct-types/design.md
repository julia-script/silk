## Context

The compiler currently parses only imports and functions, and `DeclarationIndex.collect` both
assigns function identities and resolves `I32`/`Bool` contracts in one pass. `NameResolution`
builds module scopes afterward and stores whole function facts in bindings. That order cannot
resolve a nominal type declared later or imported from another module. `Layout.plan` currently
discovers only scalar types from reachable HIR instances and constructs scalar entries directly.

Wayfinder fixes the semantic boundaries: structs are nominal; fields and top-level declarations are
private by default; physical and cleanup order follow declaration order; inline recursive storage is
invalid; and the compiler's selected target owns concrete layout before MIR and any backend. The
delta specs define the observable behavior for this change.

## Goals / Non-Goals

**Goals:**

- Introduce a canonical semantic type value that can represent built-in scalars and nominal structs
  without encoding type identity as display text.
- Separate declaration identity collection from declared-type resolution so module scopes exist
  before local or imported type paths are checked.
- Compute one declaration-wide nominal layout catalog early and derive reachable runtime plans from
  those entries without repeating layout choices.
- Preserve immutable facts, exact provenance, deterministic ordering, local recovery, and the
  facade-only tooling boundary.

**Non-Goals:**

- Struct literals, field projections, destructuring, partial moves, aggregate evaluation, MIR
  aggregate operations, or native/WebAssembly aggregate calling conventions.
- `Copy` conformance, aliases, generics, arrays, pointers, references, unions, or automatic layout
  reordering.
- A stable external ABI; field offsets remain private target-specific Silk compiler facts.

## Decisions

### 1. The concrete declaration is a sequential braced field list

The accepted form is `pub struct Name { pub field: Type other: Module.Type }`. A complete field is
self-delimiting because its type is a closed one- or two-segment path; punctuation between fields is
therefore unnecessary. The parser adds `StructKeyword`, `StructDeclaration`, `StructField`, and
`TypePath` vocabulary and synchronizes damage at a later field shape, `}`, or top-level keyword.

This follows the resolved Wayfinder spelling and avoids making line breaks semantically meaningful.
Comma-separated fields were considered, but would add a token rule not present in the accepted
examples and provide no ambiguity benefit for the bootstrap type-path grammar.

### 2. Header processing becomes an explicit staged pipeline

Declaration processing is split into three immutable results:

1. collect every function and struct name, kind, visibility, raw contract/field syntax, and
   canonical identity for the complete closure;
2. build explicit module scopes and import bindings from those identity-bearing headers; and
3. resolve every function contract and struct field type through those scopes, then validate field
   duplication, public exposure, and nominal dependency cycles.

Name-resolution bindings carry canonical declaration references rather than snapshots of partially
resolved declaration objects. Lookups receive the completed index and return its current immutable
fact. This avoids mutation, stale binding objects, a module cycle between actors, and source-order
dependence. The driver and analysis snapshot expose only the completed index as their declaration
authority, while retaining the scope result as a separate queryable fact table.

Keeping the old single-pass collector and teaching it to inspect imports was rejected because it
would make `DeclarationIndex` reconstruct module-scope semantics owned by `NameResolution`.

### 3. Semantic types are values with canonical keys

A `Type` actor owns the closed bootstrap semantic type value: built-in `I32` and `Bool`, plus a
nominal value containing the defining declaration's canonical module and name. It supplies equality,
canonical ordering, and deterministic encoding. Declarations, HIR, layout, and facade facts use the
value; diagnostic messages and encoders use the actor's canonical text rather than object identity.

Nominal types remain legal in declaration contracts and generic semantic facts, but this change
does not make them runtime values. The existing scalar expression surface cannot construct a
nominal value, so discovered executable instances continue to reach only representations the MIR
and backends already support. The following construction change will deliberately extend HIR, MIR,
ownership, evaluation, and backend aggregate behavior.

Using a branded string for nominal identities was considered, but it would make canonical identity
and user-facing spelling indistinguishable and invite ad-hoc parsing throughout later phases.

### 4. Layout has a declaration catalog and a reachable plan

`Layout` remains the sole physical-layout actor and gains an immutable catalog computed from the
selected target and completed declaration index. It evaluates struct dependencies by canonical type
key and records either an available aggregate entry or an unavailable entry with its diagnostic
cause. Available aggregate entries contain declaration-ordered field identities, offsets, nested
types, size, alignment, and aggregate representation. Offset calculation is conventional
`alignUp(cursor, fieldAlignment)` followed by tail alignment; the empty struct is size zero,
alignment one.

`Layout.plan` continues to satisfy the existing reachability contract. It receives the catalog and
selects only entries recursively required by discovered runtime types. Scalars use the same entry
constructors in both catalog calculation and runtime planning. MIR and backends receive the plan,
never the declaration-wide catalog, while the facade exposes both for early inspection.

Eagerly adding every declared struct to the runtime plan was rejected because it would contradict
the existing rule that unused concrete types are omitted. Deferring all struct layout until
construction was rejected because it would lose the early target-aware insight this slice exists to
establish.

### 5. Dependency cycles are canonical semantic failures

Declared nominal field dependencies are ordered by canonical type key and analyzed as strongly
connected components. A component with more than one type, or a self-edge, receives one `SEM0020`
inline-recursive-layout diagnostic attributed to the earliest canonical participating declaration;
every participant and transitive dependent retains an unavailable layout state carrying that cause.
Module import cycles alone remain valid.

The other new semantic codes are `SEM0017` for duplicate fields, `SEM0018` for a non-type
declaration used in type position, and `SEM0019` for a public declaration exposing a private type.
Unknown type and imported visibility failures reuse `SEM0001` and `SEM0015` respectively.

Reporting one error per dependency edge was rejected because it would make equivalent cycles noisy
and traversal-order-sensitive.

### 6. Tooling consumes completed facts only

The analysis snapshot stores collected syntax, the completed declaration index, name-resolution
scopes, the target selection, the nominal layout catalog, and the reachable runtime plan. Facade
queries return these immutable values. The unified `/labs` declaration-index pane adds struct/field
sections, and its target-layout pane renders catalog entries, the distinct reachable runtime plan,
and unavailable dependency states. The docs import-boundary test continues to prevent phase
reconstruction.

## Risks / Trade-offs

- [Staging the declaration index touches many existing call sites] → Keep raw collected headers and
  completed facts as distinct named types, update the driver and analysis construction first, and
  let strict TypeScript identify every stale assumption.
- [Nominal semantic types widen a scalar-only union used throughout the compiler] → Centralize
  equality, ordering, and encoding in `Type`; add exhaustiveness checks where runtime phases remain
  intentionally scalar-only.
- [Recovery without field separators can absorb damaged syntax] → Bound `TypePath` to two segments
  and synchronize at field-shaped tokens, braces, and top-level keywords with focused corpus tests.
- [Layout catalog and runtime plan could drift] → Construct both from the same immutable entry
  builders and assert that every selected plan entry equals its catalog entry exactly.
- [A declaration-only slice cannot exercise aggregate machine ABI] → Treat that as an explicit
  non-goal and require the construction/projection change to add end-to-end runtime parity before
  structs are considered executable data.

## Migration Plan

Implement the staged index and semantic type value as an intentional unreleased API break, migrate
all compiler and docs consumers in the same change, and regenerate deterministic encodings and
goldens. Rollback is the single change commit; no persisted user data or compatibility adapter is
required.
