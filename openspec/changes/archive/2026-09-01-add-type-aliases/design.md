## Context

A type spelling becomes a canonical `Type` in one place. `DeclarationCollection` records each
declaration header, `NameResolution.resolve` builds the module scopes, and
`DeclarationCompletion.complete` resolves every declared type through the `ResolutionSeams.type`
callback, which is `NameResolution.resolveType`. That function looks a path up in the module scope,
takes the declaration it finds, and returns `nominalOf(declaration)`. Nothing after that step knows
where a type was spelled.

The failure row `! A | B` is a `RowAlgebra` row, not a `StructuralUnionType`. Row members are
keyed individually so `Without<E, S>` can cancel one. Today the row gets two members only because
the parser hands `DeclarationCollection` two `UnionType` operands. `Type.effect` and
`specializeFailureRow` already flatten a structural union that arrives as one value, which is why
`Effect.catch<A | B>` works. The declared-row builders `semanticFailureRow` and `resolveFailureRow`
do not, because until now a single member could not resolve to a union.

`ModuleSummary.DeclarationKind` and the `ModuleSurface` encoder enumerate declaration kinds and
already encode canonical types through `encodeTypeNode`.

## Goals / Non-Goals

**Goals:**

- Add exactly one point of alias awareness in the semantic pipeline, and keep every phase after
  declaration completion unaware that aliases exist.
- Make the declared failure row independent of how its members were spelled.

**Non-Goals:**

- A `Type` variant for aliases, or alias-name retention for presentation.
- Generic aliases. The parser retains a parameter list only so the rejection is semantic and
  navigable, not a parse error.

## Decisions

### An alias is erased inside `resolveType`; there is no alias `Type`

When the scope lookup returns an alias declaration fact, `resolveType` returns the alias's resolved
target instead of a nominal. Callers receive a `Resolved` fact carrying the canonical target, the
same shape they receive for a struct. Layout, ownership, MIR, the three backends, canonical keys, and
the WeakMap memoization over `Type` values never see a new variant.

The alternative was an `AliasType` variant that carries the name and is unwrapped on demand. It was
rejected because every `Type.fold`, `key`, `encode`, `substitute`, and `isX` predicate would need an
unwrap, the key memoization rules would need an alias-transparent key, and the module surface would
need to decide whether to encode the wrapper. The structural-union spec already commits to
transparency, so the name has nothing to carry.

### Alias targets resolve lazily and memoized, with a cycle set

The resolver seam is built from the _collected_ headers before completion runs, so an alias's
target is still syntax when a use site first asks for it. The type resolver therefore resolves an
alias target on first demand through `resolveDeclaredType`, memoizes the outcome per canonical alias
id, and keeps an in-progress set. Re-entering an alias already in the set is a cycle: the alias
resolves to an unresolved spelling of its own name carrying the cyclic-alias cause, the same fact
shape an unknown type produces, so uses after completion still read the cause off the completed
fact. `DeclarationCompletion` forces every alias once
so the cycle diagnostic is reported at each declaration on the cycle, with the others related, and
so a use site only ever reports the unavailable cause rather than a second cycle diagnostic.

The alternative was a separate alias-completion pass ordered topologically before struct headers.
That adds a phase and an ordering rule for a graph that is almost always one edge deep. The lazy
form reuses the recursive-aggregate precedent already in `DeclarationCompletion` and needs no
ordering.

### Declared failure rows flatten a union member the way `Type.effect` does

`semanticFailureRow` and `resolveFailureRow` split a resolved member whose type `isUnion` into its
members before building the row, exactly as `Type.effect` splits `failureLeaves`. `never` members
drop. A nominal union is a `Nominal`, not a `StructuralUnionType`, so it is untouched by the split
and stays atomic, which is the existing "one atomic structural-union member" rule.

No change is needed for `Effect.catch<Alias>`: the alias erases to a `StructuralUnionType` type
argument, and `specializeFailureRow` already turns that into a `ConcreteRow`.

### Exposure and visibility reuse the struct rules

An alias is collected with the same visibility and canonical-id machinery as a struct. A `pub` alias
whose erased target contains a private nominal reuses the field-exposure check, folding over the
target's nominals against the defining modules' visibility facts, and reports the ordinary exposure
diagnostic at the alias. Cross-module lookup goes through the same `lookup` / `lookupQualified`
gate, so private aliases produce the same `Inaccessible` outcome as private structs.

### The surface encodes the erased target

`DeclarationKind` gains `Alias` with namespace `Type`. The surface entry is the name, visibility,
and `encodeTypeNode(target)`. Because the target is canonical, a dependent is invalidated only when
the erased type changes, and a re-spelling that normalizes to the same union leaves the surface
equal. This falls out of the existing exact-surface-equality rule.

### Parsing follows the constant declaration

`TypeAliasDeclaration` is `[pub] type Name [<params>] = <type>`. It joins the declaration-start and
`pub`-following tables, uses `parseType` for the target so unions, applications, arrays, and callable
types are accepted without a second grammar, and uses the existing contextual type-parameter parse
for the retained-and-rejected parameter list. `type` is added to the complete-identifier keyword
table beside `union`. The formatter renders it as one line through the existing type layout.

### Diagnostics

Two new semantic codes: cyclic alias and parameterized alias. Collision, exposure, visibility,
expected-type, and unknown-type reuse their existing codes. The generated diagnostic index picks the
new codes up through the ordinary regeneration step.

## Risks / Trade-offs

- [Alias name vanishes from hovers and diagnostics] → Accepted per proposal; the structural-union
  spec requires transparency. Presentation retention is a later tooling change and does not touch
  semantics.
- [Reserving `type` breaks user code that used it as an identifier] → Verified no `.silk` source in
  the repository, stdlib, examples, or docs uses `type` outside comments. Silk is unreleased.
- [Lazy resolution re-enters `resolveDeclaredType` from inside the resolver seam] → Memoize per
  canonical id and force every alias in completion so the recursion depth is bounded by the longest
  alias chain and each alias resolves once.
- [A flattened row changes an existing program] → It cannot: no existing program can put a
  structural union into a declared row as one member, because only an alias name can spell one.
- [Tooling surfaces (LSP structure, highlighting, docgen, inspector) lag the compiler] → Listed as
  tasks alongside the compiler work, following the `union` declaration precedent for each.
