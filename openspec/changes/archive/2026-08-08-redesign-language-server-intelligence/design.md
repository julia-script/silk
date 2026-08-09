## Context

See `proposal.md` for motivation and the five delta specs for behavioral requirements.

The LSP currently owns two unrelated position queries. `Document.typeAt` scans every expression and
binding fact on each hover request and selects the smallest containing syntax span; it therefore
answers the type of an expression rather than the identity of the token under the cursor.
`Document.definition` instead consumes `Analysis.semanticTargetAt`, whose index contains selected
value, call, field, and import references but omits declaration sites, declared types, type
arguments, qualifiers, and intrinsic effect operations. The server advertises neither completion
nor inlay hints.

The compiler already has the necessary semantic ingredients, but they are distributed across
declaration headers, name resolution, declared-type facts, elaboration facts, lexical bindings, and
ad hoc intrinsic recognition. Tooling is required to consume `Analysis.Snapshot` as its semantic
boundary. The LSP must remain a protocol adapter and must not become a second resolver.

Snapshots are immutable, exact-version acquisition is already handled by `ProjectSession`, and the
project intentionally does not preserve unreleased API compatibility. The design can therefore
replace `SemanticTarget` instead of layering permanent compatibility aliases over the spike.

## Goals / Non-Goals

**Goals:**

- Give every semantically meaningful identifier token one compiler-owned occurrence identity,
  role, recovery state, optional source declaration, and queryable presentation.
- Make declaration and reference hovers share one source-like renderer, including faithful
  `effect fn` signatures and scope-appropriate inferred type names.
- Make hover and definition select the same token occurrence while allowing source-less intrinsics
  to hover without inventing navigation.
- Centralize intrinsic actor and operation identity so analysis, hover, and completion cannot drift.
- Answer completion from recovered syntax plus compiler scope and type facts, including incomplete
  identifiers and member access.
- Keep the compiler data actors pure and immutable, with the callback-driven protocol runtime
  remaining at the server edge.
- Keep occurrence lookup and range hint queries efficient enough for interactive use without adding
  an incremental query engine.

**Non-Goals:**

- Find references, rename, signature help, semantic tokens, document highlights, code actions, or
  code lenses.
- Fuzzy ranking, machine-learned ranking, automatic imports, or editor-specific completion UI.
- Incremental parsing or elaboration, snapshot caching across compiler requests, or cancellation of
  an active compiler invocation.
- Rich documentation extraction, doc-comment Markdown, or rendering full struct bodies in hover.
- Virtual source files for language intrinsics.
- Preserving `SemanticTarget` or the spike's anonymous callable hover output.

## Decisions

### 1. Replace `SemanticTarget` with a token-level `SemanticOccurrence` actor

Introduce one compiler actor whose primary data is conceptually:

```text
SemanticOccurrence
  span        exact identifier token span
  role        declaration | value | type | field | actor | operation | import
  resolution  available identity, or an explicit recovery state
  declaration optional exact source declaration location
```

Available identities cover declarations, type parameters, value parameters, local bindings,
pattern bindings, fields, import namespaces or selections, intrinsic actors, and intrinsic
operations. Role and identity remain separate: the same function identity can occur as a
declaration, a call target, or a first-class value reference.

Declaration locations retain module, complete declaration span, and declaration-name selection
span. Intrinsic identities are available but deliberately carry no declaration location.

The snapshot groups occurrences by module in source order. Each module stores a start-sorted array
and a compact prefix maximum-end index. Lookup binary-searches the last possible start and scans
only the overlapping suffix, selecting the narrowest half-open token span and then stable source
ordinal. This preserves deterministic nested selection without filtering the entire module on every
request.

`Analysis.semanticOccurrenceAt` and range queries become the public facade. The existing
`SemanticTarget` actor and `semanticTargetAt` query are removed after definition parity tests move
to occurrences.

**Alternatives considered:**

- **Enrich `SemanticTarget` but retain its reference-only name and API.** Rejected because
  declaration sites, type names, and source-less intrinsics are occurrences even when they are not
  definition targets; retaining the old abstraction would keep hover and completion awkward.
- **Build a generic syntax-token index in the LSP.** Rejected because token spelling cannot encode
  lexical shadowing, visibility, field selection, type identity, imports, or recovery state.
- **Scan phase fact tables for each request.** Rejected because it repeats the current hover cost and
  leaks compiler phase organization into every tooling query.

### 2. Make semantic facts carry the exact tokens needed by the occurrence collector

The occurrence collector traverses immutable semantic facts, not raw syntax guesses. Declaration
collection covers member names, function parameters, type parameters, fields, local and pattern
bindings, and recursively nested declared types. Expression collection covers value references,
call targets, callable values, struct constructors, field initializers and projections, and every
intrinsic reference.

Where an existing fact only carries a broad syntax node or final token, its data model is deepened
to carry a resolved reference path with exact qualifier and member tokens. Declared-type facts expose
the resolution identity for each path segment needed by tooling. Special Effect expressions retain
an intrinsic actor/operation reference and their resolved type-argument facts rather than only the
whole expression syntax.

This removes helpers that infer semantic tokens with operations such as “last identifier in this
node.” Syntax remains the lossless source carrier, but semantic analysis decides what each token
means before snapshot indexing.

**Alternatives considered:**

- **Have `SemanticOccurrence` re-run name resolution over syntax.** Rejected because that creates a
  second semantic implementation inside the compiler and can disagree under recovery.
- **Index only final member tokens.** Rejected because it cannot distinguish `Effect`, `catch`, and
  `Problem`, which is a central requirement of the change.

### 3. Introduce an `Intrinsic` catalog shared by recognition, presentation, and completion

Create one concept-oriented compiler module for source-less language actors and operations. Stable
intrinsic identities name an actor and operation independently of backend HIR operation names. Each
entry owns:

- the source spelling and semantic kind;
- named generic and value parameters used for presentation and completion detail;
- the source-like result form and relevant unsafe or effect markers;
- a dispatch identity used by elaboration to select its typing rule; and
- deterministic ordering within its actor.

Elaboration continues to own non-trivial type checking and row transformation, but it recognizes
and dispatches special operations through catalog identities rather than independent spelling tests.
Concrete builtin signatures and special operations such as `Effect.catch`, `Effect.map`, and
`SystemAllocator.make` are registered through the same catalog. Tests pair each displayed signature
with successful and rejected analysis examples so presentation cannot silently drift from behavior.

The catalog is pure data with sibling lookup and presentation operations. It is not a service and
does not acquire resources.

**Alternatives considered:**

- **Keep intrinsic hover strings in the LSP.** Rejected because completion and analysis would retain
  separate catalogs and inevitably disagree.
- **Generate virtual Silk declarations.** Rejected because source navigation, diagnostics, and
  visibility would become fiction, while complex operations still require custom typing rules.
- **Encode presentation directly in HIR operation names.** Rejected because HIR is downstream of
  editor recovery and does not represent every actor, qualifier, or unavailable call.

### 4. Render declaration presentations from facts, not canonical value types or source slices

Add a pure `Presentation` actor that returns structured presentation data plus its Silk rendering.
Function presentation reads the declaration fact: visibility where useful, `fn` versus `effect fn`,
name, type parameters, named parameters, and declared result/failure/requirement forms. It therefore
shows `effect fn recover(error: OutOfMemory) -> I32` instead of rendering the function's anonymous
callable value type.

Declared signatures preserve the spellings recorded by their declared-type facts. Presentations of
inferred types, which have no declared spelling, use the requesting module's resolved scope to pick
the shortest accessible unambiguous form: local or selected names first, then a namespace alias,
then a canonical module-qualified fallback. Built-in and type-parameter spellings remain direct.

Presentations are derived lazily from occurrence identities and existing snapshot indexes rather
than storing a duplicate rendered string on every occurrence. Hover, completion detail, and inlay
hints all call the same actor. Protocol Markdown fences are added only by `packages/lsp`.

**Alternatives considered:**

- **Continue using `Type.encode` for every hover.** Rejected because it intentionally renders
  anonymous semantic value types, canonical module names, and lowered Effect results rather than
  source declarations.
- **Slice the declaration header directly from source bytes.** Rejected because damaged syntax,
  comments, formatting, and body-boundary detection make it unstable, and source-less intrinsics
  still need a renderer.
- **Store final Markdown in the compiler.** Rejected because Markdown is a protocol presentation
  concern and would couple the analysis facade to LSP clients.

### 5. Resolve hover occurrence-first and expression-second

The facade answers a structured hover subject at a module and byte offset:

1. Select the smallest semantic occurrence. If it has an available presentation, return that token
   range and presentation.
2. If there is no semantic occurrence, select the smallest available anonymous expression fact and
   return its inferred type presentation.
3. Return no subject for trivia, comments, meaningless punctuation, or unavailable facts.

The expression fallback index is built once per snapshot/module from available expression facts; the
LSP no longer scans facts per hover request. Identifier tokens never fall through to an enclosing
expression merely because their own occurrence is unavailable: an unavailable occurrence produces
no misleading expression hover.

`Document.hover` only converts UTF-16 to bytes, calls the facade, converts the returned span, and
wraps the Silk rendering for the protocol.

**Alternatives considered:**

- **Remove expression hover entirely.** Rejected because literal and anonymous-expression types are
  useful and already supported.
- **Merge expressions into the same occurrence role union.** Rejected because anonymous expression
  spans nest broadly and have no symbol identity, declaration, or completion role.

### 6. Build completion as a separate contextual query over the same semantic model

An occurrence answers what already exists at a token; completion answers what may legally exist at
an incomplete position. A separate `Completion` actor therefore owns a pure query returning:

```text
Completion.Result
  replacementSpan
  contextState
  candidates[]
    identity or syntax identity
    kind
    label and insertion spelling
    structured presentation detail
    stable sort group
```

The query first classifies recovered syntax context as expression, declared type, type argument,
qualified actor/namespace member, or typed-value member. It then reads compiler-owned lexical scope,
module/import resolution, declaration indexes, subject types, visibility, and the intrinsic catalog.
Lexical scopes are represented as compact parent-linked scope entries built during elaboration, so
completion does not duplicate the visible candidate set at every byte position.

The compiler returns the full semantically applicable candidate set and an exact replacement span.
The LSP maps stable kinds and sort groups to `CompletionItem` values; client-side prefix filtering
and fuzzy ranking remain client responsibilities. Stable groups prefer nearer locals and parameters,
then members and declarations, imports and intrinsics, and finally syntax keywords. Identity-based
deduplication happens before deterministic label ordering.

If a partial token has no resolved occurrence, recovered syntax still supplies context and scope.
If the qualifier itself is ambiguous or unavailable, the result preserves that state and does not
guess a member set. Syntax-only keywords may still be returned when their context is independently
available.

**Alternatives considered:**

- **Derive candidates in `Document.completion`.** Rejected because lexical scope, type visibility,
  and member selection are compiler semantics.
- **Use semantic occurrences alone.** Rejected because incomplete source often has no occurrence at
  the cursor and occurrences do not enumerate absent candidates.
- **Implement server-side fuzzy matching.** Rejected because LSP clients already filter and rank,
  while compiler-side fuzzy policy would obscure semantic completeness tests.

### 7. Derive local type hints from binding facts through a `TypeHint` actor

`TypeHint` is a small compiler data actor that projects available local binding facts into
`{ nameSpan, presentation }` values for a module byte range. It uses `Presentation` for inferred
types, skips unavailable inference, clips by the binding-name span, and emits each binding once in
source order. If Silk gains explicit local type annotations, facts carrying an explicit annotation
are suppressed as redundant without changing the protocol contract.

The compiler actor does not import LSP inlay-hint types. `Document.inlayHints` converts spans and
adds the `: ` label at the protocol boundary.

**Alternatives considered:**

- **Call these values code lenses.** Rejected because inferred annotations are positional inlay
  hints; code lenses represent commands or reference counts associated with larger declarations.
- **Scan syntax for `let` declarations in the LSP.** Rejected because only elaboration knows whether
  inference is available and which semantic type was selected.

### 8. Keep LSP modules as thin protocol actors and preserve the existing runtime boundary

`Document` remains the data actor for one synchronized file and owns pure protocol conversions for
hover, definition, completion, symbols, formatting, and inlay hints. `Server` advertises
`completionProvider` and `inlayHintProvider` and routes handlers through the existing exact-version
`ProjectSession.acquire` path. The shared `ManagedRuntime` remains justified solely as the bridge
from vscode-languageserver callbacks into Effect and is disposed on shutdown.

No compiler actor imports vscode-languageserver types. No new services, Layers, or runtime
dependencies are required because occurrence, presentation, completion, and hint calculations are
deterministic pure snapshot queries. Tests continue to use `@effect/vitest`; stdio tests verify
capability negotiation, Unicode conversion, version coherence, and protocol payloads.

**Alternatives considered:**

- **Put one handler module around each one-line protocol callback.** Rejected because protocol
  conversion operations all orbit the synchronized `Document` actor and do not need artificial
  service boundaries.
- **Move editor behavior into the VS Code extension.** Rejected because it would exclude other LSP
  clients and duplicate server semantics.

## Risks / Trade-offs

- **[Occurrence coverage exposes missing semantic provenance]** → Deepen the owning declaration,
  name-resolution, or elaboration fact instead of guessing tokens in the collector; add an explicit
  unavailable occurrence until provenance exists.
- **[Snapshot memory grows with declaration sites and type tokens]** → Store compact spans, roles,
  identities, and indexes rather than repeated strings or declaration objects; measure representative
  project snapshots and keep presentation lazy.
- **[Completion over damaged syntax can become misleading]** → Separate context availability from
  candidate availability, retain explicit ambiguity, and test partial tokens, missing delimiters,
  and unrelated damage independently.
- **[Intrinsic catalog migration can change compiler behavior]** → First key existing recognition
  and typing branches by catalog identity with behavioral parity tests, then consume catalog metadata
  from hover and completion.
- **[Source-visible inferred type names can be ambiguous]** → Use the same resolved module scope and
  visibility rules as analysis; fall back to canonical qualification rather than choosing a short
  ambiguous spelling.
- **[One change spans several editor features]** → Land the compiler occurrence and presentation
  foundation first, then navigation and hover, then hints, then completion; do not advertise a
  protocol capability before its handler and stdio tests are complete.
- **[The bootstrap roadmap excludes a full production LSP]** → Keep this change limited to the four
  specified semantic interactions and avoid incrementality, rename/reference databases, ranking,
  documentation extraction, and editor-specific behavior.

## Migration Plan

1. Introduce intrinsic identities/catalog metadata and source-like presentation actors behind
   existing compiler behavior; prove analysis parity for every migrated intrinsic.
2. Deepen semantic fact token provenance and build `SemanticOccurrence` alongside the old target
   index. Add declaration, type, qualifier, intrinsic, recovery, determinism, and memory-shape tests.
3. Add facade occurrence, presentation, expression fallback, completion, and type-hint queries.
   Switch definition to occurrences and remove `SemanticTarget` once parity and expanded navigation
   tests pass.
4. Switch hover to occurrence-first selection and delete the LSP's per-request expression/binding
   scan.
5. Add range-based type hints and advertise inlay-hint support only after unit and stdio coverage
   passes.
6. Add recovered-context completion, then expression/type candidates, followed by qualifier and
   typed-field candidates. Advertise completion only after every required context is covered.
7. Run repository checks and protocol fixtures, including exact user-reported examples, Unicode,
   cross-file sources, rapid edits, and damaged programs. Run the release candidate check because
   compiler package exports and public analysis APIs change.

There is no compatibility migration for the unreleased `SemanticTarget` API. Rollback is a source
revert: protocol capabilities are introduced only with their complete handlers, and intermediate
compiler actors remain unadvertised until their consumer step lands.
