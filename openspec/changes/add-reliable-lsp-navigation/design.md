## Context

See `proposal.md` for motivation and the three delta specs for behavioral requirements.

The server currently stores one `{ Document, Analysis.Snapshot }` session per URI. Every content
change launches analysis for the changed document and every other open document concurrently.
Per-URI generation counters discard stale publications, but superseded compiler work continues and
semantic requests can read the previous session while their positions refer to newer text.

Each document is analyzed as a compilation root. Open sibling documents in the same discovered
source root form resolver overlays; closed imports are read from disk. This produces correct
multi-module facts but there is no project scheduler, dependency watcher, or shared commit boundary.

The compiler already models resolved declaration, parameter, binding, callable, and member
identities with exact source spans and explicit recovery states. The supported tooling boundary is
the immutable `Analysis.Snapshot`; the LSP must not rebuild resolution from syntax or import names.
The active callable work can alter the set of semantic reference facts, so the editor query must be
derived inside that facade.

## Goals / Non-Goals

**Goals:**

- Bound analysis concurrency per project without introducing compiler-side incrementality.
- Keep synchronized text, line indexes, module identities, and semantic facts revision-coherent.
- Give the analysis facade one deterministic position-to-semantic-target query.
- Implement precise local and cross-file go-to-definition from that query.
- Legalize lexical shadowing across nested body blocks so semantic identity can select the nearest
  local declaration required by navigation.
- Keep scheduling and semantic query actors independently testable without a protocol process.
- Preserve the thin VS Code extension and generic stdio-client support.

**Non-Goals:**

- Incremental lexing, parsing, elaboration, persistent caches, or shared compiler artifacts across
  compilation roots.
- Find references, rename, completion, semantic tokens, signature help, document highlights, code
  actions, or range formatting.
- Guessing a definition from spelling when compiler analysis does not select one.
- Changing module resolution, visibility, or unrelated recovery and diagnostic semantics.
- Turning the private VS Code extension into the owner of language behavior.

## Decisions

### 1. Add one project session and latest-wins worker per discovered workspace

Introduce an internal LSP actor representing a project session. A project session owns:

- its stable workspace identity and discovered source root;
- synchronized documents keyed by URI, each with client version and immutable bytes;
- one monotonically increasing project revision;
- the last atomically committed set of analyzed document sessions;
- at most one running analysis worker and one replaceable pending revision; and
- waiters captured by exact document version for semantic protocol requests.

Standalone filesystem documents use their directory as today. Virtual or otherwise non-canonical
documents use an isolated identity derived from URI rather than the shared module name `untitled`.
Documents from distinct discovered projects never share a session or overlay.

```text
didOpen / didChange / watched file
                │
                ▼
       ProjectSession.invalidate
                │ revision + 1
                ▼
      replace pending project state
                │
        short debounce window
                │
                ▼
        one analysis worker
          │             │
          │ superseded  │ still current
          ▼             ▼
       discard       atomic commit
                         │
                         ├─ publish latest diagnostics
                         └─ resolve exact-version waiters
```

One worker run takes a frozen copy of every synchronized document in the project and analyzes each
open document as a compilation root, sequentially within that project. Results are accumulated
off-state and committed together only if the project revision still matches. A newer invalidation
replaces the pending snapshot; it does not create another concurrent worker. When the active run
finishes, the worker immediately consumes the latest pending state.

Analysis remains parallel across independent projects. This avoids a global bottleneck while
meeting project isolation.

The debounce duration is an injected scheduler setting with a small production default. Scheduler
tests use a controlled clock rather than wall-clock sleeps.

**Alternatives considered:**

- **Keep per-URI generation counters.** Rejected because they suppress stale writes but do not bound
  work, provide project atomicity, or make requests revision-coherent.
- **Cancel the active compiler Effect on every keystroke.** Deferred because external filesystem
  work and analysis phases do not yet define useful interruption boundaries. Latest-wins
  coalescing bounds concurrency even when active work runs to completion.
- **Build incremental compiler analysis now.** Rejected as disproportionate and explicitly outside
  current compiler plans. The session boundary leaves room to replace full `Analysis.make` later.
- **Use one snapshot for an entire project.** Rejected because the current facade accepts a
  compilation root and different open roots can have different reachable closures.

### 2. Capture exact document versions at semantic request boundaries

Each semantic handler captures the server's current `TextDocument.version` with the request
position. It asks the project session for a committed session matching that exact URI and version.

- If that version is already committed, the query proceeds immediately.
- If analysis for that version is active or pending, the request waits.
- If a later edit supersedes the captured version before it commits, the waiter completes with no
  session and the protocol request returns no result.
- Older and newer line indexes are never used to interpret the captured position.

The project session retains only the current committed state rather than an unbounded version
history. This makes superseded requests safely empty instead of potentially wrong.

Diagnostics are published only after an atomic current-revision commit and include the analyzed
document version. Closing a document clears its diagnostics, removes its overlay, invalidates the
project, and resolves its pending waiters with no session.

**Alternatives considered:**

- **Serve the last completed snapshot during refresh.** Rejected because an LSP position has no
  version field and can refer to text that does not match the old line index.
- **Wait for whichever revision commits next.** Rejected because a newer edit can move the token
  under the captured position.
- **Keep snapshots for every document version.** Rejected because edit bursts would retain large
  compiler closures and undermine coalescing.

### 3. Treat filesystem notifications as project invalidations

The server registers for `**/*.silk` and `**/silk.toml` watched-file notifications when the client
supports dynamic watched-file registration. The VS Code extension supplies equivalent
`synchronize.fileEvents` watchers only if its language client does not satisfy that registration.
The protocol handler remains the single invalidation entry point in both cases.

A changed `.silk` path invalidates only sessions whose source root contains that path. Open-buffer
bytes retain precedence when the changed file is synchronized. A manifest change rediscoveres
project membership for open documents below the affected directory, migrates them between project
sessions if necessary, and schedules both old and new sessions.

Clients that cannot report file events retain correct open-buffer behavior but cannot observe
external disk changes until another synchronization event. This limitation is documented rather
than hidden behind a second platform-specific watcher.

**Alternatives considered:**

- **Run server-owned recursive filesystem watchers.** Rejected because LSP clients already provide
  cross-platform watched-file reporting and duplicate watchers complicate resource lifetime and
  remote-editor paths.
- **Refresh every open project for every path.** Rejected because project containment gives a safe
  coarse invalidation boundary without requiring a dependency graph.

### 4. Build an immutable semantic target index inside the analysis facade

Add a semantic-target data actor in the compiler and expose its queries through `Analysis`.
The public answer is a tagged immutable value containing:

- the exact reference-bearing origin span;
- the semantic target kind;
- the existing canonical declaration, parameter, local binding, pattern binding, field, or callable
  identity when available;
- an available declaration-name location `{ module, span }`, or the compiler's explicit missing,
  inaccessible, ambiguous, conflicting, or unavailable resolution; and
- optional type data only where already carried by the selected fact, without expanding hover scope.

Snapshot construction collects target entries from recovered semantic facts after elaboration.
Entries are grouped by source module and sorted by start offset, then by increasing span width and a
stable source-order ordinal. A query first narrows candidates by offset and then selects the
smallest half-open containing reference span. Declaration locations are resolved once from the
snapshot's existing declaration, parameter, binding, pattern, and field indexes.

```text
Analysis.Snapshot
  ├─ existing compiler facts
  └─ semantic targets by module
       ├─ origin span
       ├─ target identity / recovery state
       └─ declaration-name location

semanticTargetAt(snapshot, module, byteOffset)
                │
                └─ immutable deterministic answer
```

The LSP converts UTF-16 positions to byte offsets and consumes only this facade query. It does not
import elaboration or name-resolution operations, scan every fact on each request, or resolve names
from syntax. Data-model types remain importable under the existing facade rule.

The index initially includes only fact forms for which the compiler exposes both a reference origin
and semantic target. Adding a new reference form, including pipe-callable forms, extends the
compiler-side collector and its tests; no protocol resolution branch is added.

**Alternatives considered:**

- **Scan all elaboration facts in `Document.definition`.** Rejected because it repeats the current
  hover scalability issue and turns the LSP into a second semantic query layer.
- **Index syntax tokens and resolve spelling in the LSP.** Rejected because shadowing, imports,
  visibility, overload-like ambiguity, members, and recovery are semantic facts.
- **Expose phase-specific maps directly.** Rejected because tooling is required to consume
  `Analysis` as its supported compiler boundary.

### 5. Return `LocationLink` values from go-to-definition

The server advertises `definitionProvider: true`. `Document.definition`:

1. converts the request position through the analyzed document's `LineIndex`;
2. asks `Analysis` for the semantic target at that byte offset;
3. returns no result for absent or non-available resolutions;
4. maps the available target module to the synchronized URI or rooted project file URI;
5. builds the target module's line index from the exact source bytes in the same snapshot; and
6. returns a `LocationLink` with the origin reference range, complete target declaration range, and
   declaration-name selection range.

Declaration range and selection range are distinct where the facade exposes both; otherwise the
exact declaration-name range is used for both. This remains precise without requiring clients to
open or pre-synchronize the target file.

Definition unit tests exercise each semantic target kind and recovery state. Real stdio tests cover
capability negotiation, Unicode position conversion, open unsaved targets, and closed cross-file
targets.

**Alternatives considered:**

- **Return plain `Location`.** Rejected because `LocationLink` preserves the origin and precise
  declaration selection needed by capable clients without harming protocol compatibility.
- **Return all ambiguous candidates.** Rejected because the compiler intentionally has not selected
  a definition; presenting candidates as definitions would invent semantics.

### 6. Keep actors narrow and resources scoped

`Server` remains the callback-driven application edge and constructs the Node platform layer once.
Project session scheduling, document analysis, semantic target conversion, and URI/range mapping
remain separate actor modules. Named public effectful operations use `Effect.fn`; reusable internal
recipes use `Effect.fnUntraced`.

Dynamic watcher registration is released on server shutdown. The server adds explicit shutdown
handling that closes pending project sessions and disposes its managed runtime after in-flight
callbacks settle. Tests use `@effect/vitest`; protocol tests continue to drive the packed stdio
entry point.

No new runtime dependency is required beyond the existing LSP and Effect packages.

### 7. Legalize nested lexical shadowing in compiler value resolution

Bindings declared in a nested body block may reuse a parameter, pattern, or local spelling from an
enclosing scope. Value lookup selects the nearest completed local binding, then the nearest pattern
binding, then a parameter. Repeating a binding name within the same block remains `SEM0008`, and a
binding initializer continues to resolve before the new binding enters scope.

This compiler semantic change is necessary for the navigation requirement that a shadowed
reference follows the compiler-selected nearer declaration. The LSP remains a pure consumer of
that identity and does not implement a separate shadowing rule.

## Risks / Trade-offs

- **[Full analysis can still be slow]** → Coalescing bounds redundant work now; the project-session
  boundary can adopt incremental analysis later without changing protocol behavior.
- **[Sequential roots increase latency in projects with many open files]** → Changed documents are
  analyzed first, semantic requests can request priority for their exact root, and independent
  projects remain parallel.
- **[A superseded semantic request can return no result]** → This is preferable to navigating to an
  incorrect location; clients naturally retry interactive requests after document changes.
- **[Compiler facts may not yet represent every desired reference form]** → Return no definition for
  unsupported or unavailable facts and expand the facade collector only when canonical identity and
  declaration provenance exist.
- **[Watched-file support varies by client]** → Prefer standard dynamic registration, provide the
  thin VS Code fallback, and document the limitation for clients that report no file events.
- **[Manifest changes can move documents between projects mid-analysis]** → Increment both affected
  project revisions and commit only if each document still belongs to the analyzed session.
- **[The semantic index increases snapshot memory]** → Store compact references to existing
  immutable identities and spans, and measure snapshot growth before considering a lazy cache.
- **[Active callable changes can alter fact shapes during implementation]** → Build the collector
  against the merged `Analysis` contract and keep LSP conversion independent of fact variants.

## Migration Plan

1. Add and test semantic target values and facade queries without changing existing consumers.
2. Add the project-session scheduler behind the current handlers while retaining existing
   diagnostics, hover, symbols, and formatting behavior.
3. Move semantic handlers to exact-version session acquisition and add versioned diagnostics.
4. Add watched-file registration and project migration behavior.
5. Add definition protocol support and client integration tests.
6. Remove the obsolete per-URI generation refresh path after parity and concurrency tests pass.

The changes are additive until the final cleanup. Rollback consists of disabling the definition
capability and restoring the previous refresh orchestration; compiler query additions can remain
unused without affecting compilation.
