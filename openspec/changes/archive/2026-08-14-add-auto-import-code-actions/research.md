# Auto-import indexing in rust-analyzer, TypeScript, and gopls

Status: comparative research supporting this OpenSpec change. This is not an implementation
specification.

## Research question

If Silk searches every source-root module for auto-import candidates, should it build an
incremental index, and what should that index own?

## Comparative summary

All three servers avoid making every candidate file a fully analyzed root on every edit. They keep
candidate discovery on a cheaper representation and perform context-sensitive validation and edit
synthesis on demand. Their exact boundaries differ:

| Server        | Project/workspace candidates                                    | Dependency candidates                      | Incremental unit                                                   | Durable auto-import index?                                       |
| ------------- | --------------------------------------------------------------- | ------------------------------------------ | ------------------------------------------------------------------ | ---------------------------------------------------------------- |
| rust-analyzer | Per-module local symbol FSTs                                    | Per-direct-dependency public import maps   | Module locally; crate for dependencies                             | No dedicated durable index found                                 |
| TypeScript    | Existing configured `Program` plus lazy export map              | Lazy, bounded auto-import provider program | Program/provider graph; export map invalidated coarsely            | No; live-project cache (inference)                               |
| gopls         | Build-aware metadata graph plus cached package symbol summaries | Persistent `GOMODCACHE` export index       | Package locally; newly changed module-cache directories externally | Yes, for both generic package summaries and module-cache catalog |

The shared lesson is not “build one global mutable symbol table.” It is “partition immutable or
replaceable export summaries by the compiler's natural invalidation unit, then query their union.”
Persistence is an independent startup optimization, not a prerequisite for incremental updates.

## Go: gopls

### Candidate discovery and scope

Current gopls uses a two-tier `imports.Source`. It searches the current snapshot's metadata graph
first, then falls back to a module-cache index for package names not satisfied by the workspace.
The workspace pass filters packages by the unresolved selector's package name, loads compact
per-package symbol summaries, and accepts a package only if it exports every referenced member.
The module-cache pass performs the equivalent all-names lookup against its index and excludes
`internal`/`vendor` paths that are illegal from the requesting file
([workspace/cache resolution source](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/gopls/internal/cache/source.go#L45-L126),
[workspace metadata search](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/gopls/internal/cache/source.go#L134-L206)).

This is broader than the open document's import closure but narrower and more structured than a
raw source-root file walk at query time: the workspace tier searches packages already represented
in gopls's build-aware metadata graph. The gopls implementation guide describes that graph as the
complete import graph for a workspace and its `Snapshot` as all workspace files after an edit
([implementation guide](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/gopls/doc/design/implementation.md)).
Dependencies outside that graph are discovered from the local `GOMODCACHE`, not from the network.
The user documentation explicitly says import selection depends on the workspace and the contents
of `GOMODCACHE`
([transformation features](https://go.dev/gopls/features/transformation#source.organizeImports)).

### Incrementality and persistence

Workspace symbol extraction is content-addressed per package. Its cache key includes the package
path and every source file identity; a hit decodes the saved summary, while a miss parses the
package, extracts symbols, and writes the encoded result to gopls's durable file cache
([symbol cache](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/gopls/internal/cache/symbols.go#L26-L100)).
This follows gopls's broader separate-compilation design: compact per-package indexes are persisted
and loaded on demand, keeping memory proportional to open packages and their direct imports rather
than to the typed form of the entire repository
([Go team's scalability account](https://go.dev/blog/gopls-scalability)).

The external-package tier is a distinct persistent `GOMODCACHE` index. An initial build scans the
whole cache; later updates retain old entries and scan only module directories newer than the
index's `ValidAt` timestamp. Only new or semantically newer package directories have their exported
symbols extracted
([incremental module-index update](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/internal/modindex/modindex.go#L28-L120)).
The on-disk payload records package name, import path, version, and sorted exported symbols,
including a coarse lexical kind and function-signature data
([module-index format](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/internal/modindex/index.go#L18-L89)).
The Go team reported an order-of-magnitude speedup for organize-imports and unimported completion
when this persistent index was introduced
([gopls v0.19 release notes](https://go.dev/gopls/release/v0.19.0#new-gomodcache-index-for-faster-organize-imports-and-unimported-completions)).

The snapshot and import caches have different invalidation domains. The per-package symbol key
changes with file identity. View-specific import state hashes active `go.mod` files and clears
module resolution state when that hash changes. Background refreshes are delayed and rate-limited;
the persistent module index is refreshed separately
([imports-state implementation](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/gopls/internal/cache/imports.go#L18-L116),
[snapshot-aware refresh](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/gopls/internal/cache/imports.go#L213-L280)).

### Ranking and ambiguity policy

gopls resolves, rather than exposes, most ambiguity: its `Source` contract asks each source to
select the best result for a missing package name
([imports source contract](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/internal/imports/source.go#L25-L59)).
For workspace candidates, it removes unusable/test-only packages and prefers the candidate whose
file path has the longest common prefix with the requesting file. For module-cache candidates, a
package already required by `go.mod` wins; otherwise non-deprecated symbols win, followed by a
version-oriented import-path heuristic. Results are sorted first so ties are deterministic
([workspace ranking](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/gopls/internal/cache/source.go#L209-L251),
[module-cache ranking](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/gopls/internal/cache/source.go#L254-L308)).

This policy is shaped by Go syntax: an unresolved import reference normally has the form
`pkg.ExportedName`, so the package-name qualifier sharply narrows the search. Silk's unresolved
bare names do not provide that discriminator, so Silk should borrow gopls's index separation and
ranking inputs, not its one-result-per-package behavior.

### Import edit synthesis and LSP delivery

Discovery returns semantic `ImportFix` values, not text offsets. The shared imports engine applies
fixes to the parsed import declarations, merges and sorts import groups, formats the result, and
gopls diffs the import-containing prefix back into LSP text edits
([fix application and formatting](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/internal/imports/imports.go#L70-L159),
[LSP edit synthesis](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/gopls/internal/golang/format.go#L110-L205)).
The same computed fixes feed two protocol surfaces: one combined `source.organizeImports` action,
and one diagnostic-associated quick fix per add/delete/rename operation
([code-action wiring](https://github.com/golang/tools/blob/7aff9f346b4713985766ba109b41398c1e1fd5ba/gopls/internal/golang/codeaction.go#L281-L317)).

That separation is directly useful for Silk: candidate selection should produce a structured
import plan, while a single import-edit actor should own merging, ordering, formatting, and
conversion to LSP edits. Completion and code actions can then share plans without duplicating text
mutation logic.

## Rust: rust-analyzer

### Candidate discovery and scope

rust-analyzer's candidate universe is semantic rather than a blind workspace-file catalog. Its
item locator joins symbols from the current crate with importable items from dependencies; the
dependency search follows direct dependency edges. An unrelated workspace sibling is therefore
not a candidate merely because its source lies under the workspace root. A transitive item appears
when a direct dependency publicly re-exports it
([item locator](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/ide-db/src/items_locator.rs#L21-L72),
[external candidates](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/ide-db/src/items_locator.rs#L121-L152),
[dependency import-map search](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/hir-def/src/import_map.rs#L452-L527)).

This differs from the proposed Silk “all source-root modules” policy, but it establishes a useful
constraint: candidate scope should be an explicit language/workspace rule, not an accidental
consequence of which files happen to be open or parsed.

### Indexing and incrementality

Local source and dependencies use different immutable FST-backed indexes. The local
`SymbolIndex::module_symbols` is tracked per semantic module, while `ImportMap::of(crate)` is a
crate-wide map of public exports and re-exports used for dependency lookup. A query unions FSTs
rather than mutating one global index
([local symbol index](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/ide-db/src/symbol_index.rs#L1-L21),
[module-index construction and union](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/ide-db/src/symbol_index.rs#L362-L421),
[public import map](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/hir-def/src/import_map.rs#L22-L129)).

Salsa memoizes these derived queries and invalidates them from semantic inputs. The local rebuild
unit is a module; the dependency export-map unit is a crate. rust-analyzer's HIR design keeps stable
item summaries separate from bodies so many body edits do not perturb global derived facts,
although local symbol collection also accounts for block modules
([Salsa query model](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/docs/book/src/contributing/guide.md#L206-L267),
[HIR architecture](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/docs/book/src/contributing/architecture.md#L138-L177)).

The server is lazy and primes selected caches after quiescence: it warms local module symbol
indexes broadly, but only selected sysroot/language-crate import maps; ordinary dependency import
maps remain demand-driven
([cache priming](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/ide-db/src/prime_caches.rs#L1-L35),
[symbol/import-map priming](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/ide-db/src/prime_caches.rs#L275-L321),
[quiescent trigger](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/rust-analyzer/src/main_loop.rs#L519-L529)).
No serialization path for these FST buffers was found, so the conclusion that these auto-import
indexes rebuild after restart is an inference from their Salsa/in-memory ownership.

### Query timing, filtering, and ranking

The code-action path performs an exact, case-sensitive unresolved-name search on request.
Fly-import completion shares candidate machinery but uses prefix matching for short queries and
fuzzy matching from three characters onward. Applicable imports are capped at one hundred
([auto-import assist](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/ide-assists/src/handlers/auto_import.rs#L94-L137),
[item-locator limit](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/ide-db/src/items_locator.rs#L16-L17),
[asset search modes](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/ide-db/src/imports/import_assets.rs#L355-L545)).
With LSP code-action resolve support, rust-analyzer initially returns labels/data and postpones AST
edit construction; resolution re-runs the assist against the same document version
([code-action request](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/rust-analyzer/src/handlers/request.rs#L1526-L1545),
[code-action resolve](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/rust-analyzer/src/handlers/request.rs#L1585-L1630)).

Candidates must correspond to an unresolved reference, not already be in scope, fit the syntactic
namespace/kind, and admit a semantic path from the current module. Results are deduplicated by
rendered path, then scored by expected-type compatibility, module locality, and crate-boundary
distance. Path finding separately prefers stability, configured prelude policy, fewer segments,
then fewer characters
([candidate validation](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/ide-db/src/imports/import_assets.rs#L405-L545),
[assist ranking](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/ide-assists/src/handlers/auto_import.rs#L256-L343),
[path preference](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/hir-def/src/find_path.rs#L449-L552)).

### Import edit synthesis

rust-analyzer edits imports through the syntax tree. It infers local style, respects configured
crate/module/item/one import granularity, tries to merge into an existing import, and otherwise
inserts into ordered standard/external/crate/self/super groups
([AST import insertion](https://github.com/rust-lang/rust-analyzer/blob/baabc5825f3f6640e99fe32887bbeced640f825e/crates/ide-db/src/imports/insert_use.rs#L215-L327),
[user-facing auto-import behavior](https://rust-analyzer.github.io/book/features.html#auto-import)).

For Silk, rust-analyzer's strongest lesson is structural: store a replaceable per-module public
export index and union those indexes at query time. Keep semantic applicability, path formation,
ranking, and AST edit construction outside the catalog itself.

## TypeScript: tsserver

### Candidate discovery and scope

TypeScript does not maintain a separate persistent workspace-symbol database for ordinary project
files. Auto-import enumerates external modules from the configured project's existing `Program`,
whose source files already include unopened files selected by `tsconfig`. It augments those modules
with ambient modules and, when necessary, modules from a separate `AutoImportProviderProject`
([module enumeration](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/services/exportInfoMap.ts#L455-L505)).

The provider is lazy. The project creates or updates it only when a query asks for it; package and
configuration watchers mark it dirty rather than rebuilding it immediately
([provider lifecycle](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/server/project.ts#L2280-L2316),
[dirty marking](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/server/project.ts#L1366-L1381)).
Its dependency scope is deliberately bounded: it reads visible `package.json` dependencies and peer
dependencies, resolves their entry points or `@types` fallbacks, includes direct project-reference
declaration outputs, and excludes files already in the main program. In the `auto` preference it
aborts provider creation above ten dependencies; the explicit `on` preference removes that cap
([provider root selection](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/server/project.ts#L2565-L2731)).
Package discovery resolves declared main/`exports` entry points and expands export patterns; it is
not a recursive source scan of each installed package
([package entry-point discovery](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/compiler/moduleNameResolver.ts#L2228-L2277)).

### Incrementality and persistence

Broad completion search uses a lazily built in-memory `ExportInfoMap`. Before building or reusing
it, TypeScript updates a dirty provider; a rebuild walks the main/provider modules and records their
importable exports, checking cancellation every hundred modules
([export-map construction](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/services/exportInfoMap.ts#L536-L600)).
The map groups symbol identities and re-exports, and its `search` still iterates entries with a
name/kind predicate. It is cached for one importing file at a time because package visibility,
shadowing, and usable module specifiers depend on the import location
([cacheable export map](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/services/exportInfoMap.ts#L118-L287)).

Invalidation is incremental but coarse at this layer: a normal project update releases
checker-bound transient symbols for later rehydration; file-set changes or loss of program
structure reuse clear the map; and provider program replacement clears the host project's map
([project update invalidation](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/server/project.ts#L1749-L1769),
[provider update invalidation](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/server/project.ts#L2803-L2821)).
There is no durable auto-import index in this implementation: this is an inference from the map
being a live `Project` field that is created lazily and discarded when the project and its provider
close
([map ownership](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/server/project.ts#L2234-L2247),
[project close](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/server/project.ts#L1122-L1165)).

### Completion and quick-fix query strategies

Completion searches the export map with fuzzy text and semantic-kind filters, rejects candidates
that cannot be imported, and defers expensive module-specifier resolution where protocol support
allows. It eagerly resolves only the first hundred uncached specifiers, permits up to one thousand
additional cache-only attempts, and attaches an export-map key so completion-details can resolve
the selected item later
([completion candidate collection](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/services/completions.ts#L4142-L4256),
[specifier-resolution budget](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/services/completions.ts#L625-L691)).

Missing-import quick fixes take a simpler exact-query path: they walk main/provider modules, look
up the exact export name with semantic-meaning filtering, and group original symbols with
re-exports. They do not depend on the completion export-map cache for initial discovery
([import code-fix registration](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/services/codefixes/importFixes.ts#L163-L215),
[exact export lookup](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/services/codefixes/importFixes.ts#L1617-L1665)).
This is a useful warning against designing Silk's first exact-name code action around completion's
more expensive fuzzy-search requirements.

### Ranking and edit synthesis

TypeScript prefers qualifying through an existing namespace, then adding to an existing import,
then adding a new import. If an add-to-existing fix is available, new-import alternatives are
suppressed
([fix-kind ordering and selection](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/services/codefixes/importFixes.ts#L817-L824),
[fix suppression](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/services/codefixes/importFixes.ts#L997-L1036)).
Among module specifiers it prefers package-json-permitted modules, the requested relative/nonrelative
style, the appropriate `node:` form, avoidance of likely barrel cycles, and shorter paths
([specifier ranking](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/services/codefixes/importFixes.ts#L1381-L1467)).

Its `ImportAdder` batches fix-all edits by existing clause and by `(type-only, module specifier)`,
merges named imports, preserves or infers type-only status and quote style, follows detectable local
sort order, and funnels insertion through shared import-edit machinery
([fix aggregation](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/services/codefixes/importFixes.ts#L455-L565),
[batched edit writing](https://github.com/microsoft/TypeScript/blob/b465fdbfe175304d9b977da137b2c178ae1091d3/src/services/codefixes/importFixes.ts#L568-L696)).

For Silk, the important TypeScript pattern is a lightweight whole-configured-project inventory,
with expensive semantic/module-specifier work postponed until an exact query or selected
completion needs it. Durable persistence is not a prerequisite for scaling a project-sized index.

## Lessons and recommendation for Silk

### Decision

Silk should index every source-root module, but it should **not** add every file as a root of
`ProjectAnalysis`. Build a separate, in-memory, incremental export catalog whose unit of replacement
is one module. This matches the common architecture across the three servers while honoring Silk's
broader source-root search policy.

The first version does not need a durable on-disk index. rust-analyzer and TypeScript demonstrate
that a project-sized in-memory export inventory can scale when it is partitioned and derived from
incremental compiler state. gopls's dedicated persistent index addresses the much larger and more
stable cross-project Go module cache; it is evidence for adding persistence later if Silk measures a
restart problem, not evidence that persistence is required now.

### Proposed ownership

```text
source-root inventory ─┐
file watcher events ───┼─> WorkspaceExportIndex snapshot
open-file overlays ────┘       ├─ byModule: ModuleId -> ExportSummary
                               └─ byName: spelling -> Candidate[]
                                             │
unresolved diagnostic + ProjectAnalysis ─────┼─> AutoImport plans
                                             │
                                             └─> ImportEdit -> LSP edits
```

`WorkspaceExportIndex` should own only discovery facts:

- canonical module identity;
- exported spelling;
- declaration namespace/kind (at least type versus value/function);
- visibility/importability flags;
- source provenance and a content identity for replacement.

Each `ExportSummary` should be immutable. Updating one file/module removes that module's old
candidates and inserts the new summary into a new index revision. `didOpen`/`didChange` should
prefer the synchronized overlay; `didClose` returns ownership to disk; create/change/delete/rename
watch events revise only affected modules. The standard library should be a separate immutable
catalog, just as all three studied servers distinguish project source from external or built-in
dependencies.

The compiler should own export extraction and auto-import applicability. The LSP should own source
inventory, watcher/overlay coordination, and protocol conversion. In particular, avoid duplicating
Silk visibility, namespace, or canonical-module rules in the LSP.

### Query and ranking policy

For the initial diagnostic quick fix, use exact `byName` lookup. Do not design the index around
fuzzy completion yet: both rust-analyzer and TypeScript use cheaper exact behavior for code actions
and spend extra search/specifier work only for completion.

For every exact match:

1. Reject the declaring module itself, private declarations, namespace-incompatible declarations,
   unnameable modules, import cycles, and flat-namespace collisions.
2. Produce all valid actions rather than silently choosing one ambiguous bare-name candidate. Silk
   lacks Go's `pkg.Name` qualifier, so gopls's one-best-package policy does not transfer.
3. Prefer, in order: an already imported namespace that can qualify the occurrence; extending an
   existing selective import; adding a new selective import.
4. Within the same edit shape, rank workspace modules before standard-library/dependency modules,
   then by semantic/contextual fit if available, module proximity, and finally canonical module
   identity for deterministic ties.
5. Put the canonical module in every action title so ambiguity is visible.

Expected-type ranking, fuzzy matching, usage-frequency learning, and alias generation can be added
without changing the discovery architecture. They belong in `AutoImport`, not in the catalog.

### Edit synthesis

Candidate selection should return a structured plan such as `QualifyExistingNamespace`,
`ExtendSelectiveImport`, or `AddSelectiveImport`; it should not return raw offsets. One import-edit
actor should parse the current document, preserve comments and local ordering, merge imports where
possible, and emit text edits. This is the clearest point of agreement among rust-analyzer,
TypeScript, and gopls, and it lets future auto-import completion reuse the exact same edit path.

The code-action request should associate each plan with the unresolved diagnostic. If the client
supports code-action resolve, Silk may later return titles and stable plan data first and defer AST
editing, following rust-analyzer. It is not necessary for the first version at the repository's
current scale.

### Startup and scaling behavior

On workspace creation, enumerate `.silk` files once and build summaries with bounded concurrency.
Keep the server responsive while this work runs; an auto-import request can await the current index
barrier when it needs complete results. Thereafter, work per change is proportional to the changed
module plus small reverse-map updates, not to the workspace and not to the reachable semantic
closure.

Track at least initial-index duration, files/modules indexed, summary bytes, per-module revision
duration, exact-lookup latency, candidate counts, and code-action end-to-end latency. Add a
versioned persistent cache only if measurements show that repeated startup extraction is material.
If persistence is added, key summaries by compiler/index format version, canonical module identity,
and content hash; treat corruption or misses as ordinary cache misses.

### What this means for the current design question

The concern about scale is real, but it does not argue for limiting search to the current analysis
closure. It argues for separating two questions:

- `WorkspaceExportIndex`: “What public declarations exist anywhere the workspace policy permits?”
- `ProjectAnalysis`: “What does the currently reachable program mean?”

Index all source-root modules for the first question. Continue to analyze only open/reachable roots
for the second. That gives complete auto-import discovery without turning each keystroke into a
workspace-wide compiler revision.
