## Context

See [proposal.md](proposal.md) for motivation and the capability deltas for required behavior.

The language server currently commits one `ProjectAnalysis` revision whose roots are the open
documents and whose remaining modules are their transitive imports. `ProjectAnalysis.revise`
already reuses unchanged syntax, module semantics, and tooling indexes within that union closure.
References and other semantic requests query that coherent revision.

Code actions currently have a narrower path: a `Diagnostic.Edit` contains one replacement in the
diagnostic's source, and `Document.codeActions` maps each replacement directly to an LSP quick fix.
An unresolved name deliberately carries no edit because candidate selection requires facts outside
the diagnostic.

The source root may contain many closed modules outside the accepted semantic closure. The shipped
standard library is separately enumerable through `Stdlib.manifest`. The comparative investigation
in [research.md](research.md) found that rust-analyzer, TypeScript, and gopls all keep import
candidate discovery on cheaper, partitioned summaries rather than making every candidate a full
semantic root.

## Goals / Non-Goals

**Goals:**

- Keep exact-name auto-import lookup fast as the source root grows.
- Preserve the existing open-root semantic analysis and its incremental reuse.
- Put language classification, applicability, and edit construction in compiler-owned actors while
  leaving filesystem discovery and LSP conversion at their existing seams.
- Commit inventory and semantic facts atomically so no action mixes workspace revisions.
- Introduce one source-action interface capable of expressing a grouped, eventually multi-file
  change without implementing unrelated refactors now.
- Make per-module inventory reuse and query behavior observable in tests and phase reports.

**Non-Goals:**

- Semantic elaboration of every source-root or toolchain module.
- Dependency/package-manager discovery beyond project files and the shipped toolchain manifest.
- A persistent on-disk symbol database, FST, fuzzy search, unimported completion, or automatic
  import organization.
- Workspace-wide reference summaries, usage CodeLens, lint warnings, function-contract inference,
  or ordinary/effect function conversion.
- Re-export modeling; Silk imports create local bindings and do not publish imported declarations as
  exports.

## Decisions

### 1. Add a lightweight inventory beside, not inside, `ProjectAnalysis`

The accepted project revision will carry two related products:

```text
Accepted workspace revision
├── ProjectAnalysis
│   └── open roots + transitive import closure
└── WorkspaceInventory
    ├── project source-root module summaries
    └── shipped toolchain module summaries
```

The inventory does not participate in name resolution, elaboration, ownership, or lowering. A
module enters those phases only through the ordinary import closure after an action is applied.

This avoids changing the meaning and cost of `ProjectAnalysis`. The rejected alternative was to
make every `.silk` file a project-analysis root. That would provide candidate facts indirectly but
would also rebuild global declaration/name-resolution state for unrelated modules and would turn a
navigation convenience into a workspace-wide semantic compilation policy.

### 2. Partition inventory state by immutable module summary

The compiler will own a `ModuleSummary` actor with a pure construction interface over one parsed
source. A summary retains only facts needed by workspace discovery:

```text
ModuleSummary
  module identity
  source identity/origin
  ordered imported module identities
  ordered public exports
    spelling
    declaration kind/namespace
    declaration ordinal
    source selection span
```

Summary extraction reads declaration headers and import syntax; it does not elaborate bodies or
resolve declared types. Recovered or duplicate public names that cannot be identified
unambiguously are omitted rather than exposed as speculative candidates.

The compiler-owned `WorkspaceInventory` actor will store summaries by module plus an exact
`spelling -> candidates` lookup. Revising one source replaces only that module summary and the
affected name buckets. Unchanged summaries retain object identity. Candidate code actions use exact
case-sensitive lookup, so an FST or fuzzy index would add complexity without helping this query.

Alternatives considered:

- **One mutable global symbol map:** harder to tie to an accepted revision and harder to test for
  partial/stale updates.
- **Scan summaries at every request:** acceptable for the current repository, but latency grows
  linearly with modules and repeats work for every unresolved name.
- **Persist summaries immediately:** improves restart time only. It introduces cache format,
  invalidation, and corruption policy before startup measurements demonstrate a problem.

### 3. Filesystem discovery remains an LSP-owned Effect boundary

The LSP will add a `WorkspaceCatalog` actor that recursively enumerates canonical `.silk` files
under the discovered source root through Effect filesystem/path services. It will reject paths
outside the canonical root and ignore symlink aliases, following the repository's existing source
selection policy. It converts bytes into compiler `ModuleSummary` values.

Initial project analysis performs one sorted source-root scan. Later revisions update summaries
from:

- synchronized document bytes for open modules;
- the exact closed paths supplied by watched-file notifications; and
- a full rediscovery after a manifest/source-root change.

The `ProjectSession` invalidation input will therefore become structured rather than a bare
priority URI. It will carry document priority plus dirty paths or a rediscovery marker. The
analyzed-document value will retain the same immutable inventory alongside `ProjectAnalysis`, and
the existing latest-wins commit will replace both together.

Toolchain summaries are built from the ordered `Stdlib.manifest` and the active standard-library
resolver. They are reused across workspace revisions for the same process/toolchain inputs and
remain a separate inventory tier so project candidates can rank first.

Keeping enumeration in the compiler was rejected because the compiler's `SourceResolver` resolves
known canonical module identities but intentionally has no filesystem-enumeration policy.

### 4. Compiler-owned source actions return grouped change plans

Introduce a `SourceAction` actor whose interface is protocol-neutral:

```text
Descriptor
  stable action key
  title
  kind
  target span
  optional diagnostic identity

ChangePlan
  source preconditions
  edits grouped by module
```

All edits in one plan are non-overlapping and apply atomically. Auto-import currently edits only the
requesting module, but grouping by module avoids another interface replacement when a future safe
refactor must update callers. Existing `Diagnostic.Edit` values remain compiler facts and are
lifted into the same action delivery path; diagnostic prose is never parsed to invent an edit.

`Analysis` remains the compiler tooling facade. It will expose action discovery/resolution queries
that accept the requesting frontend snapshot and the inventory as explicit immutable inputs. The
LSP converts source spans and module identities to protocol ranges and URIs.

Returning LSP `CodeAction` or `WorkspaceEdit` values from the compiler was rejected because it
would move protocol position encoding, URI policy, and client capabilities across the compiler
seam.

### 5. Auto-import applicability is semantic at the request site

`AutoImport` will locate the unresolved semantic occurrence at the requested range and derive its
namespace from the occurrence role and diagnostic reason. It queries the inventory by exact
spelling, then filters candidates using the requesting snapshot:

- the candidate declaration kind is valid for the unresolved namespace;
- the candidate is public and comes from a module other than the requester;
- the canonical declaration is not already available under that spelling;
- adding the binding does not collide with a declaration, import, prelude binding, or reserved
  intrinsic binding; and
- the selected module identity is importable under project/toolchain policy.

The inventory is intentionally lexical/header-level, while applicability is semantic. This keeps
the broad candidate universe cheap without duplicating name-resolution rules in the LSP.

Candidates rank by: compatible import from the module already exists; project tier before toolchain
tier; canonical module identity; then declaration ordinal. Every surviving module remains a
separate author-visible choice.

### 6. Import planning is a syntax-aware compiler actor

`ImportPlan` will accept the requesting syntax artifact and one selected candidate. It returns
source edits rather than mutating syntax or formatting the entire document.

When a compatible import from the module exists, the planner adds a selected member at token
boundaries, inferring inline versus multiline separator/indentation style from that declaration.
It preserves namespace aliases and all existing member aliases. If no compatible declaration
exists, it inserts a canonical selected-member import at the end of the current import region (or
the beginning of the module when there is no import region), using surrounding newline style.

Damaged imports are never rewritten. The planner may insert a separate valid import when doing so
does not create a duplicate module/binding diagnostic; otherwise it withholds the action. Returned
edits are validated for source ownership, ordering, and overlap before constructing a `ChangePlan`.

Full-document formatting was rejected because `Formatter.format` correctly refuses damaged syntax
and would rewrite unrelated source. Raw string search was rejected because it cannot distinguish
comments/literals from import syntax or preserve recovered trees safely.

### 7. Resolve auto-import actions against their originating revision

The server will advertise code-action resolve support. Initial auto-import responses contain a
descriptor in `CodeAction.data` with document URI/version, target span, selected module, spelling,
and candidate kind; they do not retain server-side action objects. Resolution reacquires the exact
document version from `ProjectSession`, repeats applicability for the selected key, and then asks
`ImportPlan` for the edit.

If that document version is no longer current, resolution returns a disabled/no-edit action rather
than applying old byte offsets. Existing small diagnostic replacements may remain eager, but they
are adapted through the same protocol conversion helpers.

Eager construction of every candidate edit was considered. Deferring it avoids repeated syntax
planning for ambiguous names and establishes the revision-checked path needed by later, more
expensive source actions.

### 8. Preserve extension seams without precomputing future features

The inventory deliberately retains imports as well as exports so a later reverse-import view can
narrow workspace reference work. `ChangePlan` supports multiple edits/modules so contract
completion and proven-safe function conversion do not need a second action interface. Neither
future reference occurrences nor inferred function contracts are added to this inventory: they are
semantic products with different invalidation costs and will remain separate actors.

The follow-up architecture and feature boundaries are recorded in the tracked non-normative
[future-language-intelligence.md](future-language-intelligence.md) companion note.

## Risks / Trade-offs

- **[Header summaries can expose a declaration from a semantically invalid closed module]** → Omit
  damaged/duplicate headers, perform request-site semantic filtering, and let the ordinary next
  project revision diagnose errors inside a newly reachable module. Do not semantically compile all
  candidates to eliminate this recoverable false positive.
- **[Initial recursive discovery can be noticeable in a very large source root]** → Parse files in
  bounded parallelism, publish phase counters/timing, keep summaries compact, and add persistence
  only if startup profiling justifies its lifecycle cost.
- **[Watched-file streams can coalesce or omit intermediate events]** → Treat events as dirty-path
  hints, read current bytes at revision time, and perform full rediscovery for manifest/root changes.
- **[Surgical import edits must handle varied trivia and recovered syntax]** → Concentrate the
  grammar knowledge in `ImportPlan`, test inline/multiline/hybrid/aliased/damaged imports, and
  withhold actions when one coherent edit cannot be proven.
- **[Code-action resolve is not implemented uniformly by old clients]** → Advertise the capability
  explicitly; retain eager diagnostic quick fixes and keep an eager auto-import fallback possible at
  the LSP adapter without changing compiler action plans.
- **[The in-memory exact index consumes memory proportional to workspace headers]** → Store compact
  header facts rather than syntax trees or semantic bodies and remove summaries immediately on file
  deletion.

## Migration Plan

1. Introduce summary, inventory, source-action, and import-planning actors behind compiler tests
   without changing advertised LSP behavior.
2. Extend project-session revisions to build and atomically retain the inventory; add reuse and
   invalidation observations.
3. Route existing diagnostic edits through shared source-action-to-LSP conversion helpers.
4. Advertise resolve support and enable auto-import descriptors/resolution.
5. Run the required repository checks and measure initial scan, one-module revision, and request
   latency on generated large workspaces.

There is no persisted state or external migration. Rollback removes the new capability and
inventory construction; ordinary project analysis and existing diagnostic edits remain the stable
fallback.
