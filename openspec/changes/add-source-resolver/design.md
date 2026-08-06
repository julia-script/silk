## Context

See proposal.md — Why. `ModuleClosure.load` currently receives a complete
`ReadonlyMap<identity, bytes>` and decides whether each import is resolved by testing membership in
that map. `Analysis.make` builds a total snapshot around source damage, while `Driver.compile`
duplicates the frontend orchestration and currently reaches codegen without a general diagnostic
gate. The CLI therefore reads one entry file and constructs a singleton map.

The pinned pipeline requires source mistakes to remain diagnostic data with explicit unavailable
facts, operational failures to remain typed compiler failures, and tooling to query unrelated facts
around damage. Module identities are already canonical, case-sensitive, extensionless logical paths
relative to one compilation source root; they are not OS paths.

## Goals / Non-Goals

**Goals:**

- Make source storage an injectable compiler capability without adding a Node dependency to the
  compiler package.
- Preserve one recoverable closure/analysis path for tooling and place strictness at emission and
  driver boundaries.
- Resolve each reachable logical identity once per snapshot with deterministic results and failure
  ordering.
- Keep source diagnostics, operational resolver failures, and compiler defects distinct.
- Let the CLI derive canonical entry identity and imported paths from one explicit source root.

**Non-Goals:**

- Relative-to-importer lookup, package registries, dependency manifests, search paths, index files,
  extension probing, or fallback resolution.
- Incremental invalidation, file watching, long-lived resolver caches, or snapshot isolation across
  concurrent filesystem edits.
- Sandboxing trusted local source roots or forbidding host filesystem symlinks; logical identities
  remain independent of physical aliases.
- Backward compatibility with the preloaded-source-map request API.
- Redesigning existing backend and native-toolchain failure outcomes beyond the driver gate and CLI
  status behavior required here.

## Decisions

### The compiler owns a `SourceResolver` capability, not a filesystem abstraction

Add one public `SourceResolver.ts` actor to the compiler package. Its `Context.Service` identifier
is stable and package-qualified, and its capability has one operation:

```text
resolve(canonicalModule) -> Effect<Option<Uint8Array>, SourceResolverError>
```

The service speaks only canonical module identity and bytes. It does not expose directories,
extensions, URLs, or host paths, so the compiler and browser consumers do not couple to the CLI's
storage policy. `Option.none` means the resolver authoritatively found no source. A typed error means
it could not make that determination.

`SourceResolver` owns an in-memory layer built from an immutable module-to-bytes map. The CLI owns a
separate `FileSourceResolver` actor because filesystem mapping and Effect platform services belong
at the application boundary. A callback field on every compilation request was rejected: it would
hide a capability in ordinary data, make requirements less visible, and weaken layer-based tests.
Putting filesystem calls directly in `ModuleClosure` was rejected because it would make the
compiler Node-specific and prevent browser implementations.

### The root source is explicit; only imports use the resolver

Replace `{ rootModule, sources, target? }` with a request containing one `SourceFile` root and the
optional target. The root's actor already combines canonical identity and exact bytes, avoiding a
parallel entry-source representation. Closure loading seeds its resolution table with the root and
never asks the resolver for it.

This preserves file, stdin, editor-buffer, generated, and browser roots without requiring resolver
composition merely to inject entry bytes. Making the resolver load the root too was rejected because
stdin and unsaved editor buffers would need an overlay resolver before the first import could be
processed.

### Closure loading captures per-module Effect failures as facts

`ModuleClosure.load` becomes a named Effect operation requiring `SourceResolver`. It maintains a
compilation-local table keyed by canonical identity with three cached outcomes: found bytes, absent,
or typed failure. The explicit root is inserted as found before the sorted worklist begins.

For each loaded module, closure loading parses the source and extracts import requests before
materializing their target states. Self-imports and syntactically unavailable paths are settled
without invoking the resolver. Other targets consult the cache, invoke the resolver only when
uncached, then produce `Resolved`, `Unknown`, or the new `Failed` import target. Found targets join
the canonical worklist. Cycles are computed from successfully resolved edges after loading, as
today.

The closure publishes both its exact loaded source catalog and a canonical collection of resolution
failures. Each failure is stored once per target; every failed import fact can refer to the same
typed value. Unknown imports continue to create `MOD0001`. Failed imports create no source
diagnostic, because claiming a missing module after permission or storage failure would be false.

Letting the first resolver error fail `ModuleClosure.load` was rejected because it would discard the
partial graph needed by LSP and browser tooling. Converting resolver errors into diagnostics was
rejected because operational state is not a source mistake and the native compiler assigns it a
different process outcome.

### Analysis is recoverable; emission boundaries are strict

`Analysis.make` becomes a named Effect operation requiring `SourceResolver` and succeeds with the
fullest immutable snapshot it can build. Resolver failures are already captured in the closure, so
they do not inhabit the analysis operation's error channel. The snapshot exposes loaded sources,
failed imports, and resolution failures alongside existing diagnostics and phase facts. The
single-source convenience provides an empty in-memory resolver internally.

Analysis continues to propagate unavailable facts and diagnostic causes exactly as existing phases
do. Codegen is different from inspection: `Analysis.codegen` checks a shared `Diagnostic.hasErrors`
predicate and the snapshot's resolution failures before invoking a backend, returning an explicit
unavailable result when either collection blocks emission.

Mode flags such as `{ resilient: true }` were rejected. Recovery is an invariant of frontend facts,
not a caller preference; strictness belongs at the operation that commits or emits an artifact.

### The driver adds one explicit frontend gate

The driver continues running recoverable closure, declaration, resolution, elaboration, ownership,
and discovery work so it can return useful deterministic diagnostics and reports. Before target
layout, MIR lowering, backend emission, or native tools, it applies one gate:

1. If the closure recorded resolver failures, fail with a typed `SourceResolutionFailed` aggregate
   carrying the ordered failures, loaded source catalog, available diagnostics, and phase report.
2. Otherwise, if `Diagnostic.hasErrors` is true, return a closed `Rejected` outcome carrying the
   loaded sources, diagnostics, and report.
3. Otherwise continue through target selection, MIR, backend, object, shim, and link stages.

Operational failure takes precedence when both kinds are present because an incomplete source graph
means source diagnostics may themselves be incomplete; the aggregate still retains the diagnostics
that were safely produced. All current diagnostics are errors, but a predicate rather than
`diagnostics.length > 0` preserves the gate when warnings are introduced.

Relying on instance discovery or later lowering to become unavailable was rejected. The current
driver does that accidentally for some errors but can reach codegen for other diagnosed programs,
so it is not an enforceable compilation boundary.

### Filesystem resolution is a thin CLI-owned Effect boundary

`FileSourceResolver` is configured once with a normalized source root. For canonical module
`a/b`, it asks Effect's filesystem service for exactly `<root>/a/b.silk`. Only a genuine not-found
filesystem result becomes `Option.none`; every other platform error maps to `SourceResolverError`
with operation, module, message, and wrapped cause. Canonical identity validation occurs before path
construction, and the grammar cannot supply `.` or `..` segments.

The focused resolver layer requires Effect `FileSystem` and `Path`; the CLI executable provides the
Node platform layer once at its outer edge. The CLI source actor accepts an optional source root,
defaults it to the entry's directory, verifies that the entry is below it, strips one `.silk`
suffix, and converts the relative path separators to `/` for the root identity. Imported module
display paths are derived through the same mapping, while diagnostic positions use the closure's
retained bytes.

The resolver cache remains owned by one closure load rather than the service layer. This prevents
stale source across repeated tooling snapshots and guarantees that a transient failure is stable
only for the compilation that observed it.

### Public and test surfaces follow the package's Effect conventions

Export `SourceResolver` from the compiler barrel and package subpath. Public Effect operations use
named `Effect.fn` boundaries with precise success, error, and requirement channels. Implementations
use service constructors and focused layers; tests use `@effect/vitest`, `it.effect`, and explicit
test layers rather than runtime calls or global filesystem mocks.

The browser and docs workbench provide the in-memory resolver. Compiler tests replace preloaded
request maps with memory layers. CLI integration tests use Effect filesystem services for resolver
behavior; native executable assertions may retain their existing process-level harness.

## Risks / Trade-offs

- [Making analysis Effectful touches most compiler consumers] → Migrate the compiler tests, docs
  facade consumers, driver, and CLI in one breaking change; do not retain a synchronous compatibility
  wrapper that hides the resolver requirement.
- [A filesystem can change while a closure is loading] → Cache each requested identity once per
  closure so the compilation remains internally consistent for every observed module; a new snapshot
  performs fresh resolution.
- [Operational failures can make later source diagnostics incomplete] → Preserve the partial facts
  for tooling, but give the operational aggregate precedence at the strict driver boundary.
- [Case-insensitive hosts can open a differently cased physical file] → Preserve the requested
  logical identity exactly and never enumerate or case-fold; add platform-aware tests where the host
  permits them and document that physical case enforcement depends on the filesystem.
- [The driver gate changes previously accidental output behavior] → Add regression tests proving
  every error diagnostic blocks MIR/backend/toolchain work and treat the change as the intended
  prerelease correction.
- [The CLI package may land independently of compiler changes] → Keep the compiler capability and
  memory layer self-contained; land or rebase the filesystem adapter only after the public compiler
  contract is available.

## Migration Plan

1. Add `SourceResolver` data, error, service, memory layer, exports, and focused contract tests.
2. Convert `ModuleClosure` to Effectful cached resolution and publish source/failure catalogs.
3. Migrate `Analysis`, its convenience operations, browser consumers, and compiler tests to resolver
   layers while preserving recovery queries.
4. Add the shared error predicate, unavailable codegen result, driver `Rejected` outcome, and typed
   resolver-failure gate before artifact phases.
5. Add the CLI filesystem resolver, source-root input mapping, multi-source reporting, and `0/1/2`
   exit behavior.
6. Update public exports, README material, changesets, and release-candidate surface checks; run the
   full package-content verification because the public compiler and CLI surfaces change.

Rollback is a git revert of the change before release. There is no persisted data or compatibility
state to migrate.
