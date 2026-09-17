## Context

See `proposal.md` for motivation. `ModuleClosure.loadProject` currently pre-populates its resolution map from `SourceFile` roots. Reserved roots are read again to verify caller-provided bytes and origin. `Frontend` separately resolves composition roots before seeding closure loading, and selection performs further discovery passes. `Analysis.rootAnalysis` assumes successful snapshots always contain their root.

`SourceEntry.read` currently derives an identity and reads bytes together. CLI workflows then construct root `SourceFile` values without forwarding file origin. `Analysis.ofSource` supplies an empty resolver because its root bypasses resolution. Project analysis similarly accepts source-valued roots. Existing specs explicitly require these inputs; the three delta specs replace those contracts.

## Goals / Non-Goals

**Goals:** Give the front end ownership of root loading, preserve exact resolver origins, and retain successful-snapshot root invariants with precise typed errors.

**Non-Goals:** New import syntax, relative-to-importer lookup, module aliases, source-path strings in compiler requests, backend changes, or cross-revision resolution caching.

## Decisions

### Requests select canonical identities

Use `CompilationRequest.root: string` and `ProjectRequest.roots: ReadonlyArray<string>`. Keep the name `root`; values use the existing slash-separated canonical identity, for example `app/Main`. Project analysis entry points also accept identities. Validate and canonically deduplicate requested roots before resolving any source; preserve the nonempty-project invariant. Remove byte-conflict handling for repeated roots because the resolver is now their only source authority.

Keeping source-valued project requests would preserve the same split through the shared loader. A union of strings and source files would retain two authorities and is excluded by the repository's green-field policy.

### One resolution path for roots and imports

Seed pending work with identities, not `Found` source outcomes. Root, composition, component, and import resolution share the same namespace dispatch and outcome cache within a discovery load. Reserved identities always use the toolchain resolver, so caller-supplied reserved bytes and the extra verification read disappear. Construct `SourceFile` only after resolution, retaining returned bytes and origin.

Frontend composition helpers pass identities and retain origin metadata needed to diagnose missing configured roots; they no longer independently preload source files. Preserve the existing configuration-error classification for absent optional composition roots and the existing operational failure reporting for them. Explicit application/project roots use the required-root policy below. Overlap between these roles must reuse one resolution outcome.

Module selection must keep a coherent source supply across its discovery passes: reuse the current frontend invocation's recorded outcomes, including absence and failure, and allow later passes to resolve newly admitted imports. Component activation may extend the current snapshot's resolver-backed supply without embedding source files in a new request. Revisions start a fresh supply and compare newly resolved source identity, origin, and bytes to prior syntax for reuse. Do not introduce a process-global cache.

### Required-root failures use the typed channel

As confirmed by the user, absent or operationally failed explicit roots prevent returning a successful analysis snapshot. Introduce an actor-owned `ModuleClosureError` for invalid root requests and unavailable required roots, with structured reasons distinguishing invalid input, missing root, and root resolution failure. A root resolution failure retains the original `SourceResolverError`, including its module, operation, reason, and causal ancestry; missing source has no fabricated JavaScript cause. Handle required roots in canonical order so failure selection is deterministic.

Propagate this precise error through closure loading, frontend analysis, project analysis, realization paths that reload roots, and the driver. Do not turn it into a defect, fabricate an empty source or span, or widen error channels to `unknown`. Successful project revisions contain every requested root; consumers handle the typed failure before publishing a revision. Ordinary imported-source failures remain immutable partial-closure facts and continue analysis as today.

The resolver still returns ordinary absence for missing sources. Requiring a root is the closure consumer's policy, so adding a missing-root error to the storage resolver was rejected. Returning a rootless successful snapshot would force nullable semantics throughout otherwise valid root queries and was rejected by the user.

CLI workflows render unavailable-root errors as request/operational failures (status 2) and commit no output artifact. Missing imported modules remain source rejection (status 1). Preserve the existing diagnostics and error distinction for successfully loaded roots.

### Callers supply sources through resolvers

Split entry path identification from byte loading. `SourceEntry` should describe canonical module, selected source root, and physical entry path without carrying bytes; replace its combined read operation and update every caller. Project loading and CLI build planning perform identity/path selection; `FileSourceResolver` performs the actual root read during frontend work.

`Analysis.ofSource` and corresponding test conveniences keep their useful source-input purpose by constructing an in-memory resolver containing the root and calling the string-based request API. They do not constitute an alternative compiler request path. Keep their typed error signatures honest; do not cast away failures.

LSP revisions install current document overlays for every open module, including roots, preserving original locations. Closed modules continue through the underlying filesystem/toolchain policy. Browser clients and documentation tools likewise register roots in their existing in-memory source supply. Report source diagnostics using the returned closure's source map, rather than rebuilding the root from preloaded entry bytes.

## Risks / Trade-offs

- Broad source-valued caller surface → update compiler, CLI, LSP, documentation tools, scripts, benchmarks, and fixtures together; let type checking expose missed callers and audit literal request construction.
- Repeated frontend discovery could reread roots or mix bytes → share invocation-local outcomes across selection and verify resolver call counts and source origins.
- Missing roots now surface from analysis rather than entry preloading → test CLI status/artifact behavior and LSP revision rejection handling directly.
- Stale editor or syntax-reuse inputs → test changed root overlays, unchanged dependency reuse, and changed-origin reparsing through project analysis.

## Migration Plan

Apply as one coordinated breaking change. Remove source-valued requests and entry-byte preload paths, update all callers, and update the reference documentation. No compatibility adapter, persistent data migration, or staged fallback is needed. If reverted, revert the coordinated change as a unit.
