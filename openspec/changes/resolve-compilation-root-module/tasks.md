## 1. Resolver-owned closure roots

- [x] 1.1 Change compilation and project request roots to canonical strings, validate/deduplicate identities, and resolve roots through the closure's namespace policy and outcome cache. Remove source seeding and reserved-root verification reads. Extend existing ModuleClosure tests to prove root resolution, exact origins, shared root/import outcomes, canonical ordering, cycles, and reserved-root isolation.
- [x] 1.2 Add the precise actor-owned typed root-request error and required-root validation phase. Cover invalid requests before any resolution, absent roots, original operational errors, deterministic failure selection, and unchanged partial recovery for imported failures in existing closure tests.
- [x] 1.3 Pass composition and component root identities through frontend discovery, preserving their current error classification and sharing outcomes across selection passes. Update ModuleSelection and component activation paths; existing selected-source and composition tests must prove newly admitted imports load and overlapping root identities are not reread within one analysis invocation.

## 2. Analysis and driver integration

- [x] 2.1 Propagate precise root error channels through Frontend, Analysis, ProjectAnalysis, Realization, and Driver, preserving the invariant that successful snapshots contain all explicitly requested roots. Adapt source conveniences to register root bytes in an in-memory resolver; analysis tests must cover normal root queries and typed unavailable-root failure.
- [x] 2.2 Update project analysis revision inputs to root strings and resolver-owned source snapshots. Extend existing project tests to prove reordered/duplicate roots, edited root overlays, unchanged dependency syntax reuse, changed-origin reparsing, and failure without publishing root views when a required root is unavailable.

## 3. Source-supplying callers

- [x] 3.1 Replace SourceEntry's combined identification/read API with byte-free entry metadata and update Project loading and CLI build/check workflows. Remove entry-byte preloading and report from resolved closure sources. Adapt SourceEntry and Workflow tests to prove identity selection does not read source bytes, file origins survive root loading, unavailable roots exit 2 without output, and missing imports still exit 1.
- [x] 3.2 Update LSP workspace/catalog analysis to pass root identities and supply all current open-document bytes and origins through its overlay resolver. Adapt existing workspace tests to prove unsaved roots and imports use the same overlay and failed root resolution cannot publish a successful project revision.
- [x] 3.3 Update remaining compiler consumers, browser tooling, documentation workflows/generation scripts, benchmarks, examples, and fixtures to register root sources in their resolvers. Remove obsolete source-valued request construction and entry-byte assumptions; affected package type checks and existing focused consumer tests establish caller completeness.

## 4. Public documentation

- [x] 4.1 Update compiler API comments and the modules/source-resolution reference with canonical string requests, resolver-supplied roots, and typed root failure behavior. Examples must show root bytes registered in the resolver and contain no claim that the root bypasses resolution.
