## Why

The compiler can follow imports only when callers preload every source into one map, which keeps
the CLI single-file and prevents browser and tooling consumers from resolving modules through
their own storage systems. Source resolution needs a platform-neutral Effect service that retains
the frontend's partial-analysis behavior while allowing compilation to stop cleanly on source or
operational failure.

## What Changes

- Add a compiler-owned `SourceResolver` service that resolves canonical logical module identities
  to exact source bytes through replaceable implementations, including an in-memory layer for
  browsers, tooling, and tests.
- Add a root-based filesystem implementation at the CLI boundary: canonical identity
  `compiler/Syntax` resolves exactly to `<source-root>/compiler/Syntax.silk`, independent of the
  importing module's directory.
- **BREAKING**: Replace compilation requests that preload a complete source map with an explicit
  root module and root bytes plus a `SourceResolver` requirement for imported modules.
- Make module-closure loading effectful, cache each resolution once, and preserve resolver failures
  as typed, queryable facts without misreporting them as missing-module source diagnostics.
- Keep analysis snapshots available around missing or unreadable imports, with unrelated modules
  and facts remaining queryable for browser and LSP-style tooling.
- Add an explicit compiler-driver gate: source errors reject compilation before lowering or
  codegen, while accumulated resolver failures stop compilation as typed operational failures.
- Extend CLI reporting to render diagnostics for every loaded source and distinguish successful,
  source-rejected, and operationally failed invocations.

## Capabilities

### New Capabilities

- `bootstrap-source-resolution`: The platform-neutral source resolver contract, canonical
  root-based lookup, replaceable memory and filesystem implementations, caching, and typed
  operational failures.

### Modified Capabilities

- `bootstrap-module-closure`: Load reachable imports through the resolver and retain partial
  closure facts when individual resolutions fail.
- `bootstrap-analysis-facade`: Expose loaded sources and resolver-failure facts while preserving
  queryability around unavailable modules.
- `bootstrap-compiler-driver`: Reject source errors and operational resolution failures before
  MIR lowering, backend emission, or toolchain invocation.

## Impact

The compiler package gains a public `SourceResolver` actor and changes the request, closure,
analysis, and driver APIs. `ModuleClosure`, `Analysis`, and their tests become Effectful consumers
of the resolver service. The CLI gains a filesystem resolver layer, source-root mapping,
multi-file reporting, and distinct source-rejection versus operational-failure exit behavior.
Browser labs and other tooling consumers provide the in-memory implementation. Package exports,
release-candidate validation, documentation, and changesets must reflect the new public surface.
