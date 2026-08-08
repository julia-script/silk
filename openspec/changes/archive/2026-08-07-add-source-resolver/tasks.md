## 1. Source Resolver Foundation

- [x] 1.1 Add the compiler `SourceResolver` actor with its package-qualified `Context.Service`,
  precise `SourceResolverError`, canonical module validation boundary, and
  `resolve(module) -> Effect<Option<Uint8Array>, SourceResolverError>` capability.
- [x] 1.2 Add empty and immutable in-memory resolver layers and focused `@effect/vitest` coverage
  for exact bytes, absent modules, case-sensitive identities, and replaceable implementations.
- [x] 1.3 Export the resolver namespace from the compiler barrel and add its explicit package
  subpath without retaining the preloaded-map API for compatibility.

## 2. Resolver-Backed Module Closure

- [x] 2.1 Replace `CompilationRequest`'s complete source map with one explicit root `SourceFile` and
  migrate root identity validation to the new request shape.
- [x] 2.2 Split module parsing/import extraction from target materialization so self-imports and
  damaged paths settle without resolver calls and other imports can become resolved, unknown, or
  operationally failed facts.
- [x] 2.3 Convert `ModuleClosure.load` to a named Effect operation, add the compilation-local cache
  for found, absent, and failed outcomes, and preserve the canonical sorted worklist and cycle
  computation.
- [x] 2.4 Publish the exact loaded source catalog and canonically ordered resolver failures on the
  closure, keeping operational failures outside the diagnostic collection.
- [x] 2.5 Extend closure and name-resolution tests for diamond caching, unreachable modules, self
  imports, parser recovery, absent targets, partial operational failure, cycles, and repeated
  determinism.

## 3. Recoverable Analysis Facade

- [x] 3.1 Make `Analysis.make` and affected facade operations Effectful resolver consumers while
  keeping `Analysis.ofSource` usable through an internally provided empty memory resolver.
- [x] 3.2 Add facade queries for loaded sources and resolver failures, and prove unrelated syntax,
  declarations, scopes, HIR, ownership, target, and layout facts remain queryable around a failed
  import.
- [x] 3.3 Add `Diagnostic.hasErrors` and make facade codegen return an explicit unavailable outcome
  without invoking a backend when diagnostics or resolver failures block emission.
- [x] 3.4 Migrate compiler tests and browser/docs facade consumers from preloaded request maps to
  in-memory resolver layers without introducing runtime calls inside library code or Effect tests.

## 4. Strict Compiler Driver Boundary

- [x] 4.1 Add the closed source-`Rejected` outcome and typed `SourceResolutionFailed` aggregate with
  loaded sources, available diagnostics, ordered failures, and executed-phase reporting.
- [x] 4.2 Add one driver frontend gate after recoverable checking and before target layout, MIR,
  backend, object, shim, link, or destination commit, with operational failure taking precedence
  over source rejection.
- [x] 4.3 Add driver regression tests proving any error diagnostic rejects compilation, resolver
  failure uses the typed channel, clean input still compiles, and backend/toolchain spies are never
  invoked for either blocked case.
- [x] 4.4 Update phase reporting tests for the shortened rejected and operational paths while
  preserving the valid pipeline's canonical phase order and counts.

## 5. Rooted CLI Filesystem Resolution

- [x] 5.1 Add the CLI `FileSourceResolver` actor over Effect `FileSystem` and `Path`, mapping
  `a/b` exactly to `<source-root>/a/b.silk`, translating only genuine not-found results to absence,
  and wrapping all other filesystem failures.
- [x] 5.2 Extend the CLI source-entry actor and command options to normalize the source root,
  default it to the entry directory, reject entries outside it, strip one `.silk` suffix, and derive
  slash-separated root identity from the relative entry path.
- [x] 5.3 Compose the filesystem resolver with Node platform services once at the executable edge
  and pass the explicit root source into the new compiler request.
- [x] 5.4 Replace single-source reporting with catalog-based diagnostic lookup and deterministic
  logical-to-physical display paths for every loaded module.
- [x] 5.5 Add CLI unit and integration tests for root-based nested imports, importer-directory
  independence, explicit source roots, unknown modules, unreadable modules, multi-file diagnostics,
  no partial artifact, and exit statuses `0`, `1`, and `2`.

## 6. Public Surface and Documentation

- [x] 6.1 Update compiler and CLI package exports, dependency metadata, lockfile, facade-boundary
  lists, and packed-consumer validation for the new public actors and breaking request types.
- [x] 6.2 Update compiler and CLI READMEs to document canonical source roots, exact `.silk` mapping,
  browser memory resolution, recovery semantics, and strict compilation outcomes.
- [x] 6.3 Add changesets describing the breaking compiler API and the CLI's multi-file/root-resolution
  behavior.

## 7. Verification

- [x] 7.1 Run focused compiler, docs/browser, and CLI typechecks and tests while migrating each
  consumer, including deterministic repeated resolver fixtures.
- [x] 7.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test` in repository order and
  fix every introduced failure.
- [x] 7.3 Run `pnpm check` before handoff and report any exact pre-existing failure separately.
- [x] 7.4 Run `pnpm release:candidate` because compiler package contents, exports, and packed consumer
  behavior change.
