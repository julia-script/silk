# Proposal

## Why

Header consumers currently bypass a single observable boundary by reading declaration indices,
resolver closures, and local memo maps directly. Introducing a fresh-run semantic query session for
declaration and name reads makes successful, missing, conflicting, and candidate-set answers
explicit, records their dependencies during execution, and establishes the first Milestone B query
slice without changing Silk language semantics.

## What Changes

- Add an immutable semantic query session bound to one prepared module-closure/selection epoch,
  normalized profile, and header semantic context.
- Add typed `Semantic.resolveName` and `Semantic.typeOf` requests with one authoritative provider per
  migrated read family, within-session memoization, forced-fresh execution, diagnostics, dependency
  observations, and interruption-safe publication.
- Route declaration/header completion, header conformance reads, name resolution, and every existing
  body/evaluation consumer of those reads through the query session.
- Record positive and negative namespace membership, examined candidate sets, import selection,
  headers, aliases, bounds, conformance answers, and configuration reads, including dependency edges
  on memo hits.
- Preserve legal mutually recursive signatures, finite nominal recursion, opaque public-signature
  boundaries, and dependency-path diagnostics for prohibited alias/inline cycles.
- Start a new session when selected declarations, bindings, candidate sets, or profile facts change;
  seal the final session without retaining source-resolver capability.
- Remove parallel resolver/header dispatch and compatibility aliases, and update public exports,
  reports, traces, compiler documentation, and the architecture reference atomically.

## Capabilities

### New Capabilities

- `semantic-query-sessions`: Typed fresh-run semantic query sessions, request identity, observation,
  memoization, cancellation, diagnostics, and sealing contracts.

### Modified Capabilities

- `bootstrap-declaration-index`: Declared public type/signature reads become authoritative
  `Semantic.typeOf` requests while preserving complete closure-wide header facts and recursion rules.
- `bootstrap-name-resolution`: Name reads become authoritative `Semantic.resolveName` requests that
  publish complete lookup outcomes and observe selected and rejected candidates.

## Impact

The change affects compiler header preparation, declaration completion, name resolution, semantic
contexts, body/evaluation consumers, compiler exports and traces, focused structural tests, OpenSpec
contracts, compiler documentation, and the external compiler architecture document. It does not add
disk caching, generalized cross-revision query reuse, body/evaluation providers, source-language
inference, or downstream instance/layout/MIR/backend extraction.
