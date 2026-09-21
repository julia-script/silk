# Design

## Context

See proposal.md for motivation and the semantic-body-revision-validation spec for behavior. JUL-217 introduced a shared semantic query runtime with typed descriptors, current input readers, previous/current snapshots, ordered dependency validation, result-fingerprint cutoffs, abort-safe reservations, and forced-fresh propagation. Body checking still delegates to BodyQuery, while evaluation, residualization, and ownership retain separate stores or local validity rules. Complete checked results also contain syntax-bound presentation objects that cannot safely cross revisions unchanged.

## Goals / Non-Goals

**Goals:**

- Make the shared semantic query runtime the only reuse-admission authority for bodies, evaluation, residualization, and ownership.
- Keep stable semantic answers independent from revision-local syntax while preserving editor identity and current diagnostics at presentation time.
- Preserve complete checked-unit contents, including hidden bodies, with explicit dependencies and deterministic fingerprints.
- Account for reused evaluation work with the same budgets and policy used for executed work.

**Non-Goals:**

- Persist query results across processes; JUL-219 and JUL-221 add storage and persisted checked-unit envelopes.
- Generalize persistence to every semantic query.
- Change Silk language semantics or diagnostic wording.

## Decisions

### One shared query runtime admits every semantic result

Extend the closed semantic descriptor and provider dispatch with checked-unit, evaluation, residual-program, and ownership families. Each provider executes through the existing query runtime and records nested reads there. Delete BodyQuery's validity scanner and the local reuse decisions in evaluation, residualization, and ownership rather than wrapping them behind a second admission layer.

The alternative—keeping each existing cache and consulting it from a shared root query—would leave multiple authorities able to return stale results and would make dependency traces incomplete.

### Stable answer and current presentation are separate values

Each family stores a position-independent semantic answer addressed by canonical module/body/application identity. Revision-local syntax, source spans, and diagnostic objects are reconstructed from the current module and correspondence at the public boundary. Checked-unit construction captures all explicit and generated bodies in one immutable answer so downstream consumers never observe a partially reused unit.

The alternative—retaining prior CheckedUnit objects directly—would leak predecessor declarations and positions into the current project.

### Query observation replaces dependency scanning

Providers use ordinary semantic query and typed input reads while executing. The shared runtime records those observations in order and validates them recursively. No post-hoc traversal guesses which facts an answer depended on. Static selection naturally records only the chosen branch; parser recovery records the explicit unavailable input it observed.

The alternative—maintaining BodyQuery's structural dependency scanner—duplicates compiler semantics and becomes stale whenever checked-unit shapes evolve.

### Evaluation and residual construction have separate identities

Evaluation answers are keyed by canonical static application identity, normalized arguments, profile, and evaluation policy. Residual construction is a separate query keyed by its runtime specialization identity and records the evaluated facts it consumes. Both answers carry deterministic work cost. On an admitted hit, the caller charges that cost once before receiving the answer; validation itself never decrements a budget.

The alternative—treating a residual program as part of every evaluated value—invalidates static results for runtime-only changes and obscures which phase exhausted a limit.

### Ownership consumes the complete checked-unit query

Ownership is keyed by canonical checked-unit identity and records only the body facts and configuration it reads. Its stable result contains semantic ownership facts; current diagnostic presentation uses the same revision-local presenter as checked units. Ownership no longer keeps an independent reusable-result map.

The alternative—caching ownership by object identity—cannot survive reconstruction or explain reuse across revisions.

## Risks / Trade-offs

- [Risk] Splitting stable data from presentation touches large checked-unit structures. → Reuse the existing body presentation/renumbering logic temporarily as the sole projection boundary, then delete only its validity and dependency-scanning portions.
- [Risk] Missing a hidden body would admit an incomplete unit. → Build and fingerprint the unit from the compiler's complete body registry and add a generated-body invalidation fixture.
- [Risk] Budget charging can double-count nested hits. → Associate cost with the owning application query and charge only at the public demand boundary, with focused hit/miss accounting tests.
- [Risk] Cycles can span new families. → Keep reservation and cycle detection centralized in the shared runtime and test a cross-family cycle/abort path.

## Migration Plan

1. Add stable identities and semantic projections for complete checked units and applications.
2. Route body construction through the shared runtime while retaining only current-revision presentation helpers.
3. Split evaluation and residual construction into separate providers with dependency and cost capture.
4. Route ownership through the shared runtime and remove independent reuse maps.
5. Delete superseded validity scanners, local cache admission, and obsolete snapshot fields; update all callers and focused fixtures in the same change.
