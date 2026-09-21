# Design

## Context

SemanticQuery currently stores closure-backed requests in one session and identifies records with
strings. Semantic builds those closures over a revision's DeclarationIndex and NameResolution
objects. Observations describe semantic concepts but do not all have current readers, while
qualified lookup diagnostics can capture a caller anchor. Header completion is eager before the
session is created and remains so. See proposal.md and the three delta specs.

## Goals / Non-Goals

**Goals:**

- Make every reusable request and dependency reconstructible in another revision.
- Admit previous records only through one ordered validator and stop invalidation at equal results.
- Separate immutable semantic content from current declaration and diagnostic presentation.
- Preserve eager header components and current cycle behavior.

**Non-Goals:**

- Lazy or incremental declaration completion, a new fixed-point engine, or extraction of
  DeclarationCompletion's internal graph.
- Body/evaluation migration, disk persistence, HIR caching, backend caching, or new IR.

## Decisions

### Use a closed descriptor union and exhaustive provider registry

SemanticQuery.Descriptor will name query family, schema version, stable authored owner or logical
scope, and canonical parameters. The runtime will encode the address and ask a session-owned
provider registry to execute or project it. Providers can use current indices internally, but no
closure crosses the session boundary.

Alternative: retain generic request closures and add a second reconstructible manifest. Rejected
because the manifest could drift from execution and would leave two authorities for identity.

### Represent dependencies as typed query and leaf reads

QueryRead records a descriptor/address plus expected result fingerprint. InputRead records a typed
leaf address plus expected fingerprint. A leaf reader projects current completed header components,
exact namespace membership, binding selection, candidate sets, and configuration. The same
dispatcher validates both reads.

Alternative: infer leaf dependencies by scanning returned declarations. Rejected because absence,
conflicts, selection, and obsolete branches are not recoverable from the result alone.

### Transfer only one previous snapshot

Creating a new session accepts an optional immutable snapshot from the immediately preceding
analysis. It owns empty current records and counters. Validation recursively admits reachable
previous records to current storage, while execution replaces dependencies. The finalized current
snapshot can seed one later session; it does not retain its predecessor.

Alternative: keep session objects linked. Rejected because closures and revision objects would stay
live and retention would be unbounded.

### Compare semantic and diagnostic projections explicitly

Each provider owns a canonical result projection and fingerprint. Reusable name outcomes hold stable
candidate identities and location-independent reasons. Presentation resolves current declarations
and instantiates use-site diagnostics after reuse. Where semantic and diagnostic consumers differ,
the provider publishes separate observable projections so a diagnostic-only change does not force
unrelated semantic work.

Alternative: fingerprint the existing lookup object. Rejected because it embeds current
declarations and caller-specific causes.

### Treat eager header components as atomic leaf inputs

The validator does not enter unresolved declaration-completion work. Only completed current
components may be read, with stable member identity plus canonical semantic/diagnostic content in
their fingerprint. Merge and split therefore change the leaf even if one member looks equal.

Alternative: recursively validate incomplete header jobs. Rejected because that would introduce a
new fixed-point engine and could admit unknown active data.

### Preserve reservations and provide a whole-runtime fresh mode

Reservation release stays bracketed around provider execution and only completed exits publish.
Fresh mode disables previous and current reuse for root and nested reads while using the same
providers and presentation. Counters separately record validations, executions, and reuses.

Alternative: bypass only the requested root. Rejected because nested cache hits would weaken the
fresh comparison oracle.

## Risks / Trade-offs

- **Projection omissions can admit stale results** -> centralize family projection and leaf readers,
  cover positive and negative cases structurally, and compare against whole-runtime forced fresh.
- **Canonical encoders can accidentally include positions or ordinals** -> keep address and
  fingerprint encoders typed and test insertion/movement explicitly.
- **Recursive validation can encounter active current work** -> treat active/unresolved reads as
  invalid and execute through the existing cycle policy.
- **Caller diagnostics can leak from reused lookup objects** -> reusable outcomes contain no anchors
  or spans; presentation requires the current request anchor.

## Migration Plan

1. Replace the runtime request/observation model and add snapshot validation/counters.
2. Add current leaf readers and stable projections for completed headers and name inputs.
3. Migrate name, qualified/associated, type, alias, bound, and header-conformance providers.
4. Remove closure/string request paths and synthetic dependency keys.
5. Update focused revision fixtures and architecture documentation, then publish the current
   snapshot for JUL-218.
