# Design

## Context

`Frontend.analyzeHeaders` currently performs declaration collection, preliminary resolution,
declaration completion, and final name resolution by directly composing `NameResolution` and
`DeclarationCompletion`. `NameResolution` exposes resolver closures and lookup helpers, while
`BodyQuery` reconstructs selected semantic dependencies from completed TIR and diagnostics. The
JUL-209 boundary cleanup introduced the `Semantic` actor and final operation names but deliberately
did not introduce a query engine. See `proposal.md` and the three delta specs for the new contract.

## Goals / Non-Goals

**Goals:**

- Establish a small typed query runtime that owns active/completed jobs and ordered dependency
  observations for one immutable session.
- Move declared-type, name, and header-conformance reads behind the runtime without changing their
  semantic result shapes.
- Make the session the only header reader available to later body/evaluation work and preserve
  current diagnostics, recursion, and preparation behavior.
- Keep fresh execution as a first-class correctness oracle.

**Non-Goals:**

- Cross-revision or persistent reuse, red/green invalidation, disk codecs, or worker scheduling.
- Body checking or static evaluation providers, which belong to JUL-211.
- New inference, conformance, import, visibility, or opaque-type language behavior.
- Downstream instance, layout, MIR, emission, or linking extraction.

## Decisions

### Add a dedicated `SemanticQuery` actor and keep `Semantic` as the public operation actor

`SemanticQuery` will own immutable request/answer types, session state, dependency observations,
reservation/publication, and provider dispatch. `Semantic` will expose named public operations such
as `resolveName` and `typeOf` with `Semantic.*` tracing. This keeps the low-level runtime reusable
without turning `Semantic.ts` into a grab bag and follows the repository's one-module-per-actor
rule.

Alternative: put the store and all providers in `Semantic.ts`. Rejected because it mixes the
session data actor with multiple semantic operations and makes JUL-211 harder to extend cleanly.

### Use closed request variants with canonical string keys

The initial request union will contain declared-type and name-resolution variants. Each variant
stores stable declaration/scope identities plus canonical substitutions/evidence where relevant.
The query key encoder is exhaustive over the closed union and excludes source spans and current
presentation. Provider registration is static and exhaustive rather than a runtime string registry.

Alternative: generic string names and `unknown` payloads. Rejected because they erase result typing,
permit duplicate dispatch, and make public error channels imprecise.

### Model active reservations separately from completed answers

Each request key has at most one active reservation. The reservation records its ordered dependency
reads and stack parent. Only a successful provider exit publishes a frozen completed result.
`Effect.acquireUseRelease` brackets the reservation so failure, defect, and interruption remove it
without replacing the original exit. Completed semantic rejection remains ordinary answer data.

Alternative: poison failed keys or retain rejected promises. Rejected because Silk's contract
requires cancellation retry and no transient-state cache entries.

### Preserve current algorithms behind provider-owned projections

The name provider will call the existing lookup rules through an immutable scope/index projection;
the declared-type provider will read completed header facts and the conformance/alias answers used
to form them. Direct public resolver construction and index lookup entry points will be removed or
made private in the same change. Existing lookup outcome types remain the answers to avoid a second
semantic representation.

Alternative: rewrite resolution and declaration completion algorithms together with the boundary.
Rejected because it expands the change into language semantics and obscures migration regressions.

### Attach observations to execution, not completed-result scanning

Provider-side read helpers record namespace membership, candidate enumeration, binding selection,
header/alias/bound/conformance reads, and profile facts as they occur. A nested query records a
request dependency before returning, even on a memo hit. Structural observation values remain
immutable and comparable so JUL-211 can feed them to existing cross-revision body validation.

Alternative: continue deriving dependencies from TIR and diagnostic codes. Rejected because
negative/candidate observations are incomplete and couple invalidation to output representation.

### Create one session per preparation selection epoch

Preparation constructs the session only after it has an immutable authored closure, normalized
profile, and selected module environment. Any selection/binding/candidate/profile change creates a
new session; no answer maps transfer. The final prepared frontend retains the sealed session but no
source resolver capability. This is intentionally coarse and correct until a later invalidation
milestone.

Alternative: key a global store by source file sets. Rejected because selection can change inside
the same loaded set and because unvalidated answer transfer is outside B1.

## Risks / Trade-offs

- **[Migration can leave a direct resolver escape hatch]** → make migrated helpers private, search
  all callers, and add a sealed-boundary structural fixture with no source/resolver capability.
- **[Recursive headers can be mistaken for active-query cycles]** → keep header completion and
  declared signature availability separate from body demand; only prohibited alias/inline cycles
  fail with an explicit dependency path.
- **[Observation coverage can be incomplete]** → centralize semantic reads in provider-owned access
  helpers and test negative membership, full candidate sets, imported selection, and memo-hit edges.
- **[Cancellation is limited by synchronous inner loops]** → test supported Effect/query yield
  boundaries and do not claim new synchronous-loop preemption.
- **[Session creation may recompute more than necessary]** → accept coarse per-epoch sessions now;
  cross-revision validation and persistence remain explicit later milestones.

## Migration Plan

1. Introduce request, session, observation, and reservation/publication data structures with isolated
   unit fixtures.
2. Add name and declared-type providers over immutable header projections and prove fresh/memoized
   agreement, recursion, and cancellation behavior.
3. Construct the session during header preparation and migrate declaration completion, final name
   resolution, header conformance reads, and downstream header consumers.
4. Delete public direct resolver/header dispatch, seal the session in the frontend artifact, and
   migrate exports, traces, reports, tests, and documentation.
5. Roll back by reverting the complete stack layer; no persisted data or compatibility path exists.
