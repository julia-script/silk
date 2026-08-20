## Context

See [proposal.md](proposal.md) for motivation. The current server calls a project scheduler directly
from document synchronization. That scheduler stores mutable `pending`, `active`, committed-map,
waiter, and idle-deferred state, but the worker resets `active` and completes `idle` only on its
successful tail. A compiler defect therefore leaves an impossible state: `active` is true, no worker
exists, and every future edit, exact-version request, and shutdown observes the false marker.

The compiler trigger is also a contract mismatch rather than an LSP-only error. Pattern parsing can
recover a non-nominal type primary inside a `NominalPattern`; elaboration then asserts that the node
contains `AppliedType` or `TypePath`. Editor input routinely visits these intermediate forms.

Even on successful analysis, document synchronization remains pending for the whole analysis, a
fixed sleep is used instead of trailing-edge debounce, obsolete work is not interrupted, all
requests wait for a global document-update set, and pure frontend phases can occupy the Node event
loop for hundreds of milliseconds. Project semantic reuse is valuable and must remain based only on
the last complete atomic commit.

The implementation must follow the repository's Effect lifecycle rules: workers and lifecycle
resources are scoped, defects are observed through `Exit`/`Cause`, and cleanup is not duplicated
across success and failure branches.

## Goals / Non-Goals

**Goals:**

- Give recovered pattern syntax and semantic facts an explicit unavailable representation.
- Make the project scheduler a supervised latest-state actor with unconditional lifecycle cleanup.
- Keep at most one active and one latest pending project revision, with real trailing-edge debounce.
- Introduce cooperative interruption points so the protocol loop can observe newer edits and cancel
  obsolete frontend work without discarding the last committed incremental-analysis basis.
- Make request waiting and update ordering project/document scoped.
- Preserve exact-version atomic snapshots and diagnostic versioning across success and failure.
- Make server shutdown and extension restart terminate in every scheduler state.

**Non-Goals:**

- Changing Silk grammar, match semantics, diagnostic wording/codes, or standard-library APIs.
- Serving semantic answers from an older snapshot against current document coordinates.
- Adding timing assertions to correctness tests or promising a machine-specific millisecond SLA.
- Moving all compiler data and editor queries behind a worker-thread RPC boundary in this change.
- Reusing partial work from a failed or interrupted analysis.

## Decisions

### 1. Represent invalid recovered patterns explicitly

The syntax vocabulary will gain an `ErrorPattern` node and the semantic pattern vocabulary will gain
an `UnavailablePattern` fact. `parsePattern` will select nominal parsing only when the nominal start
predicate is satisfied; otherwise it will retain the damaged tokens in `ErrorPattern` and emit one
parser-owned recovery diagnostic. Elaboration will translate that node into an unavailable pattern
with its identity, empty bindings/omissions, and exact syntax provenance.

Coverage, occurrence indexing, tooling projections, HIR eligibility, ownership, and lowering will
handle `UnavailablePattern` exhaustively. It contributes no selected member, bindings, or coverage,
and makes the enclosing construct incomplete without inventing a semantic diagnostic.

This is preferred over catching `RangeError` around the whole compiler because a catch would lose
the useful facts from unrelated declarations and modules. It is also preferred over synthesizing a
fake missing `TypePath` beneath every malformed pattern: source such as `[` or `&` is not a damaged
nominal path and should not be represented as one.

The same rule applies during the source-reachable invariant audit: a parser-recovered shape gets an
explicit unavailable fact at the smallest affected boundary. Assertions remain appropriate for
compiler-state corruption that cannot be produced by source recovery, but not for an omitted or
unexpected CST child.

### 2. Make project sessions scoped actors whose mutations return promptly

Project-session construction will become scoped and will own a single supervisor fiber. `open`,
`close`, and `invalidate` will update the actor's desired project state, increment the accepted
revision, signal the supervisor, and return; they will no longer run the debounce and compiler job
inside the document-notification operation.

The actor state has four authoritative parts:

- the latest synchronized document map and accumulated invalidation;
- the accepted revision and optional newest pending captured revision;
- the last complete committed revision/map; and
- exact-version waiters keyed by document URI and version.

There is no standalone boolean whose truth can outlive its worker. Active work is represented by an
owned fiber handle. Fiber exit is observed exactly once, and an unconditional finalizer removes the
handle, settles impossible waiters, completes shutdown/idle observers, and signals the supervisor
when newer pending work exists.

The analysis callback will be observed with `Effect.exit`. Success may commit only when its captured
revision is still current. Typed failure, defect, and interruption never mutate the committed map.
If the failed revision is current, the actor emits a revision-failed event for each affected open
document and settles its waiters with no session. If a newer revision exists, the supervisor
continues with that latest state.

This is preferred over adding `Effect.ensuring` around only `active = false` in the current loop.
That narrow patch would fix the captured deadlock but retain synchronous notification handling,
non-resettable debounce, implicit worker identity, and fragile idle/waiter bookkeeping.

### 3. Use a resettable latest-state signal and cooperative preemption

The supervisor consumes a capacity-one/sliding latest-state signal. Before starting analysis it
waits for the debounce interval; receipt of another signal replaces the captured pending revision
and resets the interval. Once analysis begins, accepting a newer revision interrupts the active
analysis fiber. The interrupted exit is discarded and the supervisor debounces the newest desired
state. Queue size is therefore independent of edit count.

Effect interruption alone cannot preempt a long pure JavaScript phase. Project frontend analysis
will expose interruption checkpoints between closure discovery, header/index construction, module
semantic work, ownership/tooling work, and bounded module batches inside global phases. Each
checkpoint yields to the runtime and checks interruption before continuing. Existing immutable
phase values and structural reuse remain unchanged; only orchestration becomes cooperative.

An opt-in pressure benchmark will record accepted edits, started/completed/interrupted analyses,
phase durations, semantic reuse observations, and time from the final edit to its commit. These are
observations, not correctness thresholds. If one phase remains an event-loop monopolist after
checkpointing, its loop will be split into smaller deterministic batches. A worker-thread boundary
is deferred because project snapshots and all semantic editor queries currently share rich
in-process data; moving them would require a separate project-worker RPC architecture and would
discard or serialize the incremental state this change is preserving.

### 4. Scope synchronization and acquisition to the target document/project

The server will replace the global `documentUpdates` barrier with synchronization handles keyed by
document URI. Updates for one URI remain ordered. After the current URI's lightweight workspace
discovery/enqueue operation finishes, a semantic request asks only that document's project session
for its captured version. File-watcher invalidation signals each affected project independently and
does not enter a server-global request barrier.

`ProjectSession.acquire(uri, version)` has exactly three terminal outcomes:

- the exact version committed, returning its analyzed document;
- the version was superseded, closed, failed, interrupted, or the session shut down, returning
  none; or
- the version is still the actor's current pending/active revision, in which case the caller waits
  on its keyed deferred.

This preserves exact-version coherence while allowing unrelated projects to answer independently.
It is preferred over serving the last good snapshot during refresh because current UTF-16 positions
cannot safely be interpreted against older bytes.

### 5. Treat analysis failure as a versioned diagnostic transition

Successful commits continue to publish the compiler diagnostics with the analyzed document
version. When the current revision exits without a snapshot, the session's failure event causes the
server to publish an empty diagnostic set tagged with that current version. The `Cause` is logged to
the language-server output channel, not converted into a source diagnostic, because an internal
failure is not a claim about the user's program.

Older diagnostics may remain visible while a current analysis is legitimately pending, but they
cannot survive a terminal failure as if they described the current text. A later successful
revision publishes normally. This is preferred over clearing diagnostics immediately on every
keystroke, which would create visible flicker and discard still-useful feedback during ordinary
short analysis windows.

### 6. Shutdown closes scopes; restart always creates a fresh client

Project shutdown marks the actor closed, settles every waiter, clears pending state, interrupts the
owned analysis/supervisor fibers, and closes their scope. It does not await a separately maintained
`active` marker. Server shutdown first prevents new project creation, closes all project scopes,
then disposes the shared runtime and protocol resources.

The extension will centralize client construction and serialize restart requests. Restart attempts
a normal stop; whether stop succeeds or times out, the old client is retired and a fresh
`LanguageClient` instance is constructed and started. A failed replacement start leaves the owned
client state explicitly absent/stopped and reports that startup error. Reusing
`LanguageClient.restart()` is rejected because its implementation awaits `stop()` and skips
`start()` when stop rejects—the exact behavior observed in the incident.

## Risks / Trade-offs

- **[Risk] Cooperative cancellation is only as responsive as its longest uninterrupted phase.** →
  Preserve phase observations, add checkpoints at phase and bounded-batch boundaries, and use the
  pressure benchmark to locate remaining monopolists before considering worker isolation.
- **[Risk] Adding unavailable pattern variants touches many exhaustive consumers.** → Keep the
  variant intentionally data-poor, update every consumer in the same green-field change, and use
  exhaustive TypeScript switches plus compiler tests for match, `let`, and `if let` recovery.
- **[Risk] Interrupting stale work reduces incremental reuse when edits outpace analysis.** → Keep
  the last committed project immutable and reusable; never discard it merely because an attempted
  successor was interrupted.
- **[Risk] Empty diagnostics on internal failure can temporarily hide real source errors.** → Log
  the full internal cause, expose server health in the output channel, and prioritize immediate
  processing of the next accepted revision; never publish fabricated source diagnostics.
- **[Risk] Fresh-client restart can briefly overlap with termination of the old child process.** →
  Detach old protocol listeners before starting the replacement, serialize restart commands, and
  rely on stdio process isolation so the retired process cannot publish into the new client.
- **[Risk] Recovery pressure coverage can make the critical test suite expensive.** → Keep a small
  deterministic prefix/deletion canary corpus in correctness tests and put broad generated sweeps
  and latency observations in opt-in pressure targets.

## Migration Plan

1. Introduce explicit unavailable-pattern syntax/facts and make all frontend consumers exhaustive;
   retain the failing pattern prefixes as regression cases.
2. Add the scoped project-session actor and deterministic scheduler failure/preemption tests, then
   switch the server to enqueue-only document synchronization.
3. Add frontend interruption checkpoints and the opt-in rapid-edit pressure benchmark.
4. Replace global request synchronization with URI/project-scoped acquisition and add multi-project
   protocol tests.
5. Add failure diagnostic publication and bounded shutdown tests.
6. Replace extension restart handling with fresh-client lifecycle ownership and test healthy,
   timed-out-stop, and replacement-start-failure paths.
7. Run the repository verification sequence and manually repeat the original Cursor reproduction:
   trigger recovered match syntax, repair the source, confirm diagnostics recover, and restart the
   server once.

There is no persisted-data or public-API migration. Rollback is a single repository revert; no
compatibility path or dual scheduler is retained.
