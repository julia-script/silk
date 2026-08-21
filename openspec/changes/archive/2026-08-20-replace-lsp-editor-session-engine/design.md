## Context

See [proposal.md](proposal.md) for motivation. The production path currently distributes one
revision-coherence invariant across `vscode-languageclient`, `TextDocuments`, per-URI
`DocumentUpdates`, workspace discovery, `ProjectSession`, compiler fibers, push-diagnostic side
effects, and a replaceable extension client. The server publishes diagnostic versions, but the
installed VS Code language client advertises no push-version support and discards those versions.
`ProjectSession.acquire` also infers pending work from the absence of a committed view, so requests
created after a terminal failure can wait forever.

The compiler already exposes immutable `ProjectAnalysis` values and sound revision reuse. A direct
`valid → effects → effec → Effect. → valid` revision sequence recovers without retaining old
diagnostics. The architecture therefore needs a deeper editor-session seam and hard execution
isolation; this change does not need a second semantic implementation.

The extension and server are private green-field packages. No current internal interface or
process layout is a compatibility contract, so the superseded scheduler and lifecycle paths can be
deleted in the same change.

## Goals / Non-Goals

**Goals:**

- Put accepted text, project generations, terminal status, waiters, and worker health behind one
  deep workspace-engine interface.
- Keep the protocol event loop responsive even when compiler JavaScript does not yield.
- Give every accepted update and semantic request one bounded terminal outcome.
- Make diagnostic freshness correct through the production VS Code client rather than relying on
  ignored push versions.
- Commit and reuse immutable analysis independently of diagnostics, inspection, or transport
  backpressure.
- Make client/server replacement an acknowledged generation transition with stable consumers and
  built-in document synchronization plus an acceptance barrier.
- Provide deterministic in-process tests plus real worker, stdio, and extension-host evidence for
  the transition that originally failed.

**Non-Goals:**

- Changing Silk syntax, semantics, diagnostic codes, or the compiler analysis-facade vocabulary.
- Building a general-purpose worker pool or making compiler snapshots serializable.
- Preserving `DocumentUpdates`, the current `ProjectSession` interface, push diagnostics, or the
  exposed replaceable-client reference.
- Converting the entire compiler into a fine-grained dependency database in this change. The worker
  keeps the existing committed `ProjectAnalysis` reuse; later phase-level optimization must be
  justified by measurements behind the new seam.
- Guaranteeing that every semantic query succeeds before its deadline. Unavailable, superseded,
  canceled, deadline-exceeded, and analysis-failed are valid terminal outcomes.

## Decisions

### 1. One workspace engine owns revision truth

The protocol adapter will depend on one `WorkspaceEngine` module with three conceptual operations:

```ts
interface WorkspaceEngine {
  readonly accept: (event: SourceEvent) => Result.Result<Acceptance, ProtocolViolation>
  readonly request: <Q extends EditorQuery>(query: Q) => Effect.Effect<QueryOutcome<Q>, never>
  readonly shutdown: Effect.Effect<void>
}
```

`accept` is a synchronous, non-yielding mutable transition. It records `(uri, version, bytes)` or
close state in a monotonic source ledger, updates desired project state, and signals a capacity-one
background queue with `Queue.offerUnsafe` before returning a typed acceptance result. It performs
no filesystem, discovery, compiler, promise, or Effect execution. Consequently the JSON-RPC
dispatcher cannot deliver a later request that still observes the previous accepted version.
Discovery and analysis consume the signal asynchronously.
The engine owns cached URI-to-project association, project generations, filesystem invalidations,
pending work, exact-generation requests, analysis-context epochs, and health. `request` is an
ordinary interruptible Effect whose scoped finalizer removes its waiter and cancels worker work;
the engine interface does not expose an editor-specific cancellation primitive. `Server.ts`
retains only JSON-RPC/LSP translation, capability negotiation, token-to-fiber interruption, and
runtime startup.

Project discovery is asynchronous and coalesced. A newly opened URI initially has a stable
provisional identity; discovery migrates it to a manifest-backed project only if the discovery
result still names its latest version. When one or several provisional identities resolve to the
same manifest-backed project, the engine atomically merges their latest ledger entries into one
new project generation, supersedes and settles every provisional generation, and retires any
provisional worker. Manifest or source-root reassociation performs the same terminal handoff.
Subsequent text edits reuse the association. Manifest, source-root, close, or relevant filesystem
changes explicitly invalidate the association. This removes per-keystroke manifest search and
root-file reads without making project identity ambient.

This is preferred over repairing `DocumentUpdates` because a FIFO before a latest-state scheduler
still preserves obsolete work, and callers would still need to coordinate two revision owners.

### 2. Project generations use explicit terminal states

Each project context has one monotonic generation and one analysis-context epoch. A generation is
represented explicitly as:

```ts
type GenerationState =
  | { readonly _tag: 'Pending'; readonly generation: Generation }
  | { readonly _tag: 'Committed'; readonly generation: Generation }
  | { readonly _tag: 'Failed'; readonly generation: Generation; readonly incident: IncidentId }
  | { readonly _tag: 'Superseded'; readonly generation: Generation }
  | { readonly _tag: 'Closed'; readonly generation: Generation }
```

The engine retains one last committed worker snapshot, one active generation, and at most one
latest pending desired generation per project. It does not retain a linked history or one promise
per edit. Accepting newer source terminally supersedes the older desired/active generation and
settles its requests. Worker results carry both epoch and generation and are discarded unless both
are still authoritative.

After accepted input quiesces, a healthy project's supervisor must continue until the newest
desired generation commits. Query deadlines do not change generation health or satisfy this
liveness obligation; they bound individual callers while background analysis continues. A
deterministic supervisor test owns this invariant, while pressure measurements report its observed
latency.

`request` captures the current URI version, project generation, and worker epoch inside the engine.
`Committed` executes the query; `Pending` registers one scoped waiter; every terminal state returns
immediately. Waiters are keyed by request id and generation, removed by cancellation, and settled
on every state transition. A code-action descriptor that names an older generation resolves as
superseded rather than consulting current coordinates.

This is preferred over adding `committedRevision === revision ? none : wait` because an enum makes
failure, missing root views, close, supersession, and worker retirement exhaustive throughout the
module and its tests.

### 3. Production project analysis runs in worker threads

Each active project uses one Node worker thread that owns its rich `ProjectAnalysis`, committed
reuse basis, inventory, realized inspection cache, and pure document-query execution. Worker
threads provide a separate event loop and can be forcibly terminated when compiler JavaScript is
non-cooperative. The protocol process never runs compiler phases or semantic document projections.

Only editor-neutral messages cross the seam:

- host to worker: initialize/rebuild input, analyze generation, supersede analysis, semantic query,
  cancel query, shutdown;
- worker to host: ready/heartbeat, generation committed/failed, query result, incident, stopped;
- source inputs: immutable module identities, bytes, origins, source-root and invalidation facts;
- query results: diagnostics, spans/ranges, hover Markdown, locations, edits, symbols, hints,
  completion items, inspection projections, and explicit unavailable outcomes.

Snapshots never cross the worker boundary. Query computation remains beside the snapshot that owns
its identities. The worker retains only the last complete commit as the next reuse basis; partial,
failed, superseded, or interrupted work is discarded.

The production adapter is `ProjectWorker` backed by `node:worker_threads`. Deterministic scheduler
tests use a controlled in-process adapter with virtual time, failures, blocked sends, and explicit
commit gates. Real-worker tests prove serialization, termination, and reconstruction.

A child process per project was rejected because it adds process startup and memory cost without
additional protection needed for the current pure-TypeScript compiler. Cooperative fibers alone
were rejected because they cannot preempt a long synchronous phase.

Accepting a newer generation while analysis is active marks the old generation logically
`Superseded` immediately and sends `SupersedeAnalysis(oldGeneration)` to the worker. At the next
compiler checkpoint the worker interrupts that analysis, discards its partial state, acknowledges
supersession, and starts only the latest pending generation after trailing-edge debounce. If the
worker cannot acknowledge within the superseded-work lease, the host terminates it, waits for exit,
and creates a replacement only after input quiesces. Supersession retirement is user-driven control
flow, not an analysis failure, and does not consume the failure budget. This avoids both waiting for
the full no-progress lease on every edit and destroying warm state immediately when cooperative
interruption is working.

### 4. Health policy is bounded and cannot form an automatic restart loop

The worker sends readiness and progress events at protocol receipt, compiler checkpoints, commit,
and query completion. The host owns configurable production defaults for:

- trailing analysis debounce: 25 ms;
- ordinary semantic query deadline: 2 seconds;
- diagnostic query deadline: 5 seconds, returning a retriggerable server-cancel outcome;
- superseded-work acknowledgement lease: 500 ms;
- no-progress worker lease: 10 seconds;
- graceful worker/server retirement: 2 seconds before forced termination.

Tests use virtual time and assert transitions, not machine performance. Pressure benchmarks record
latency and churn separately.

After three consecutive worker failures without an intervening successful commit, the project
opens a circuit and automatic restart stops. A newer source/filesystem generation that reaches the
trailing edge permits one half-open replacement attempt; further edits coalesce while that attempt
is pending and do not reset the budget. A successful commit closes the circuit and resets the
failure count. An explicit server restart creates a new engine epoch with a fresh budget. This
prevents a deterministic compiler defect or sustained typing from creating an infinite automatic
worker-restart loop while still allowing repaired source to recover.

Deadlines terminate only the query; they do not discard an otherwise healthy project analysis.
Worker lease expiry terminally fails the generation, settles its requests, terminates the worker,
and rebuilds the newest desired generation when the failure budget permits. Superseded-work lease
expiry retires the worker and rebuilds the latest desired generation but does not increment the
failure count.

### 5. Commit precedes observer delivery

On an authoritative worker commit, the engine first records `Committed`, installs the worker
snapshot handle/epoch as the query and reuse basis, and settles exact-generation waiters. Only then
does it emit commit events used for diagnostic refresh and inspector invalidation. Observer events
are best-effort outputs of committed state; they are never part of analysis success and cannot roll
back or block the commit.

The engine exposes a stable stream of committed-revision and health events. A slow subscriber uses
a sliding latest event per consumer and cannot block the engine mailbox. This is preferred over
calling `sendDiagnostics` or `sendNotification` inside the analysis fiber.

### 6. Diagnostics use the LSP pull model

The server advertises document diagnostic support and removes `sendDiagnostics` from ordinary
analysis. A diagnostic request enters `WorkspaceEngine.request` with the current synchronized URI,
client cancellation, and diagnostic deadline. A committed result returns a full report with a
result id derived from worker epoch, project generation, document version, and dependency revision.
An identical previous result id returns unchanged. Superseded requests are canceled/discarded;
failed current generations return a current full-empty/unavailable report rather than leaving an
older collection authoritative.

Project commits caused by open dependencies or watched filesystem inputs invoke the standard
`workspace/diagnostic/refresh` request when the client advertises refresh support. Direct text
changes rely on the extension's document diagnostic lifecycle. Standard pull requests, protocol
conversion, cancellation, and refresh still pass through `vscode-languageclient`, but its built-in
pull feature is retired after capability negotiation. That feature owns a second private diagnostic
collection, so the stable editor session cannot synchronously clear findings from it on edit or
prevent it from applying a retired generation after replacement.

Pull cancellation does not itself remove diagnostics that were already applied before the edit.
`EditorSession` therefore delegates one generation-scoped `EditorDiagnostics` actor that owns the
sole visible collection. It listens after built-in source synchronization, deletes the URI and
cancels its active pull immediately on editor change or close, sends a standard
`textDocument/diagnostic` request through `LanguageClient`, and captures the document version and
client generation before conversion. A response can update the collection only if its request,
version, and generation are still current; retriggerable server cancellation reschedules only the
current document. Standard refresh pulls every current open Silk document. This pairs immediate
retirement of old findings with exact edge gating and removes competing collection ownership.

Keeping versioned push diagnostics as a fallback was rejected. The production client explicitly
advertises no push-version support, and dual push/pull collections would create another ownership
race. Clients without pull diagnostics receive no diagnostics but retain other language features;
the private Cursor/VS Code extension requires pull support.

### 7. LSP cancellation interrupts Effect requests and worker queries

Every request handler accepts the protocol cancellation token. The protocol adapter runs the
engine request in a scoped Effect fiber and interrupts that fiber when the token cancels.
Interruption finalizers remove any engine waiter immediately and send worker-query cancellation
when execution has begun. Result messages for a canceled request id are ignored.

Internal outcomes are explicit:

```ts
type QueryOutcome<A> =
  | { readonly _tag: 'Ready'; readonly value: A }
  | { readonly _tag: 'Superseded' }
  | { readonly _tag: 'Canceled' }
  | { readonly _tag: 'DeadlineExceeded' }
  | { readonly _tag: 'AnalysisFailed'; readonly incident: IncidentId }
  | { readonly _tag: 'Unavailable' }
  | { readonly _tag: 'Closed' }
```

The LSP adapter maps these to the method-appropriate null/empty result or standard cancellation
error. Diagnostic deadline uses `ServerCancelled` with retrigger enabled. No raw `unknown` error or
worker exception crosses the interface.

### 8. The extension owns one stable editor session

`LanguageClientLifecycle.current()` and the mirrored `inspectorClient` global are removed. The
extension constructs one `EditorSession` for its activation lifetime. Commands and the inspector
depend on stable session operations/events, never on a replaceable concrete `LanguageClient`.

The session owns a `ServerProcess` adapter that spawns the LSP and retains the child handle while a
custom detached `StreamInfo` stdio transport connects it to `LanguageClient`; language-client
cleanup owns protocol resources and the session exclusively owns process termination. Process
termination therefore remains observable outside the language-client stop promise.

`vscode-languageclient` remains the sole sender of `didOpen`, `didChange`, and `didClose`; the
session never manually duplicates document notifications. Before client start, the session
subscribes to the built-in text-synchronization feature's notification-sent events. Those callbacks
are lossy wake-up signals, not evidence that the server has accepted a version: a client may deliver
an already-open document without replaying that delivery to a late or implementation-specific
observer. After initialization, the session issues an ordered custom acceptance-barrier request
naming the current `(uri, version)` entries. The server answers true only after its synchronous
ledger contains all of them. A false response waits for a later synchronization signal and retries;
a true response for an unchanged current set is authoritative even when the observer missed the
corresponding `didOpen`. A generation becomes authoritative after process spawn, initialization,
stable feature/subscription binding, and server barrier acknowledgement. Stable event subscribers
are rebound internally before readiness.

Restart is serialized:

1. mark the old generation retiring and cancel its session requests;
2. stop/dispose the language client and close its transport;
3. await process exit until the retirement deadline, then terminate and await exit;
4. only after the old generation cannot publish, construct/start/bind the replacement and await the
   server-authoritative acceptance barrier, using built-in synchronization observations only to
   wake retries;
5. mark ready or leave the stable session explicitly unavailable with a typed failure.

Starting a replacement immediately after ignoring `LanguageClient.stop()` failure was rejected
because it does not prove process death or stable-subscriber rebinding. Reloading the editor window
was rejected as a recovery mechanism because restart is an explicit extension capability.

### 9. Verification targets the real failing transitions

The correct primary seam is the workspace-engine interface. Deterministic tests cover:

- source acceptance coalescing before discovery and cold first commit;
- delayed lower-version rejection and provisional-to-manifest or manifest-driven reassociation
  while semantic and diagnostic requests are pending;
- explicit terminal states, including requests created after failure and after a commit missing a
  root view;
- cancellation and deadline removal of pending/executing hover and code-action requests;
- commit visibility while observer delivery is blocked;
- worker non-cooperation, forced retirement, rebuild, and bounded crash-loop policy;
- dependency invalidation and exact diagnostic result ids;
- immediate retirement of already-applied findings, version/generation-gated pull responses, close
  cleanup, and defect-to-valid diagnostic recovery through the production client adapter;
- the exact `effects → effec → Effect. → valid` transition while hover, completion, code actions,
  and diagnostic pulls overlap.

Real stdio tests exercise pull diagnostics and cancellation through the server binary. Real worker
tests deliberately block the worker event loop and prove the protocol remains responsive.
Extension lifecycle tests use a process/transport adapter that exposes exit, synchronization,
acceptance acknowledgement, subscriptions, and diagnostic generation—not a fake with only
`start()`/`stop()`. An opt-in Extension Development Host test passes through production
`vscode-languageclient` synchronization, diagnostic transport/conversion, the extension-owned
collection, and the real editor model; the manual launch-script reproduction remains a
release-candidate check until that host test is stable in CI.

The existing pressure benchmark is revised to include source acceptance, discovery, real compiler
work, overlapping same-document queries, cancellation counts, worker replacements, and final
commit. It records rather than asserts machine-specific latency, while deterministic virtual-time
tests assert bounded state transitions.

## Risks / Trade-offs

- **[Risk] Worker startup and one compiler heap per active project increase latency and memory.** →
  Create workers lazily, retire idle/closed projects, retain one worker per active project, and
  measure cold/steady memory in the pressure target.
- **[Risk] Query/result DTOs become a broad internal protocol.** → Keep one discriminated query
  catalog, derive request/result typing from it, version messages by the server build rather than
  supporting compatibility, and keep compiler semantic graphs private.
- **[Risk] A legitimate large project exceeds the default worker lease.** → Emit checkpoint
  progress, make policy configurable for development, record phase observations, and require a
  newer source generation or explicit restart after the bounded same-input failure budget.
- **[Risk] Pull diagnostics behave differently in non-VS Code clients.** → Advertise the standard
  capability accurately, test the private extension's production client, and do not maintain a
  racy push fallback.
- **[Risk] Worker termination loses warm incremental state.** → Rebuild from the authoritative
  source ledger and last filesystem state; correctness and recovery take precedence over warm
  reuse after an unhealthy context.
- **[Risk] Extension process ownership can conflict with language-client cleanup.** → Use one
  explicit process/transport adapter, make shutdown idempotent, and integration-test normal exit,
  timeout, forced termination, and startup failure.
- **[Risk] This is a large atomic replacement.** → Build the new modules behind test adapters, move
  all callers once their interface is complete, delete old paths in the same change, and retain a
  single-commit rollback until archive.

## Migration Plan

1. Add the typed source ledger, generation state model, query catalog/outcomes, and controlled
   in-process worker adapter with deterministic transition tests.
2. Implement the production worker-thread protocol and move project analysis plus document queries
   behind it; prove forced termination and reconstruction.
3. Implement the workspace engine, cached/coalesced discovery, cancellation/deadline handling, and
   commit event stream.
4. Convert server handlers to the engine, replace push diagnostics with pull/refresh, and add real
   stdio transition tests.
5. Replace extension lifecycle ownership with `EditorSession` and `ServerProcess`, then bind
   inspector and diagnostics through the stable generation.
6. Run pressure and Extension Development Host verification against the original reproduction.
7. Delete `DocumentUpdates`, the old `ProjectSession`, push publication, exposed concrete-client
   access, and superseded tests before repository verification.

Rollback reverts the whole change. No persisted data or compatibility migration exists.
