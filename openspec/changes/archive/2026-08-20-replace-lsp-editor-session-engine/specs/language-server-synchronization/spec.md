## ADDED Requirements

### Requirement: Synchronized source acceptance precedes project work

The language server SHALL accept each monotonically newer open-document version into one
authoritative source ledger before performing project discovery, filesystem access, analysis, or
observer delivery. Acceptance of version N MUST NOT wait for processing of an obsolete version,
and pending work SHALL retain only the newest required document state and project invalidation.
Closing a document SHALL terminally remove its ledger entry and settle work that names it.
When discovery or manifest invalidation reassigns a document to another project identity, the
server SHALL terminally supersede the old association, settle every query and diagnostic pull bound
to it, and create the new association from the document's latest accepted ledger entry.

#### Scenario: Character-by-character repair during cold analysis

- **WHEN** a document receives several character edits before its first project analysis completes
- **THEN** each notification is accepted without waiting for earlier discovery or analysis and only the newest required state remains pending

#### Scenario: Sustained typing before project discovery

- **WHEN** edits arrive faster than project discovery can classify the document
- **THEN** discovery uses the newest accepted bytes and does not perform one complete discovery operation per obsolete version

#### Scenario: Non-monotonic document version

- **WHEN** a document notification names a version no newer than the ledger's accepted version
- **THEN** the server rejects or disregards that notification without replacing the authoritative bytes

#### Scenario: Request immediately follows change notification

- **WHEN** a version-N change notification is followed immediately by a semantic request before background discovery or analysis advances
- **THEN** the request captures version N from the authoritative ledger rather than the previously accepted version

#### Scenario: Provisional project becomes manifest-backed

- **WHEN** project discovery reassigns an open document while a query and diagnostic pull are waiting on its provisional project
- **THEN** the provisional generation is superseded, both operations settle, and subsequent requests bind to the manifest-backed project using the latest accepted bytes

#### Scenario: Manifest change reassigns a document

- **WHEN** a manifest or source-root change moves an open document to a different project identity
- **THEN** work bound to the old association settles as superseded and the new project analyzes the latest ledger entry without requiring another edit

### Requirement: Analysis failure is isolated from the protocol session

Compiler analysis and semantic query execution SHALL be isolated so non-cooperative computation,
defect, or resource failure in one project cannot prevent the protocol session from accepting
edits, cancellation, restart, shutdown, or requests for another project. The server SHALL detect a
project analysis that exceeds its bounded health policy, terminally settle its generation, retire
its execution context, and recreate analysis from the latest accepted source ledger without
requiring an editor-window reload.

#### Scenario: Analysis stops making progress

- **WHEN** one project analysis stops responding to interruption or health checks
- **THEN** the protocol session remains responsive, terminally settles requests for that generation, retires the analysis context, and schedules the latest accepted project state on a replacement

#### Scenario: Healthy project beside a wedged project

- **WHEN** one project is wedged and a semantic request targets another committed project
- **THEN** the server answers the healthy project without waiting for the wedged analysis context

#### Scenario: Replacement loses incremental state

- **WHEN** a failed analysis context is replaced and its in-memory incremental state is unavailable
- **THEN** the replacement reconstructs analysis from the latest accepted source ledger and filesystem inputs rather than requiring another document edit

## MODIFIED Requirements

### Requirement: Analysis scheduling is bounded and latest-wins

The language server SHALL coalesce bursts of accepted source changes at the trailing edge per
project, SHALL run no more than one analysis generation at a time for a project, and SHALL retain at
most one latest pending generation. Every generation SHALL have an explicit `Pending`, `Committed`,
`Failed`, `Superseded`, or `Closed` terminal status; the absence of a committed document view MUST
NOT be interpreted as proof that analysis is still pending. Superseded work MUST NOT replace or
publish over a newer generation. Successful completion, typed failure, defect, interruption,
worker retirement, and close SHALL all settle affected requests and allow the newest pending
generation to run. Once source and filesystem input quiesce and the project has a healthy analysis
context, the newest accepted generation SHALL eventually commit and current semantic and diagnostic
requests SHALL become ready. Only a complete atomic commit MAY become the reuse basis for later
work.

#### Scenario: Rapid edits during analysis

- **WHEN** several revisions arrive while project analysis is queued or running
- **THEN** the server supersedes obsolete queued work, preempts or retires obsolete active work, analyzes the newest pending revision next, and does not start or retain one job per revision

#### Scenario: Older analysis finishes last

- **WHEN** analysis for an older generation finishes after the server has accepted a newer generation
- **THEN** the older result neither replaces the current session nor emits current protocol results

#### Scenario: Valid revision follows an analysis defect

- **WHEN** generation N terminates with an internal defect and valid generation N+1 is accepted
- **THEN** the scheduler records N as failed, analyzes and commits N+1, and remains able to process subsequent generations

#### Scenario: Failed work is not reused

- **WHEN** one generation fails or is interrupted after producing partial frontend work
- **THEN** the next analysis starts from the last complete atomic commit rather than the partial failed generation

#### Scenario: Request starts after terminal failure

- **WHEN** a semantic request names the current generation after that generation has already failed or committed without a view for the document
- **THEN** the request returns an explicit unavailable outcome immediately rather than registering a waiter

#### Scenario: Healthy project commits after typing stops

- **WHEN** edits quiesce with a newest pending generation and its analysis context remains healthy
- **THEN** that generation eventually commits and a current semantic request and diagnostic pull answer from it rather than timing out indefinitely

### Requirement: Protocol queries use a coherent document snapshot

The language server SHALL bind every semantic request to the exact accepted document version,
project generation, and analysis-context generation that were current when the request was
accepted. A current pending request MAY wait only within its own project and MUST settle as ready,
superseded, canceled, deadline-exceeded, analysis-failed, closed, or unavailable. Client
cancellation, document supersession, project retirement, and shutdown SHALL interrupt the request
and remove its waiter. A request MUST NOT interpret current editor coordinates against an older or
newer snapshot.

#### Scenario: Definition requested during refresh

- **WHEN** a definition request arrives after a document change but before analysis of that change completes
- **THEN** the server answers from that exact revision or returns a terminal unavailable outcome if it cannot commit, without consulting another snapshot

#### Scenario: Atomic snapshot replacement

- **WHEN** a new project analysis completes
- **THEN** all document text, line indexes, module identities, and semantic facts from that result become queryable together

#### Scenario: Request waits on a defective revision

- **WHEN** a semantic request is waiting for a revision whose analysis defects
- **THEN** the request settles as analysis-failed rather than remaining pending indefinitely

#### Scenario: Client cancels hover

- **WHEN** the editor cancels a hover while its exact revision is pending
- **THEN** the server interrupts that query, removes its waiter, and returns no later hover result for it

#### Scenario: Query deadline expires

- **WHEN** a semantic request cannot become ready within its configured deadline
- **THEN** it settles as deadline-exceeded without changing or canceling the underlying project revision

#### Scenario: Unrelated project is analyzing

- **WHEN** a semantic request targets one project while another project has queued or active source updates
- **THEN** the request synchronizes only with its target project and is not delayed by the unrelated work

### Requirement: Diagnostics describe only the latest accepted revision

The language server SHALL provide document diagnostics through the standard pull-diagnostic
protocol for the exact current synchronized document and project generation, and SHALL NOT depend
on push-diagnostic version filtering. A diagnostic response SHALL carry a stable result identity;
an unchanged request MAY reuse that identity, while a source or dependency change MUST invalidate
it. The server SHALL request a standard diagnostic refresh when a committed dependency or
filesystem change can alter an open document's diagnostics. Superseded, canceled, failed, closed,
or retired generations MUST NOT produce a current diagnostic result.

#### Scenario: Superseded diagnostics

- **WHEN** an edit fixes an error while a diagnostic request for the erroneous revision is running
- **THEN** the old request is canceled or discarded and only a pull for the current revision can replace the editor's diagnostic collection

#### Scenario: Intermediate spelling is repaired

- **WHEN** typing visits `effec` and then reaches `Effect` before the intermediate diagnostic conversion finishes
- **THEN** diagnostics for `effec` are not applied to or retained as diagnostics for the `Effect` document

#### Scenario: Closing a document

- **WHEN** an open document with diagnostics closes
- **THEN** the diagnostic result is invalidated and the synchronized overlay is removed

#### Scenario: Open dependency changes

- **WHEN** an open imported module changes in a way that invalidates an importing document
- **THEN** the server requests diagnostic refresh and the next pull for the importer uses the new committed project generation

#### Scenario: Current analysis defects

- **WHEN** the current document generation cannot produce a snapshot because analysis defects
- **THEN** its diagnostic pull returns an explicit current unavailable/full-empty result and older findings are not presented as current

#### Scenario: Diagnostics recover on the next edit

- **WHEN** a valid revision follows one whose analysis failed
- **THEN** the next diagnostic pull returns its complete current diagnostics without requiring a language-server restart

### Requirement: Protocol handling remains responsive during analysis

The protocol session SHALL remain able to accept document notifications, cancellation, diagnostic
pulls, shutdown, and requests for healthy projects independently of compiler and semantic-query
execution. The amount of obsolete source synchronization and analysis work retained after a newer
revision arrives SHALL be bounded independently of the number of edits already superseded.

#### Scenario: Edit arrives during expensive analysis

- **WHEN** frontend analysis is computing and a newer document revision arrives
- **THEN** the protocol session accepts the revision without waiting for the obsolete analysis phase to yield or complete

#### Scenario: Sustained typing does not build a revision backlog

- **WHEN** edits continue faster than frontend analysis can complete
- **THEN** pending synchronization and analysis remain bounded to the newest required project state rather than growing once per edit

#### Scenario: Another project receives a request

- **WHEN** one project is performing expensive frontend analysis and a request targets another idle project
- **THEN** the protocol session can answer the idle project without waiting for the expensive analysis

#### Scenario: Canceled queries do not accumulate

- **WHEN** the editor repeatedly starts and cancels hover or code-action requests during typing
- **THEN** canceled requests release their waiters and do not increase retained in-flight work over time

### Requirement: Language-server shutdown is bounded

Shutdown SHALL stop accepting new work, terminally settle every source generation and query,
retire every project analysis context, and release protocol resources without waiting indefinitely
for graceful compiler cooperation or previously canceled promises. Each analysis context SHALL be
acknowledged stopped or forcibly retired within the bounded shutdown policy.

#### Scenario: Shutdown after analysis defect

- **WHEN** shutdown begins after a project analysis context terminated with a defect
- **THEN** the server settles that project's requests and completes without waiting for a nonexistent worker

#### Scenario: Shutdown during active analysis

- **WHEN** shutdown begins while one or more projects are analyzing
- **THEN** active contexts are interrupted or forcibly retired and server lifecycle resources are released exactly once

#### Scenario: Shutdown with canceled requests

- **WHEN** canceled editor requests still have unresolved internal work at shutdown
- **THEN** shutdown retires that work without awaiting each obsolete request promise

### Requirement: Shared analysis preserves atomic document results

The language server SHALL atomically commit the complete analyzed-document map only after the
shared project analysis and every requested root view are complete. The committed generation SHALL
become the query and incremental-reuse basis before diagnostic refresh, inspector invalidation, or
other observer delivery begins. Observer delay or failure MUST NOT roll back or block access to the
committed generation. A protocol request SHALL observe document bytes, line indexes, module
identity, URI mappings, and semantic facts from one exact accepted revision.

#### Scenario: Commit several root views

- **WHEN** shared analysis completes for several synchronized documents
- **THEN** all analyzed-document results become queryable together and no request can observe a partially replaced project map

#### Scenario: Diagnostic delivery is blocked

- **WHEN** diagnostic refresh or another observer is delayed after analysis completes
- **THEN** exact-revision semantic queries and later incremental analysis can use the committed snapshot without waiting for that observer
