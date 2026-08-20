## MODIFIED Requirements

### Requirement: Analysis scheduling is bounded and latest-wins

The language server SHALL coalesce bursts of changes at the trailing edge per project, SHALL run no
more than one analysis job at a time for a project, and SHALL eventually analyze the newest
synchronized revision without first completing every superseded revision. Superseded work MUST NOT
replace a newer completed document snapshot or publish protocol results after a newer revision has
been accepted. Successful completion, typed failure, defect, and interruption SHALL all finalize the
active worker state, settle revision waiters that can no longer succeed, and allow the newest pending
revision to run. Only a complete atomic commit MAY become the reuse basis for later work.

#### Scenario: Rapid edits during analysis

- **WHEN** several revisions arrive while project analysis is queued or running
- **THEN** the server supersedes obsolete queued work, preempts or disregards obsolete active work, analyzes the newest pending revision next, and does not start one concurrent analysis per revision

#### Scenario: Older analysis finishes last

- **WHEN** analysis for an older revision finishes after the server has accepted a newer revision
- **THEN** the older result neither replaces the current session nor publishes diagnostics

#### Scenario: Valid revision follows an analysis defect

- **WHEN** analysis of revision N terminates with an internal defect and valid revision N+1 is accepted
- **THEN** the scheduler finalizes revision N, analyzes and publishes revision N+1, and remains able to process subsequent revisions

#### Scenario: Failed work is not reused

- **WHEN** one revision fails or is interrupted after producing partial frontend work
- **THEN** the next analysis starts from the last complete atomic commit rather than the partial failed revision

### Requirement: Protocol queries use a coherent document snapshot

The language server SHALL answer a semantic request from a document and analysis snapshot produced
for the same synchronized revision. If that revision has not completed analysis, the request SHALL
wait only for its own project scheduler to commit the applicable snapshot or SHALL return no
semantic result when that revision is superseded, fails, or is interrupted. Every request waiter
MUST settle when its exact revision can no longer commit, and a request MUST NOT interpret a current
editor position against an older or newer document snapshot.

#### Scenario: Definition requested during refresh

- **WHEN** a definition request arrives after a document change but before analysis of that change completes
- **THEN** the server answers from analysis of that exact captured revision or returns no location if it is superseded, without consulting an older or newer document snapshot

#### Scenario: Atomic snapshot replacement

- **WHEN** a new project analysis completes
- **THEN** all document text, line indexes, module identities, and semantic facts from that result become queryable together

#### Scenario: Request waits on a defective revision

- **WHEN** a semantic request is waiting for a revision whose analysis defects
- **THEN** the request settles without a semantic result rather than remaining pending indefinitely

#### Scenario: Unrelated project is analyzing

- **WHEN** a semantic request targets one project while another project has queued or active document updates
- **THEN** the request synchronizes only with its target project and is not delayed by the unrelated work

### Requirement: Diagnostics describe only the latest accepted revision

The language server SHALL publish diagnostics only from the newest accepted analysis for each open
document, SHALL include the analyzed document version when the protocol permits it, and SHALL clear
diagnostics when the document closes. A dependency change SHALL refresh diagnostics for affected
open documents even when their own text did not change. If the current revision cannot produce an
analysis result, the server SHALL retire diagnostics published for an older document version and
SHALL allow a later valid revision to publish normally.

#### Scenario: Superseded diagnostics

- **WHEN** an edit fixes an error while analysis of the erroneous revision is still running
- **THEN** diagnostics from the erroneous revision are not published after diagnostics for the fixed revision

#### Scenario: Closing a document

- **WHEN** an open document with published diagnostics closes
- **THEN** the server clears its diagnostics and removes its synchronized overlay

#### Scenario: Open dependency changes

- **WHEN** an open imported module changes in a way that invalidates an importing document
- **THEN** the server republishes diagnostics for the affected importing document without requiring an edit to it

#### Scenario: Current analysis defects

- **WHEN** a current document revision cannot produce a snapshot because analysis defects
- **THEN** the server clears diagnostics from the older published version and does not present them as findings for the current text

#### Scenario: Diagnostics recover on the next edit

- **WHEN** a valid revision follows one whose analysis failed
- **THEN** the valid revision publishes its complete current diagnostics without requiring a language-server restart

## ADDED Requirements

### Requirement: Protocol handling remains responsive during analysis

Frontend computation for one project SHALL NOT prevent the server from accepting newer document
notifications, cancellation, shutdown, or requests for unrelated projects. The amount of obsolete
work executed after a newer revision arrives SHALL be bounded independently of the number of edits
already superseded.

#### Scenario: Edit arrives during expensive analysis

- **WHEN** frontend analysis is computing and a newer document revision arrives
- **THEN** the server accepts the revision without waiting for every obsolete analysis revision to complete

#### Scenario: Sustained typing does not build a revision backlog

- **WHEN** edits continue faster than frontend analysis can complete
- **THEN** queued work remains bounded to the newest required project state rather than growing once per edit

#### Scenario: Another project receives a request

- **WHEN** one project is performing expensive frontend analysis and a request targets another idle project
- **THEN** the server can answer the idle project's request without waiting for the expensive analysis

### Requirement: Language-server shutdown is bounded

Shutdown SHALL interrupt or retire every project worker, settle all pending project waiters, and
complete without depending on a worker that has already failed or stopped making progress. No
worker-state marker or pending analysis revision MAY keep shutdown open indefinitely.

#### Scenario: Shutdown after analysis defect

- **WHEN** shutdown begins after a project analysis worker terminated with a defect
- **THEN** the server settles that project's waiters and completes shutdown without waiting for a nonexistent worker

#### Scenario: Shutdown during active analysis

- **WHEN** shutdown begins while one or more projects are analyzing
- **THEN** active analysis is interrupted or retired and server lifecycle resources are released exactly once
