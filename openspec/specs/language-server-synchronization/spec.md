# language-server-synchronization Specification

## Purpose

Defines how the Silk language server keeps project analysis consistent with editor buffers and
filesystem changes while bounding redundant work and preventing stale protocol results.
## Requirements
### Requirement: Open documents form project-scoped overlays

The language server SHALL group synchronized documents by discovered Silk project, SHALL resolve an
open document's bytes before the corresponding on-disk file, and SHALL isolate overlays belonging
to different projects. A document outside a project SHALL use a stable isolated workspace identity
that cannot collide with another unrelated document.

#### Scenario: Unsaved imported module

- **WHEN** an open module imports another open module whose editor contents differ from disk
- **THEN** analysis uses the imported module's synchronized editor contents

#### Scenario: Same module spelling in different projects

- **WHEN** two open projects each contain a module with the same canonical name
- **THEN** each project's analysis resolves only its own module and overlay

#### Scenario: Standalone virtual document

- **WHEN** a synchronized document cannot be assigned a canonical filesystem module
- **THEN** the server gives it a stable isolated identity rather than sharing a fallback identity with another document

### Requirement: Analysis scheduling is bounded and latest-wins

The language server SHALL coalesce bursts of changes per project, SHALL run no more than one
analysis job at a time for a project, and SHALL eventually analyze the newest synchronized revision.
Superseded work MUST NOT replace a newer completed document snapshot or publish protocol results
after a newer revision has been accepted.

#### Scenario: Rapid edits during analysis

- **WHEN** several revisions arrive while project analysis is queued or running
- **THEN** the server completes or supersedes the active work, analyzes the newest pending revision next, and does not start one concurrent analysis per revision

#### Scenario: Older analysis finishes last

- **WHEN** analysis for an older revision finishes after the server has accepted a newer revision
- **THEN** the older result neither replaces the current session nor publishes diagnostics

### Requirement: Protocol queries use a coherent document snapshot

The language server SHALL answer a semantic request from a document and analysis snapshot produced
for the same synchronized revision. If that revision has not completed analysis, the request SHALL
wait for its project scheduler to commit the newest applicable snapshot or SHALL return no semantic
result; it MUST NOT interpret a current editor position against an older document's line index or
semantic facts.

#### Scenario: Definition requested during refresh

- **WHEN** a definition request arrives after a document change but before analysis of that change completes
- **THEN** the server answers from analysis of that exact captured revision or returns no location if it is superseded, without consulting an older or newer document snapshot

#### Scenario: Atomic snapshot replacement

- **WHEN** a new project analysis completes
- **THEN** all document text, line indexes, module identities, and semantic facts from that result become queryable together

### Requirement: Diagnostics describe only the latest accepted revision

The language server SHALL publish diagnostics only from the newest accepted analysis for each open
document, SHALL include the analyzed document version when the protocol permits it, and SHALL clear
diagnostics when the document closes. A dependency change SHALL refresh diagnostics for affected
open documents even when their own text did not change.

#### Scenario: Superseded diagnostics

- **WHEN** an edit fixes an error while analysis of the erroneous revision is still running
- **THEN** diagnostics from the erroneous revision are not published after diagnostics for the fixed revision

#### Scenario: Closing a document

- **WHEN** an open document with published diagnostics closes
- **THEN** the server clears its diagnostics and removes its synchronized overlay

#### Scenario: Open dependency changes

- **WHEN** an open imported module changes in a way that invalidates an importing document
- **THEN** the server republishes diagnostics for the affected importing document without requiring an edit to it

### Requirement: Filesystem changes invalidate affected project analysis

The language server SHALL react to relevant changes to closed `.silk` files and `silk.toml`
manifests within an open document's project. It SHALL preserve open-buffer precedence and SHALL
reanalyze only projects that can be affected by the changed path.

#### Scenario: Closed dependency changes on disk

- **WHEN** a closed imported module changes on disk while an importing document remains open
- **THEN** the server refreshes the importing document against the new on-disk contents

#### Scenario: Open file also changes on disk

- **WHEN** a file with unsaved synchronized contents also changes on disk
- **THEN** analysis continues to use the synchronized contents until the document closes or the editor updates them

#### Scenario: Project manifest changes

- **WHEN** a relevant `silk.toml` change alters project or source-root discovery
- **THEN** the server rediscovers affected open documents before publishing subsequent analysis results

### Requirement: Editor analysis executes frontend phases only

The language server SHALL produce each ordinary synchronized-document result from an immutable
frontend analysis snapshot. Diagnostics, hover, completion, navigation, symbols, inlay hints, and
formatting SHALL NOT require instance discovery, target selection, layout planning, MIR lowering,
evaluation, or code generation. This computation choice SHALL NOT weaken version matching, atomic
project revision commit, recovery, or compiler ownership of semantic facts.

#### Scenario: Serve editor features from frontend analysis

- **WHEN** an accepted document revision completes analysis and receives hover, definition, completion, symbol, inlay-hint, formatting, and diagnostic requests
- **THEN** every request uses the committed frontend snapshot for that exact document version and no runtime realization phase executes

#### Scenario: Preserve coherent replacement

- **WHEN** a newer frontend snapshot atomically replaces the committed project revision
- **THEN** protocol requests observe its document text and semantic facts together under the existing latest-wins scheduling rules

### Requirement: One accepted project revision uses one shared analysis

For each accepted synchronized project revision, the language server SHALL invoke one project
frontend analysis over the complete captured open-document set and SHALL derive every analyzed
document result from that one immutable project revision. The amount of compiler frontend work
within a revision SHALL scale with the union module closure rather than with open roots multiplied
by their individual closures.

#### Scenario: Open documents share dependencies

- **WHEN** multiple synchronized documents in one project reach overlapping dependency closures
- **THEN** the accepted revision analyzes each shared module once and commits document results backed by the same project analysis

#### Scenario: Supersede shared project work

- **WHEN** a newer synchronized revision arrives while shared project analysis is queued or running
- **THEN** the older result cannot publish or replace the newer project revision under the existing latest-wins rules

### Requirement: Shared analysis preserves atomic document results

The language server SHALL commit the complete analyzed-document map only after the shared project
analysis and every root view are complete. A protocol request SHALL continue to observe document
bytes, line indexes, module identity, URI mappings, and semantic facts from one exact accepted
revision.

#### Scenario: Commit several root views

- **WHEN** shared analysis completes for several synchronized documents
- **THEN** all analyzed-document results become queryable together and no request can observe a partially replaced project map

### Requirement: Revision reuse begins from the last atomic commit

The project scheduler SHALL supply the last complete committed analyzed-document map to a new
project analysis callback. The first analysis SHALL receive an empty prior map. A superseded,
failed, interrupted, or still-running analysis MUST NOT become the reuse basis for later work.

#### Scenario: Analyze after one accepted edit

- **WHEN** revision N commits and revision N+1 begins
- **THEN** the N+1 analysis callback receives revision N's complete committed map as its prior value

#### Scenario: Supersede an active revision

- **WHEN** revision N+1 becomes stale while N+2 is queued
- **THEN** N+2 receives the last committed map from revision N rather than any result produced for N+1

### Requirement: Shared workspace results retain their project analysis

Every analyzed-document view produced by one workspace analysis SHALL reference the same completed
project analysis value. Workspace revision analysis SHALL derive its syntax reuse only from the
prior committed value for that workspace.

#### Scenario: Commit several revised root views

- **WHEN** a multi-root workspace revision completes using a prior committed project
- **THEN** every document result references the same new project analysis and its own current root view
