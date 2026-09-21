# Spec Delta

## Purpose

Defines safe cross-revision semantic query reuse through reconstructible addresses, owned result
projections, ordered dependency validation, and current-revision presentation.

## ADDED Requirements

### Requirement: Semantic queries have reconstructible typed identities

Every reusable semantic query SHALL have a closed typed descriptor whose canonical address contains
its family and schema plus stable authored owner, logical scope, and result-affecting parameters.
Addresses MUST NOT contain source positions, traversal ordinals, process epochs, current content
hashes, object identities, or complete binding lists. One authoritative provider SHALL dispatch each
descriptor family and reusable answers SHALL contain only immutable owned projections.

#### Scenario: Reconstruct a header query in another revision

- **WHEN** another revision requests the public signature of the same stable declaration under the same semantic context
- **THEN** it reconstructs the same typed address without retaining the prior request closure, declaration object, index, or session

#### Scenario: Keep presentation out of identity

- **WHEN** a use site moves without changing the semantic request
- **THEN** the canonical query address remains equal and current diagnostic presentation uses the moved site

### Requirement: Previous records validate into one bounded current snapshot

A session SHALL own distinct immutable previous and current snapshots and SHALL retain no chain of
older sessions. On demand it SHALL validate a previous record's dependencies in recorded order,
stopping at the first changed or unresolvable read and executing the current provider without
demanding obsolete later branches. Validated or executed completed records SHALL enter the current
snapshot with their current dependency set. Missing records MUST NOT imply unchanged input.

#### Scenario: Reuse after an unrelated edit

- **WHEN** every recorded dependency of a previous query has the same current fingerprint
- **THEN** the previous result is admitted to the current snapshot without executing its provider

#### Scenario: Replace an obsolete dependency branch

- **WHEN** the first changed dependency makes the provider take a different branch
- **THEN** validation stops at that dependency, the provider executes, and the current record contains only dependencies observed on the new branch

#### Scenario: Retry after an aborted request

- **WHEN** execution is interrupted or defects before publication
- **THEN** no completed current record remains and a later request can reserve and execute the same address

### Requirement: Result fingerprints stop downstream execution

Every completed record SHALL contain a canonical result projection and fingerprint. When dependency
validation requires provider execution but the current result projection equals the previous one,
dependents SHALL treat that query read as unchanged and MAY reuse. A changed semantic or diagnostic
projection SHALL invalidate dependents that observe it. Cache hits SHALL still record the caller's
query read, and structural counters SHALL distinguish validation, provider execution, and reuse.

#### Scenario: Relevant input changes without changing the answer

- **WHEN** a query executes because one input fingerprint changed but produces the same canonical result projection
- **THEN** a dependent that observed the query result reuses without executing

#### Scenario: Diagnostic content changes independently

- **WHEN** semantic content remains equal but an observed definition-owned diagnostic projection changes
- **THEN** diagnostic consumers recompute while semantic-only consumers may reuse

### Requirement: Leaf observations have current readers

Each observation SHALL be either a typed query read with its expected projection fingerprint or a
typed leaf input read with its expected fingerprint. Current readers SHALL cover completed
header/alias/bound components, exact namespace membership including absence and conflicts, imported
binding selection, conformance candidate sets, and relevant configuration or target facets.
Synthetic query keys without providers are forbidden.

#### Scenario: A missing member becomes available

- **WHEN** a previous qualified lookup observed absence and the current namespace gains that member
- **THEN** the namespace leaf fingerprint changes and the lookup provider executes

#### Scenario: A candidate set changes

- **WHEN** an associated or conformance candidate is added or removed
- **THEN** the exact candidate-set input invalidates affected readers even if unrelated declarations remain unchanged

### Requirement: Active cycles never validate unknown data

Current eager declaration completion, alias handling, and legal mutually recursive signature
components SHALL remain authoritative. A completed current header component MAY be one atomic input
whose fingerprint includes stable membership and observable semantic and diagnostic content.
Component merge or split SHALL invalidate affected readers, and an unresolved active query or
component MUST NOT be admitted as unchanged.

#### Scenario: Preserve legal mutual signatures

- **WHEN** mutually recursive signatures complete as one legal component in consecutive revisions
- **THEN** readers validate the completed component without treating active work as reusable

#### Scenario: Invalidate a component split

- **WHEN** edits split or merge a completed header component
- **THEN** the component membership fingerprint changes and affected readers execute

### Requirement: Forced-fresh mode bypasses every reuse layer

A forced-fresh request SHALL use the same providers and current presentation paths while bypassing
root and nested previous/current reuse. Its semantic results and diagnostic codes/current spans
SHALL agree with an ordinary validated request.

#### Scenario: Compare validated and fresh analysis

- **WHEN** the same current revision is analyzed once with validation enabled and once forced fresh
- **THEN** both analyses publish equal semantic projections and equal diagnostic codes at current spans
