# Spec Delta

## Purpose

Defines fresh-run typed semantic query sessions that memoize complete answers, record the semantic
inputs observed by each request, preserve diagnostics, and isolate answers between prepared
selection epochs without introducing a persistent incremental engine.

## ADDED Requirements

### Requirement: A session owns one immutable semantic environment

A semantic query session SHALL bind one immutable prepared module closure and selection epoch, its
canonical declaration identities and scopes, normalized profile and configuration facts, and the
semantic context required by its providers. Request identity SHALL include the query kind, stable
subject/scope/goal, and canonical substitutions or evidence that can affect the answer; caller
positions and presentation SHALL NOT participate in semantic identity.

#### Scenario: Distinct selection epochs do not alias

- **WHEN** a selection decision exposes a declaration inside an already loaded module without changing the source-file set
- **THEN** the new environment uses a distinct session and cannot reuse a prior missing or conflicting answer

#### Scenario: Presentation movement preserves semantic identity

- **WHEN** only a caller position or current presentation mapping changes
- **THEN** an otherwise identical request retains its semantic identity and its diagnostics can be presented at current anchors

### Requirement: Each request family has one authoritative provider

Each migrated semantic request family SHALL have one provider used by both memoized and forced-fresh
execution. Repeating an identical request in one session SHALL execute its provider at most once,
while a forced-fresh request SHALL execute that same provider without consulting the completed
answer store and SHALL produce an equivalent answer and diagnostics.

#### Scenario: Repeated request executes once

- **WHEN** two callers request the same declared type or name answer in one session
- **THEN** the provider executes once and both callers receive the same immutable completed answer

#### Scenario: Forced-fresh agrees

- **WHEN** a completed request is executed through the forced-fresh path
- **THEN** the fresh answer and anchored diagnostics are equivalent to the memoized answer

### Requirement: Query dependencies are observed during execution

A query SHALL record every result-affecting semantic read made while its provider executes,
including positive and negative namespace membership, examined candidate sets, selected import
bindings, declaration headers, aliases, bounds, conformance answers, and relevant profile or
configuration facts. Serving a completed answer SHALL still record a dependency edge from the
current caller to the completed request.

#### Scenario: Missing lookup records absence

- **WHEN** name resolution examines a namespace and finds no matching member
- **THEN** the completed answer records the examined namespace and missing membership as dependencies

#### Scenario: Memo hit records caller edge

- **WHEN** one query reads another query whose answer is already memoized
- **THEN** the caller's dependency observations include the completed callee request

### Requirement: Only complete outcomes are published

A provider SHALL publish an immutable answer only after successful completion, including completed
semantic rejection with anchored diagnostics. Interruption, cancellation, or defect SHALL release
the active reservation, publish no answer, and permit the same request to be retried.

#### Scenario: Cancellation permits retry

- **WHEN** a query is canceled at a supported query or yield boundary before publication
- **THEN** no completed or active reservation remains and a later identical request can execute successfully

#### Scenario: Semantic rejection is reusable answer data

- **WHEN** a provider completes with an unavailable or rejected semantic result and anchored diagnostics
- **THEN** the immutable rejection is published as the request's completed answer

### Requirement: Recursion distinguishes available headers from prohibited cycles

The query boundary SHALL make complete declared signatures available without demanding checked
bodies, SHALL preserve finite recursive nominal handling, and SHALL report prohibited alias or
inline-declaration cycles with their explicit dependency path rather than fabricating a provisional
answer.

#### Scenario: Mutual function headers resolve

- **WHEN** two functions in different modules legally call each other using complete declared signatures
- **THEN** both public types resolve without either request demanding the other's checked body

#### Scenario: Inline representation cycle is rejected

- **WHEN** declared aliases or inline nominal fields form a prohibited representation cycle
- **THEN** the result is unavailable with diagnostics that identify the dependency path

### Requirement: Sealed sessions cannot discover source

Final semantic sealing SHALL retain the session and its immutable semantic inputs while removing
source resolver, parser, and source-syntax capabilities. A visible change to declarations,
bindings, candidate sets, selection, or profile facts SHALL require a fresh session rather than
transferring unvalidated answers.

#### Scenario: Sealed header reads remain available

- **WHEN** preparation discards its source resolver and parser after sealing
- **THEN** declared-type and name queries needed by downstream semantic work still execute from the sealed session

#### Scenario: Changed candidate set starts fresh

- **WHEN** the examined conformance or namespace candidate set changes
- **THEN** the new preparation epoch cannot inherit the earlier session's completed answers
