## Purpose

One structured diagnostic model that every compiler phase publishes into, with error sentinels
that preserve provenance and a single deterministic ordering authority, so tools and humans
consume the same diagnostic data regardless of which phase produced it.

## ADDED Requirements

### Requirement: Unified diagnostic model

Every diagnostic from any compiler phase SHALL be one `Diagnostic` value carrying a stable code,
a severity, a concise message, exactly one primary source-owned span, and its structured reason
data where the originating phase defines reasons for that code. A diagnostic MAY additionally
carry labeled related spans, notes, and unambiguous machine-applicable edits. Every diagnostic
SHALL identify its originating phase and, where one exists, its originating semantic entity. A
diagnostic caused by another diagnostic SHALL carry that diagnostic's identity as its cause.

#### Scenario: Every phase produces the same shape

- **WHEN** one source produces lexical, parser, and semantic mistakes in a single compilation
- **THEN** every returned diagnostic exposes the same model — stable code, severity, message, primary span, and originating phase — regardless of which phase produced it

#### Scenario: Cascades name their cause

- **WHEN** an unresolved name makes a dependent fact unavailable and that unavailability produces a further diagnostic
- **THEN** the dependent diagnostic carries the originating diagnostic's identity as its cause

#### Scenario: Duplicate names surface their original as a related span

- **WHEN** a declaration or parameter name repeats a present earlier occurrence
- **THEN** the duplicate's diagnostic carries the original occurrence's span as a labeled related span in addition to its structured reason data

### Requirement: Error sentinels preserve provenance

Unavailable, missing, ambiguous, and damaged states in phase results SHALL retain the identity of
the diagnostic that originated them, so dependent cascades can be suppressed or attached to the
primary error rather than duplicated.

#### Scenario: Suppress a dependent cascade

- **WHEN** a fact is unavailable because of an earlier diagnostic and a consumer would report the same underlying mistake again
- **THEN** the consumer can identify the originating diagnostic from the sentinel and no duplicate diagnostic is emitted for the same cause

### Requirement: Deterministic cross-phase ordering

Phases SHALL return diagnostics as data and SHALL NOT print them. One driver-side ordering
authority SHALL sort all diagnostics of a compilation by canonical module identity, primary span,
stable code, and a stable tie-breaker. Repeated compilations of identical input SHALL produce
identical diagnostic sequences.

#### Scenario: Cross-phase merge is stable

- **WHEN** equivalent malformed sources are compiled repeatedly in fresh processes
- **THEN** the fully merged diagnostic sequence — across every phase — is identical in content and order on every run

#### Scenario: Phases never print

- **WHEN** any phase encounters any source mistake
- **THEN** the mistake is returned as diagnostic data and no phase writes diagnostic text to any output stream
