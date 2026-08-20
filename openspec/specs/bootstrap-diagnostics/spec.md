# bootstrap-diagnostics Specification

## Purpose
One structured diagnostic model that every compiler phase publishes into, with error sentinels
that preserve provenance and a single deterministic ordering authority, so tools and humans
consume the same diagnostic data regardless of which phase produced it.
## Requirements
### Requirement: Unified diagnostic model
Every diagnostic from any compiler phase SHALL be one `Diagnostic` value carrying a stable code,
a severity, a concise message, exactly one primary source-owned span, and its structured reason
data where the originating phase defines reasons for that code. A diagnostic MAY additionally
carry labeled related spans, notes, and unambiguous machine-applicable edits. Every diagnostic
SHALL identify its originating phase and, where one exists, its originating semantic entity. A
diagnostic caused by another diagnostic SHALL carry that diagnostic's identity as its cause.
The unary-only deeper-under-application code `SEM0079` SHALL be retired. Zero-argument and
over-arity calls SHALL use the ordinary arity diagnostic, while valid non-empty trailing sections
SHALL produce no arity diagnostic.

#### Scenario: Every phase produces the same shape
- **WHEN** one source produces lexical, parser, and semantic mistakes in a single compilation
- **THEN** every returned diagnostic exposes the same model — stable code, severity, message, primary span, and originating phase — regardless of which phase produced it

#### Scenario: Cascades name their cause
- **WHEN** an unresolved name makes a dependent fact unavailable and that unavailability produces a further diagnostic
- **THEN** the dependent diagnostic carries the originating diagnostic's identity as its cause

#### Scenario: Duplicate names surface their original as a related span
- **WHEN** a declaration or parameter name repeats a present earlier occurrence
- **THEN** the duplicate's diagnostic carries the original occurrence's span as a labeled related span in addition to its structured reason data

#### Scenario: Diagnose only an invalid remaining arity

- **WHEN** a multi-parameter callable receives zero arguments or more arguments than remain
- **THEN** analysis reports the ordinary arity code and never `SEM0079`

### Requirement: Error sentinels preserve provenance
Unavailable, missing, ambiguous, and damaged states in phase results SHALL retain the identity of
the diagnostic that originated them, so dependent cascades can be suppressed or attached to the
primary error rather than duplicated. A write destination that is unavailable because its name or
syntax is unresolved MUST NOT additionally be diagnosed as a resolved-but-non-writable place.

#### Scenario: Suppress a dependent cascade
- **WHEN** a fact is unavailable because of an earlier diagnostic and a consumer would report the same underlying mistake again
- **THEN** the consumer can identify the originating diagnostic from the sentinel and no duplicate diagnostic is emitted for the same cause

#### Scenario: Suppress invalid-place after unknown name
- **WHEN** an assignment destination is unavailable because its root name is unknown
- **THEN** the unknown-value diagnostic stands alone and no invalid-assignment-place diagnostic is emitted

### Requirement: Recovery diagnostics represent independent source mistakes
The parser SHALL retain every missing or unexpected CST element needed for lossless recovery while
reporting one primary diagnostic for each independently actionable source mistake. Synthetic
elements introduced only because a larger construct is absent MUST NOT each become equal-weight
diagnostics.

#### Scenario: Aggregate recovered return structure
- **WHEN** recovery inserts both the keyword and expression leaves of one wholly absent return statement
- **THEN** one parser diagnostic identifies the missing statement while both leaves remain queryable in the CST

#### Scenario: Suppress an incomplete declaration cascade
- **WHEN** source ends after the declaration prefix `pub`
- **THEN** one parser diagnostic identifies the missing `fn` token and no dependent diagnostic is emitted for the remaining synthesized function structure

#### Scenario: Resume after synchronization
- **WHEN** recovery reports one syntax mistake and later consumes a concrete token expected by the grammar
- **THEN** recovery ends and a subsequent independent syntax mistake can produce its own diagnostic

#### Scenario: Exclude indentation from recovered ranges
- **WHEN** an indented bare identifier is recovered as the expression after a missing `return`
- **THEN** the missing-keyword diagnostic has an empty span at the identifier boundary, the unknown-value diagnostic covers only the identifier, and neither range includes leading trivia

### Requirement: Expected tokens use source-language descriptions
Missing-token diagnostic messages SHALL describe expected tokens using their Silk source spelling
or source-language role rather than compiler-internal token-kind identifiers. Structured reason
data SHALL retain the stable token kind for machine consumers.

#### Scenario: Describe keywords and punctuation
- **WHEN** recovery expects `ReturnKeyword` or `Equals`
- **THEN** the user-facing messages name `` `return` `` or `` `=` `` while the structured reasons retain `ReturnKeyword` or `Equals`

### Requirement: Unexpected syntax diagnostics identify token and context

An unexpected-syntax diagnostic SHALL identify the encountered source token or bounded construct
and the grammatical context in which it was rejected. Its structured reason SHALL retain the
unexpected token kinds and the expected source-language token spellings or grammatical roles.
Generic wording such as `Unexpected token sequence` without encountered or expected context MUST
NOT be the sole user-facing explanation when the parser can determine that context.

#### Scenario: Describe an unexpected block token

- **WHEN** punctuation that cannot begin any statement appears directly inside a block
- **THEN** the diagnostic names that punctuation, says it was encountered while parsing a statement, and describes the valid statement starts or closing brace

#### Scenario: Describe a bounded malformed construct

- **WHEN** recovery groups multiple concrete tokens into one error region while parsing a known construct
- **THEN** one diagnostic identifies the bounded construct or its first decisive token and retains every unexpected token kind in structured reason data

### Requirement: Statement recovery remains inside its owning block

When malformed syntax begins where a statement is expected, recovery SHALL retain one error or
unavailable statement branch in the current block, synchronize at the next valid statement or the
current block's closing brace, and continue parsing that block. Tokens following the malformed
statement MUST NOT be reinterpreted as a top-level declaration solely because of that recovery.
Missing tokens synthesized only by the primary recovery SHALL not produce independent diagnostics.

#### Scenario: Keep return after a damaged run expression

- **WHEN** a malformed standalone run expression is followed by a valid `return ()` in the same block
- **THEN** one primary diagnostic describes the malformed run expression, the return remains a sibling statement in that block, and no phantom function or missing-brace cascade is reported

#### Scenario: Recover unexpected punctuation before a statement

- **WHEN** unexpected punctuation appears before a valid binding or return statement
- **THEN** the punctuation is retained in one block-owned recovery branch and the following statement parses normally without a dependent declaration diagnostic

#### Scenario: End recovery at the owning right brace

- **WHEN** a malformed final statement reaches its block's concrete closing brace
- **THEN** recovery retains that brace as the block delimiter and does not consume it into the malformed statement

### Requirement: Deterministic cross-phase ordering

Phases SHALL return diagnostics as data and SHALL NOT print them. One driver-side ordering
authority SHALL sort all diagnostics of a compilation by canonical module identity, primary span,
stable code, and a stable tie-breaker. The module-closure phase SHALL participate in the unified
model and ordering as its own originating phase alongside the lexer, parser, and semantic
analysis. Repeated compilations of identical input SHALL produce identical diagnostic sequences.

#### Scenario: Cross-phase merge is stable

- **WHEN** equivalent malformed sources are compiled repeatedly in fresh processes
- **THEN** the fully merged diagnostic sequence — across every phase — is identical in content and order on every run

#### Scenario: Phases never print

- **WHEN** any phase encounters any source mistake
- **THEN** the mistake is returned as diagnostic data and no phase writes diagnostic text to any output stream

#### Scenario: Module diagnostics carry their phase

- **WHEN** closure loading produces any module-phase diagnostic
- **THEN** the diagnostic is a unified `Diagnostic` value identifying the module-closure phase as its originating phase and merges into the same driver order

### Requirement: Ownership diagnostics join the unified model

The ownership phase SHALL publish its diagnostics as unified `Diagnostic` values identifying the
ownership phase as their originating phase, with stable `OWN`-prefixed codes, structured reason
data, and source-owned primary spans. Ownership diagnostics SHALL merge into the single
driver-side ordering after the semantic phase's rank and SHALL follow the same determinism,
never-print, and sentinel-provenance rules as every other phase.

#### Scenario: Ownership diagnostics carry their phase

- **WHEN** the ownership phase diagnoses a consumed binding used again
- **THEN** the diagnostic is a unified `Diagnostic` value identifying the ownership phase, and merging it with other phases' collections yields one deterministic sequence

#### Scenario: Ownership sentinels preserve provenance

- **WHEN** an ownership violation makes a function's verdict unsatisfied
- **THEN** the verdict retains the originating ownership diagnostic's identity so consumers can attach to the primary error rather than duplicate it

### Requirement: Target-dependent usize diagnostics retain exact values

The compiler SHALL report an error before MIR lowering when a reachable contextual `usize` literal
exceeds the selected target's unsigned range. The diagnostic SHALL retain the exact source magnitude,
selected target identity, and supported bit width and SHALL sort deterministically after
target-independent semantic diagnostics. It MUST NOT report a rounded or truncated value.

#### Scenario: Diagnose a native-sized literal on Wasm

- **WHEN** a literal of `4294967296` has contextual type `usize` and the selected target is `wasm32-unknown-unknown`
- **THEN** one diagnostic names the exact magnitude, Wasm target, and 32-bit limit before MIR lowering

### Requirement: Row-contract diagnostics are stable structured data

The generated diagnostic catalog SHALL contain distinct identities and structured payloads for row
kind mismatch, invalid singleton member, exact-access mismatch, checked absence, underconstrained
row computation, provider no-match, joint provider-selection conflict, provider ambiguity,
selected-row cardinality, conformance ambiguity, invalid conformance, cyclic substitution,
and non-concrete specialization.

Provider conflict and ambiguity payloads SHALL be span-free semantic data containing canonical
constraint/member keys and candidate sets. Primary and ordered secondary source origins SHALL live
in a separate diagnostic-location record and SHALL NOT affect diagnostic identity, payload equality,
or source/intrinsic parity. Precedence SHALL be syntax/kind, structural inference/underconstraint,
checked constraint failure, specialization non-concreteness, then the existing
run-boundary `SEM0071` for an already concrete Effect.

#### Scenario: Report provider ambiguity deterministically

- **WHEN** unequal provider relation maps retain more than one common candidate
- **THEN** one diagnostic carries the common candidate list and every full relation candidate set, with selector/application primary and ordered relation secondary locations stored separately

#### Scenario: Keep equivalent call payloads equal

- **WHEN** equivalent source and intrinsic contracts fail at different source locations
- **THEN** their diagnostic identity and semantic payload are equal while their diagnostic-location records remain local

#### Scenario: Preserve run-boundary responsibility

- **WHEN** a concrete Effect reaches `run` with unsatisfied requirements
- **THEN** `SEM0071` remains the run-boundary diagnostic and is not reused for row inference or selection failures
