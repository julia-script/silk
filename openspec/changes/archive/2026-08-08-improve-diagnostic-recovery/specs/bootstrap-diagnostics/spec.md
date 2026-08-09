## ADDED Requirements

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

## MODIFIED Requirements

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
