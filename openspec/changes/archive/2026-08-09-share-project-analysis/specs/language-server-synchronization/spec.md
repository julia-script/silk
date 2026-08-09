## ADDED Requirements

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
