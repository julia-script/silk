## MODIFIED Requirements

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
