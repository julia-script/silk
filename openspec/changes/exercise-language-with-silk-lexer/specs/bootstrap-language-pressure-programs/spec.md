## Purpose

Define how complete, recognizable Silk programs pressure the language with differential,
cross-engine evidence without being mistaken for a commitment to self-hosting or replacement of a
canonical implementation.

## ADDED Requirements

### Requirement: A real Silk lexer exercises ordinary language and library features

The repository SHALL contain a readable lexer written in ordinary Silk source that consumes a
runtime-sized borrowed byte slice and returns owned token and lexical-diagnostic data. It SHALL use
the public allocation and growable-sequence surface, and compiler phases and backends MUST NOT gain
lexer-specific or token-specific operations, layouts, or branches.

#### Scenario: Runtime-sized input produces owned results

- **WHEN** the Silk lexer receives borrowed source bytes whose length is known only at runtime
- **THEN** it returns owned token records with byte spans and owned diagnostics that remain valid independently of the input borrow

#### Scenario: Pressure program remains ordinary Silk

- **WHEN** its published source, MIR, evaluation trace, or backend artifact is inspected
- **THEN** only general language, allocation, collection, control-flow, and cleanup mechanisms are present

### Requirement: The Silk lexer is checked against the canonical lexer

The Silk lexer SHALL be differentially checked against the TypeScript lexer, which remains the
canonical implementation. The corpus SHALL cover whitespace, comments, identifiers, every current
keyword, decimal integer and float forms, text and byte-string literals with escapes, every current
single and compound punctuation token, end of file, and unsupported byte runs.

#### Scenario: Valid source agrees token by token

- **WHEN** representative valid Silk source is lexed by both implementations
- **THEN** the ordered token kinds and half-open byte spans, including trivia and end of file, are identical

#### Scenario: Invalid source agrees on diagnostics

- **WHEN** source contains one or more unsupported byte runs
- **THEN** both implementations produce identical invalid-token spans and lexical-diagnostic spans while continuing with later supported tokens

### Requirement: Execution and ownership evidence is cross-engine and deterministic

Representative valid and invalid lexer cases SHALL agree across evaluation, native LLVM, and
direct WebAssembly execution. Allocation failure at every exercised growth ordinal SHALL preserve
typed `OutOfMemory`, release every acquired allocation exactly once, and leave subsequent runs
deterministic.

#### Scenario: Engines agree on a representative valid case

- **WHEN** the valid acceptance case is evaluated, compiled and run natively, and instantiated as WebAssembly
- **THEN** every engine reports the same deterministic lexer fingerprint and successful cleanup

#### Scenario: Engines agree on a representative invalid case

- **WHEN** the invalid acceptance case runs on all three engines
- **THEN** every engine reports the same deterministic token-and-diagnostic fingerprint and successful cleanup

#### Scenario: Allocation failure rolls back cleanly

- **WHEN** allocation is rejected at any token or diagnostic vector growth ordinal exercised by the acceptance cases
- **THEN** the typed failure is preserved and every earlier acquisition is released exactly once without double-dropping initialized records

### Requirement: Pressure findings determine follow-up work

The change SHALL retain a checked-in findings report that classifies observed walls as language,
standard-library, compiler-defect, tooling/ergonomics, or performance/cost findings. Each finding
SHALL cite evidence and state whether it was repaired, deferred to a focused proposal, or accepted
as local complexity. Completing the lexer MUST NOT automatically schedule a parser port or replace
the TypeScript compiler path.

#### Scenario: A wall is encountered during implementation

- **WHEN** completing the lexer requires awkward source, a missing general operation, a compiler repair, or a material cost
- **THEN** the report records its category, concrete evidence, disposition, and the smallest plausible general follow-up

#### Scenario: The lexer exercise completes

- **WHEN** all lexer acceptance gates pass
- **THEN** the next pressure program or repair is selected from the recorded evidence rather than from a predetermined self-hosting module order
