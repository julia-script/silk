# bootstrap-language-pressure-programs Specification

## Purpose

Define how complete, recognizable Silk programs pressure the language with differential,
cross-engine evidence without being mistaken for a commitment to self-hosting or replacement of a
canonical implementation.

## Requirements

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

### Requirement: A bounded stack VM exercises execution and owned observations

The repository SHALL contain a readable bounded stack bytecode VM written in ordinary Silk source.
It SHALL consume a runtime-sized borrowed bytecode slice, execute general arithmetic and
control-flow instructions against a fixed-capacity operand stack, and return one owned growable
ordered stream of execution-step and diagnostic events. Compiler phases and backends MUST NOT gain
VM-specific, opcode-specific, or operand-stack-specific operations, layouts, or branches.

#### Scenario: Branching bytecode produces an owned trace

- **WHEN** the Silk VM executes valid bytecode whose branch target and instruction count are known only at runtime
- **THEN** it returns the expected result and an owned ordered step trace that remains valid independently of the input borrow

#### Scenario: Pressure VM remains ordinary Silk

- **WHEN** its published source, MIR, evaluation trace, or backend artifact is inspected
- **THEN** only general language, allocation, collection, control-flow, failure, and cleanup mechanisms are present

### Requirement: The stack VM is checked against a canonical reference

The Silk VM SHALL be differentially checked against a TypeScript reference VM over valid
arithmetic, taken and untaken branches, malformed opcodes and operands, stack underflow and
overflow, invalid jump targets, and bounded nontermination. The comparison SHALL include the
result, ordered executed steps, and ordered diagnostics.

#### Scenario: Valid programs agree step by step

- **WHEN** representative arithmetic and branching bytecode runs in both implementations
- **THEN** the result and every executed instruction observation are identical and ordered

#### Scenario: Malformed programs agree on recovery

- **WHEN** bytecode contains unsupported opcodes or invalid operands
- **THEN** both implementations emit the same ordered diagnostics and make the same continue-or-stop decision

### Requirement: Stack VM resource behavior is cross-engine and deterministic

Representative valid and malformed VM programs SHALL agree across evaluation, native LLVM, and
direct WebAssembly execution. Allocation failure at every exercised trace or diagnostic growth
ordinal SHALL preserve typed `OutOfMemory`, release every acquired allocation exactly once, and
leave subsequent executions deterministic.

#### Scenario: Engines agree on VM fingerprints

- **WHEN** representative valid and malformed programs run on all three engines
- **THEN** every engine reports the same deterministic result, trace-and-diagnostic fingerprint, and cleanup outcome

#### Scenario: VM observation allocation rolls back cleanly

- **WHEN** allocation is rejected at any trace or diagnostic vector growth ordinal exercised by the acceptance programs
- **THEN** the typed failure is preserved and every earlier acquisition is released exactly once without exposing a partial result

### Requirement: Pressure findings determine follow-up work

Each language-pressure program SHALL retain a checked-in findings report that classifies observed
walls as language, standard-library, compiler-defect, tooling/ergonomics, or performance/cost
findings. Each finding SHALL cite evidence and state whether it was repaired, deferred to a focused
proposal, or accepted as local complexity. Later reports SHALL compare repeated findings with
earlier programs before promoting a general design. Completing a pressure program MUST NOT
automatically schedule a neighboring compiler port, install the example as production
infrastructure, or begin continuous self-hosting.

#### Scenario: A wall is encountered during implementation

- **WHEN** completing the lexer requires awkward source, a missing general operation, a compiler repair, or a material cost
- **THEN** the report records its category, concrete evidence, disposition, and the smallest plausible general follow-up

#### Scenario: Independent programs expose the same wall

- **WHEN** a later pressure program independently reproduces a deferred language or standard-library finding
- **THEN** the later report compares both evidence sets and states whether they now justify a focused proposal

#### Scenario: A pressure exercise completes

- **WHEN** all acceptance gates for one pressure program pass
- **THEN** the next pressure program or repair is selected from the recorded evidence rather than from a predetermined self-hosting module order
