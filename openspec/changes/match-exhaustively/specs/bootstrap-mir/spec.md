## ADDED Requirements

### Requirement: MIR carries verified logical match regions

MIR SHALL represent a match as one evaluated scrutinee local, exact logical type and access mode,
canonical member cases in source decision order, optional guard regions, pattern-bound locals,
per-arm result and cleanup regions, and one typed join outcome. Member cases SHALL reference the
compiler layout plan while omitting source aliases, public numeric tags, backend types, target
blocks, branch depths, and arbitrary cyclic edges.

#### Scenario: Lower a complete two-member match

- **WHEN** HIR exhaustively matches `Token | End` with two unguarded arms
- **THEN** MIR contains one structured acyclic selection whose cases produce one verified joined result local

#### Scenario: Keep guarded member order

- **WHEN** two guarded arms and one unguarded fallback arm target the same nominal member
- **THEN** MIR preserves their source decision order and guard fallthrough without duplicating the scrutinee payload

### Requirement: MIR verifies match coverage bindings and cleanup

Verification SHALL reject a match whose scrutinee or result local disagrees with its logical type or
layout, whose member cases are invalid or non-exhaustive, whose source decision order contradicts
the semantic coverage facts, whose pattern field or binding types disagree, whose guard is not
`Bool`, whose access mode violates ownership metadata, or whose arm result and cleanup outcomes do
not reach the declared join consistently. Violations SHALL be deterministic data produced before
evaluation or backend emission.

#### Scenario: Reject a missing member case

- **WHEN** hand-built MIR omits one required unguarded member and has no universal case
- **THEN** verification identifies the exact uncovered canonical member

#### Scenario: Reject an escaping borrow local

- **WHEN** a match-local shared or exclusive binding is referenced outside its arm region
- **THEN** verification reports its arm boundary and no backend receives the program

### Requirement: Match MIR encoding is deterministic

Text encoding SHALL include scrutinee and result types, access mode, source-ordered decisions,
canonical members, pattern paths, guards, bound locals, cleanup, arm outcomes, join relationships,
and provenance in stable topological order. Equivalent exhaustive matches SHALL encode
byte-identically across fresh processes.

#### Scenario: Repeat guarded match lowering

- **WHEN** one guarded and destructuring match is lowered repeatedly
- **THEN** its member decisions, regions, bindings, cleanup, join, and encoded bytes are identical
