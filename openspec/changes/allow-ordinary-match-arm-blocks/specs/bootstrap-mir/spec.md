## MODIFIED Requirements

### Requirement: MIR carries verified logical match regions

MIR SHALL represent a match as one evaluated scrutinee local, exact logical type and access mode,
canonical member cases in source decision order, optional guard regions, pattern-bound locals,
per-arm expression or ordinary statement regions, their completion and cleanup outcomes, and one typed match outcome. A normally completing block SHALL supply unit with zero payload lanes. Only normally completing paths SHALL assign or access a join destination; a returning, failing, breaking, continuing, or otherwise noncompleting path SHALL transfer without reading an uninitialized result or executing continuation-only operations. An all-noncompleting match SHALL require no joined result local. Member cases SHALL reference the
compiler layout plan while omitting source aliases, public numeric tags, backend types, target
blocks, branch depths, and arbitrary cyclic edges.

#### Scenario: Lower a complete two-member match

- **WHEN** HIR exhaustively matches `Token | End` with two unguarded arms
- **THEN** MIR contains one structured acyclic selection whose cases produce one verified joined result local

#### Scenario: Keep guarded member order

- **WHEN** two guarded arms and one unguarded fallback arm target the same nominal member
- **THEN** MIR preserves their source decision order and guard fallthrough without duplicating the scrutinee payload

#### Scenario: Lower selected statements with unit completion

- **WHEN** a selected ordinary block performs several statements and reaches its closing brace
- **THEN** MIR preserves statement order, produces unit without payload storage, and continues the enclosing body without a synthesized return

#### Scenario: Stop a containing expression on transfer

- **WHEN** a match nested in an argument, initializer, assignment operand, or return operand has a selected block that transfers
- **THEN** MIR terminates that path before later operands, call execution, initializer storage, destination replacement, or later statements; earlier acquired resources receive exactly their transfer cleanup

#### Scenario: Join a scalar only on its completing path

- **WHEN** one match arm returns from the enclosing body and another produces `i32`
- **THEN** only the scalar-producing path initializes the scalar join destination and no transfer path reads or writes it

### Requirement: MIR verifies match coverage bindings and cleanup

Verification SHALL reject a match whose scrutinee or result local disagrees with its logical type or
layout, whose member cases are invalid or non-exhaustive, whose source decision order contradicts
the semantic coverage facts, whose pattern field or binding types disagree, whose normally completing guard paths do not produce
`bool`, whose access mode violates ownership metadata, or whose arm result and cleanup outcomes do
not agree with the declared completion and transfer outcomes. Verification SHALL reject a join read without a value on every reaching path, a join assignment on a noncompleting path, or continuation-only operations or cleanup attached to an unconditional transfer. Violations SHALL be deterministic data produced before
backend emission.

#### Scenario: Reject a missing member case

- **WHEN** hand-built MIR omits one required unguarded member and has no universal case
- **THEN** verification identifies the exact uncovered canonical member

#### Scenario: Reject an escaping borrow local

- **WHEN** a match-local shared or exclusive binding is referenced outside its arm region
- **THEN** verification reports its arm boundary and no backend receives the program

#### Scenario: Reject use after noncompletion

- **WHEN** hand-built MIR reads a required match result after an arm transfers without reaching the join or attaches later argument evaluation to that transfer path
- **THEN** verification reports the inconsistent path and result provenance before backend emission

#### Scenario: Transfer while evaluating a guard

- **WHEN** a guard contains a nested match whose selected block transfers from the enclosing computation or loop
- **THEN** MIR takes that exit with applicable cleanup and executes no later candidate; only a normally completing Boolean-false guard advances candidates, and an all-transferring guard requires no Boolean result local
