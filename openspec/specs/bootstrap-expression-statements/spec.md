# bootstrap-expression-statements Specification

## Purpose

Allow unit-producing or diverging expressions to execute as ordered block statements without
inventing bindings, returns, or destruction semantics, while rejecting accidental value discard.

## Requirements

### Requirement: Blocks accept expression statements

Every function, conditional arm, loop body, unsafe block, and effect block SHALL accept a complete
expression wherever a statement may begin. The expression statement SHALL retain the complete
expression, trivia, and source-owned span as one concrete syntax branch. A leading writable place
followed by `=` SHALL remain an assignment statement; otherwise a leading identifier expression
SHALL remain eligible to form an expression statement. Expression statements SHALL require no
semicolon or newline delimiter and SHALL stop at the same expression and following-statement
boundaries used by other expression-bearing statements.

#### Scenario: Run a unit effect from effectful main

- **WHEN** `pub effect fn main() -> () { run foo() }` calls an effect function returning `()`
- **THEN** the body contains one expression statement followed by its implicit unit return and produces no parser diagnostic

#### Scenario: Keep following statements in source order

- **WHEN** a block contains `run first()` followed by `run second()` and an explicit unit return
- **THEN** the syntax tree retains two expression statements followed by the return statement in source order

#### Scenario: Preserve assignment precedence

- **WHEN** one identifier-led statement spells `value = replacement()` and another spells `observe(value)`
- **THEN** the first remains an assignment statement and the second is an expression statement

#### Scenario: Accept an ordinary unit call

- **WHEN** an ordinary function body uses a unit-returning call as a standalone statement
- **THEN** the call is accepted as an expression statement without requiring a synthetic binding

### Requirement: Expression statements do not discard values implicitly

An available expression statement SHALL be semantically valid only when its result type is
compatible with `()` or `never`. A non-unit, non-diverging result SHALL produce one semantic
diagnostic whose primary span covers the expression, whose structured reason retains the actual
type, and whose message or notes direct the author to bind, return, or explicitly consume the value.
An unavailable expression SHALL preserve its originating diagnostic without adding a duplicate
statement-result diagnostic.

#### Scenario: Reject a scalar result

- **WHEN** a standalone call expression has result type `i32`
- **THEN** one semantic diagnostic identifies `i32` as the unconsumed result and suggests binding or returning it

#### Scenario: Reject an owned result before lowering

- **WHEN** a standalone expression produces an owned value requiring cleanup
- **THEN** the expression-statement diagnostic prevents executable lowering rather than silently discarding or implicitly dropping that value

#### Scenario: Accept a diverging expression

- **WHEN** a standalone expression has result type `never`
- **THEN** it is accepted without a statement-result diagnostic

#### Scenario: Preserve an unavailable cause

- **WHEN** a standalone expression is unavailable because of an existing parser or semantic diagnostic
- **THEN** no additional result-compatibility diagnostic is emitted for that expression statement

### Requirement: Expression statements have first-class semantic and HIR identity

Every available expression statement SHALL publish a statement fact and an HIR evaluate statement
carrying its expression, region, and exact source provenance. It MUST NOT create a binding identity,
be represented as a return, or acquire explicit-drop semantics. Statement and expression queries,
semantic occurrences, deterministic HIR encoding, layout discovery, and ownership traversal SHALL
include its expression in source order.

#### Scenario: Inspect an expression statement

- **WHEN** analysis inspects a valid standalone `run foo()` expression
- **THEN** it reports one expression-statement fact and one HIR evaluate statement at the authored span with no synthetic binding, return, or drop

#### Scenario: Encode expression statements deterministically

- **WHEN** equivalent source containing expression statements is analyzed repeatedly in fresh processes
- **THEN** its statement facts and encoded HIR are byte-identical across runs

### Requirement: Expression statements execute in source order on every supported target

Lowering SHALL evaluate each valid expression statement exactly once, preserve its effect and
failure behavior, and continue with the next statement only after successful completion. Unit
results SHALL require no value cleanup. A `run` expression that propagates a declared failure from
an enclosing effect function SHALL use the same propagation and ownership cleanup path as the same
`run` expression in a binding or return. Native execution and LLVM-generated WebAssembly
execution SHALL agree on outcomes and observable traces.

#### Scenario: Execute two unit effects in order

- **WHEN** two standalone `run` expressions append distinguishable observations
- **THEN** every supported target records each observation exactly once in source order

#### Scenario: Propagate a declared failure from effectful main

- **WHEN** effectful `main` runs a unit effect statement that fails with an error declared by `main`
- **THEN** execution stops before the following statement, performs required cleanup, and reports the failure through the entry termination path

#### Scenario: Keep successful execution moving

- **WHEN** a unit expression statement completes successfully before a following return
- **THEN** every supported target evaluates the following return normally

### Requirement: Canonical formatting preserves expression statements

Canonical formatting SHALL print each expression statement as an ordinary statement on its own
block line, preserve attached comments under existing statement-comment rules, and remain
idempotent. Formatting and reparsing SHALL retain expression-statement identity and ordering.

#### Scenario: Format adjacent run statements

- **WHEN** a block contains two adjacent standalone `run` expressions
- **THEN** canonical formatting prints them on separate indented lines and reparsing retains two expression statements
