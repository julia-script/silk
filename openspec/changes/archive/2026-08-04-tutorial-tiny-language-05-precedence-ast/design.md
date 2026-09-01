## Context

The lexer emits arithmetic tokens, but calculation order is not represented. The tutorial needs immutable expression data and a precedence algorithm that can later expand to comparisons without replacement. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Make grouping observable in AST snapshots.
- Teach precedence climbing and left associativity.
- Establish expression data that later parsing and lowering lessons can reuse.

**Non-Goals:**

- Parse complete function definitions or conditionals in this lesson.
- Perform type checking or constant folding.
- Use a parser generator or combinator library.

## Decisions

### Model `Expression` as a discriminated union of immutable records

This matches TypeScript teaching goals and permits exhaustive lowering. Class methods would mix data with phase-specific behavior.

### Use precedence climbing with numeric binding powers

It mirrors Kaleidoscope, keeps the operator table explicit, and makes the `%` transfer exercise localized. A grammar function per precedence tier is simpler initially but less directly extensible.

### Make the binary loop left-associative by parsing the right side at stronger precedence

Tests for subtraction and division will pin this behavior.

### Include the full eventual node union but construct only arithmetic nodes here

Later lessons can add grammar without reshaping already-authored AST consumers.

## Risks / Trade-offs

- [Risk] The generic algorithm obscures simple arithmetic → Trace one expression token by token and visualize its final tree.
- [Risk] Full AST variants feel speculative → Clearly mark call and conditional nodes as destination shapes used in the next lesson.
- [Risk] Snapshot tests become unreadable → Add concise constructors or a didactic renderer solely for assertions.

## Migration Plan

Add `Expression.ts`, the arithmetic portion of `Parser.ts`, parser tests, and Lesson 5. Lesson 6 extends these files rather than replacing their public shapes.
