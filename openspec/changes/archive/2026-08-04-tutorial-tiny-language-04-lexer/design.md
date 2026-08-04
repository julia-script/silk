## Context

The consumer scaffold exists, but no Tiny source can be analyzed. The lexer must be small, observable, Effect-compatible, and reusable unchanged by the browser playground. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Produce deterministic tokens with source offsets.
- Introduce the first typed compiler diagnostic.
- Give learners a complete, testable compiler stage.

**Non-Goals:**

- Handle Unicode identifiers, comments, strings, or numeric formats beyond decimal integers.
- Parse unary signs as part of numeric literals.
- Perform parsing or semantic validation.

## Decisions

### Represent token kinds with string literal unions and immutable records

This keeps pattern matching explicit and avoids TypeScript enums that complicate direct Node execution.

### Keep `-` separate from integer tokens

Unary negation is a parser decision; folding the sign into the lexer would make subtraction context-sensitive.

### Store UTF-16 start and end offsets

They align with JavaScript string indexing and browser editor ranges. The confirmed grammar is ASCII, so byte/code-point distinctions do not affect lesson inputs.

### Expose `Lexer.tokenize` as a named Effect operation

Lexical failures remain typed and observable while internal cursor helpers stay synchronous and private.

## Risks / Trade-offs

- [Risk] Cursor loops forever on unknown input → Require every branch to advance or return `LexError` and include a regression test.
- [Risk] Keyword handling duplicates identifier logic → Scan one identifier lexeme, then classify through a keyword table.
- [Risk] Spans distract from the core scan → Use offsets only; line/column rendering remains a later UI concern.

## Migration Plan

Add `Token.ts`, `Lexer.ts`, lexer tests, and Lesson 4. The compiler entry remains stubbed until parsing lessons land. Rollback removes these additive tutorial files.

