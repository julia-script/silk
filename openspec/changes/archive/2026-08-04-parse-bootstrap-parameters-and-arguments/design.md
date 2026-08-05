## Context

The lexer currently treats `:` and `,` as invalid bytes, the parser accepts only empty parameter and
argument lists, and semantic analysis has a closed integer-or-call returned-expression slice. The
change crosses lexer, CST, parser, semantic fallback behavior, and the inspector, so the recovery
boundaries and temporary semantic contract must be decided together.

## Goals / Non-Goals

**Goals:**

- Extend the lossless grammar without weakening exact token ownership or deterministic recovery.
- Represent parameters, arguments, and bare identifier expressions as concrete syntax only.
- Keep existing parameterless and zero-argument programs behaviorally unchanged.
- Give every parser behavior a visible inspector preset.

**Non-Goals:**

- Parameter declaration facts, local lookup, argument binding, general expression parsing, or evaluation.
- Nested call arguments, operators, additional types, default values, labels, or variadics.
- AST, HIR, MIR, LLVM lowering, or compatibility shims for the current public union shapes.

## Decisions

### Keep the first value grammar closed and non-recursive

`ReturnExpression` gains a bare identifier alongside the current integer and call forms. Call
arguments are limited to decimal integers or bare identifiers; calls cannot yet nest inside calls.
This supports the first `identity(42)` data path while avoiding precedence or recursive recovery
questions. A general recursive expression grammar is deferred until the language needs it.

### Give lists their own concrete ownership

Parameter and argument list nodes own ordered item nodes, commas, trivia, and error regions. Items
do not absorb separators. This keeps reconstruction and span inspection straightforward and gives
future semantic passes a stable source-order mapping. Flattening list contents into the parent call
or function was rejected because it would make recovery and positional binding harder to explain.

### Recover at explicit list and declaration boundaries

Parameter recovery synchronizes on comma, right parenthesis, return arrow, following `pub`, or EOF.
Argument recovery synchronizes on comma, right parenthesis, closing brace, following `pub`, or EOF.
Each loop must either consume a concrete token or insert one missing element. This follows the
existing parser invariant and prevents a damaged first function from swallowing the next one.

### Defer semantics as closed data

Declaration facts record only the exact parameter count in this change. Bare identifier expressions
receive an explicit unavailable fact, and calls keep their existing top-level target/type facts
without claiming anything about arguments. Silent placeholder parameter identities were rejected
because the next change should establish that contract once, with diagnostics and provenance.

## Risks / Trade-offs

- [Two list parsers may drift] → Share only a narrow internal list-loop shape while keeping parameter and argument node construction concept-specific.
- [Future expression grammar may replace the non-recursive argument slice] → Keep argument nodes source-faithful so a later parser change can break the semantic API cleanly without migrating source ownership.
- [Deferred semantics could look implemented] → Label unavailable states explicitly in public data, documentation, and the inspector.

## Migration Plan

Add token kinds and fixtures first, then concrete node kinds and parser recovery, then the semantic
fallback and inspector. Update public exports and release-candidate assertions in the same change;
no backward-compatibility adapter is retained during this prerelease stage.
