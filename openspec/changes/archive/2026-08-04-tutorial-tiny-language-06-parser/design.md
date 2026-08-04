## Context

Arithmetic parsing exists, but the confirmed Tiny program contains definitions, parameters, calls, comparisons, and `if/then/else`. The parser must produce one complete immutable `Program` and preserve source-owned diagnostics. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Parse the complete confirmed grammar.
- Keep parser errors precise enough for local CLI and future browser display.
- Freeze the AST contract consumed by lowering lessons.

**Non-Goals:**

- Resolve names or function arity.
- Introduce semicolons, comments, prototypes, or external declarations.
- Recover and continue after multiple syntax errors.

## Decisions

### Parse a sequence of `fn` definitions until EOF without semicolon syntax

The confirmed examples use no terminators; the next `fn` or EOF naturally follows a completed expression. Unexpected residual tokens still fail.

### Treat bare identifiers as parameter references and an immediate `(` suffix as calls

Tiny intentionally supports only named direct calls, avoiding a general postfix-expression grammar.

### Give `if` the lowest expression precedence

Each branch recursively accepts the full expression grammar, matching expression-valued semantics.

### Reject duplicate parameters while constructing a function definition

The parser owns that local syntactic invariant; function-table and call checks remain in resolution.

## Risks / Trade-offs

- [Risk] Missing delimiters produce confusing downstream errors → Each production reports its expected token and current span.
- [Risk] Nested `if` binds `else` incorrectly → Recursive `if` parsing and focused dangling-else tests pin the intended nearest-expression structure.
- [Risk] Parser state leaks into public data → Keep cursor mutation private and return frozen `Program` values.

## Migration Plan

Extend parser and AST files, add `Program.ts`, complete parser fixtures/tests, and add Lesson 6. Downstream compiler code should depend only on the resulting immutable data.

