## Context

See `proposal.md` for motivation. `Parser.parse` currently invokes the existing function parser once
and then expects end-of-file. The concrete source-file node therefore has one function child, while
`SemanticAnalysis` intentionally locates and analyzes only the first direct function child.

## Goals / Non-Goals

**Goals:**

- Repeat the already-proven function grammar without generalizing statements or expressions.
- Keep function boundaries recoverable and every lexer token represented exactly once.
- Preserve the current single-function behavior and empty-input recovery.
- Make the new concrete structure immediately visible in the inspector.

**Non-Goals:**

- Collecting semantic facts for later functions.
- Duplicate-name rules, calls, references, AST, HIR, MIR, or lowering.
- Making an empty source file valid.

## Decisions

### The source file contains a non-empty ordered declaration sequence

The parser will always parse one function, then repeat while concrete input remains before EOF. This
preserves the current total recovery result for empty input while allowing any positive number of
functions. A separate generic list-node abstraction is unnecessary; direct `FunctionDeclaration`
children already express source order.

The alternative of accepting zero declarations would silently turn today's useful missing-syntax
diagnostics into success. The alternative of supporting exactly two functions would encode a
temporary milestone limit into the grammar.

### Trivia keeps its existing consumption rule

The existing `expect` operation consumes leading trivia with the expected token. Inter-function
trivia will therefore precede the next declaration's `pub` leaf, while trailing trivia remains with
the EOF expectation. This keeps token identity and source order without a new trivia ownership model.

### A following `pub` synchronizes a missing right brace

Block recovery will treat `PubKeyword` as a source-level synchronization point when a right brace is
absent. That allows the parser to insert the missing brace without consuming the next declaration.
Unexpected input between complete declarations is retained as one bounded error region before the
next function begins.

### Semantic analysis remains explicitly first-function-only

This ticket does not change the semantic result shape. The analyzer continues to select the first
direct `FunctionDeclaration`; compiler documentation and the inspector will label that limitation.
Collecting all declarations belongs to the immediately dependent change, avoiding a parser ticket
that also decides lookup and duplicate semantics.

### The inspector supplies the acceptance visualization

A two-function preset and a damaged-first-function preset will show the top-level branches, token
coverage, parser diagnostics, and the honest first-only semantic boundary. The route remains hidden
from navigation and search.

## Risks / Trade-offs

- **The intermediate build parses declarations it does not analyze** → Label the boundary in public
  docs and the inspector, and keep `collect-bootstrap-declarations` as the next dependency.
- **Recovery could absorb the next function** → Add focused fixtures where the first closing brace
  is missing and where punctuation appears between declarations.
- **Repeated immutable array growth is quadratic for huge files** → Keep the straightforward loop
  for this bootstrap corpus; optimize only with measured evidence and without changing the API.

## Migration Plan

Update parser fixtures and documentation as a breaking prerelease grammar expansion. Existing
single-function consumers remain valid. Rollback is the previous single invocation of the function
parser; no persisted data or compatibility shim exists.
