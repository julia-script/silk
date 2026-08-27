## Context

`@silk-lang/compiler` currently ends at an immutable `Lexer.LexicalResult`: exact source bytes,
owner-qualified spans, explicit trivia and invalid tokens, stable lexical diagnostics, and one EOF
token. The docs app is a Next.js/Fumadocs application whose `/docs` layout can also host direct
Next routes that are not part of the content tree. See `proposal.md` for motivation and the two
delta specs for observable behavior.

The implementation must remain a small vertical slice. The repository is pre-release, so the first
syntax API should prefer a clear contract over compatibility or a generalized parsing framework.

## Goals / Non-Goals

**Goals:**

- Preserve original token objects as concrete leaves and make missing or unexpected syntax
  inspectable without losing source bytes.
- Make recovery progress and diagnostic ordering obvious enough to validate exhaustively on a tiny
  corpus.
- Expose one browser-only proof surface through the existing docs layout without publishing it in
  the documentation information architecture.
- Leave a narrow extension seam for a later grammar while accepting that unreleased syntax kinds
  and tree shapes may change.

**Non-Goals:**

- A parser-combinator library, generated parser, green/red tree, interning, incremental reparsing,
  stable syntax IDs, or editor protocol.
- A separate abstract syntax tree or any declaration, name, type, contract, ownership, HIR, or MIR
  fact.
- Arbitrary binary editing, source persistence, server execution, or a supported language
  playground in the docs app.

## Decisions

### 1. Add three public actors around one concrete tree

`SyntaxTree` owns the closed node-kind vocabulary, immutable node and element data, missing-token
data, traversal, and source-span access. A syntax element is exactly one of:

- a `SyntaxTree.Node` with ordered child elements;
- an existing `Token.Token` leaf from the lexical result; or
- a `SyntaxTree.MissingToken` carrying the expected `TokenKind` and an empty source span.

The first node kinds are `SourceFile`, `FunctionDeclaration`, `ParameterList`, `ReturnType`,
`Block`, `ReturnStatement`, `IntegerLiteralExpression`, and `Error`. `Error` nodes own unexpected
concrete tokens; missing tokens are leaves because the absent thing is a token, not an invented
subtree.

`ParseDiagnostic` owns stable parser diagnostic data. `Parser.parse` consumes a
`Lexer.LexicalResult` and returns that lexical result, the root `SyntaxTree.Node`, and a readonly
parser-diagnostic collection. Root exports and package subpaths remain explicit.

This reuses token identity and avoids parallel token wrappers or an early generic `Diagnostic`
hierarchy. A single `Syntax` grab-bag module and a class hierarchy were rejected because they would
hide actor boundaries and encourage methods or unrelated helpers to accumulate around the tree.

### 2. Use direct recursive descent for only the accepted grammar

The parser mirrors the seven fixed grammar regions directly: source file, function declaration,
empty parameter list, named return type, block, return statement, and decimal integer expression.
It does not introduce parser combinators, precedence tables, generic lists, or reusable declaration
machinery before a second real form exists.

Parser state is an immutable cursor plus accumulated syntax elements and diagnostics. Fixed-depth
grammar functions return the next state rather than exposing mutable parser objects. This keeps
source mistakes in ordinary return data and avoids claiming a performance-critical imperative
exception before measurement exists.

### 3. Keep trivia concrete while matching significant tokens

Lookahead skips `Whitespace` and `LineComment` only for grammar decisions. When the parser advances,
every skipped trivia token is appended to the current node in its original position. `Invalid` is
never trivia.

An expectation either consumes the matching significant token, inserts a missing token at the
current significant token's start (or EOF), or consumes unexpected concrete tokens into one
`Error` node. Error recovery includes trivia between unexpected significant tokens so the flattened
leaf order remains identical to the lexical stream.

Each expectation has a deliberately small synchronization set: its expected kind, the next token
kind that could validly continue the fixed grammar, the current enclosing right brace where
applicable, and EOF. An error run stops before a synchronization token. If no concrete token is
consumed, the expectation inserts a missing token. This makes the progress argument local,
preserves later structure when an element is absent, and prevents recovery loops.

The first stable parser codes are `PAR0001` for a missing required token and `PAR0002` for an
unexpected token region. Lexical diagnostics remain owned by `LexicalResult`; the parser neither
copies nor reclassifies them.

### 4. Derive spans from concrete descendants and insertion positions

A node with concrete token descendants spans from the first descendant's start through the last
descendant's end. A node containing only missing elements uses the shared insertion position as an
empty span. EOF remains a concrete token leaf with the source-length empty span. Error-node spans
cover their first through last unexpected concrete token.

Tree flattening must return the exact original token objects in lexical order. This invariant is
checked independently of any pretty-printer and provides the losslessness proof used by both tests
and the inspector.

### 5. Make the inspector a direct Next route, not docs content

`apps/docs/app/docs/labs/syntax-inspector/page.tsx` is a direct route beneath the existing docs
layout. It is not registered with the Fumadocs content loader or root `meta.json`, which keeps it out
of navigation, package sidebars, generated search content, and published package documentation.

A small client component owns one in-memory text value initialized to the accepted fixture. It
encodes that text as UTF-8 under one stable demo source identity, invokes `SourceFile.make`,
`Lexer.lex`, and `Parser.parse`, then renders:

- a recursively indented concrete tree;
- token kinds, escaped slices, and `[start, end)` byte spans;
- missing-token expectations;
- lexical and parser diagnostic lists.

Recomputation is synchronous and local on each edit. The page does not call an API route, local
storage, filesystem, or compiler service. `@silk-lang/compiler` becomes a workspace runtime
dependency of the docs app because its code participates in the client bundle.

An MDX content page was rejected because it would enter the content/search tree and need custom
component registration merely to remain hidden. A standalone app outside the docs layout was
rejected because the demo belongs to the docs site and should reuse its shell.

### 6. Verify the tree contract before the visual surface

Compiler tests use `@effect/vitest` and table-driven source fixtures to prove the accepted shape,
trivia retention, token-object flattening, byte reconstruction, missing-name and missing-brace
positions, unexpected-token grouping, wholly unrelated input termination, and determinism.

The docs build proves the route and browser bundle compile. A local browser smoke check exercises
the initial fixture, one missing brace, and one non-ASCII edit at the direct route. Release-candidate
validation packs and imports every new compiler root and deep export; the package still ships no
source files or docs-app code.

## Risks / Trade-offs

- **[Risk] The tiny grammar could tempt later code to extend ad hoc conditionals indefinitely.** →
  Keep the change limited to the accepted fixture; the next grammar change must reassess direct
  descent against the first genuinely repeated forms.
- **[Risk] Public concrete node shapes may churn as the grammar grows.** → Treat the compiler as
  pre-release, document the narrow guarantee, and add no compatibility adapter or semantic methods.
- **[Risk] Recovery could duplicate, reorder, or drop trivia.** → Make original-token flattening and
  byte reconstruction primary invariants across every valid and malformed fixture.
- **[Risk] A “hidden” lab could accidentally become discoverable.** → Implement it as a direct app
  route outside the content loader and assert that navigation metadata is unchanged.
- **[Risk] Client-bundling the compiler may make the docs lab disproportionately heavy.** → Inspect
  the production build output for the route and keep the lab disposable; do not introduce workers,
  lazy infrastructure, or bundle tooling unless the measured output warrants it.

## Migration Plan

The change is additive and has no existing syntax consumers to migrate. Add the compiler actors and
tests first, extend package exports and release-candidate validation, then add the docs dependency
and route. Rollback removes the direct route and new exports without changing existing source,
span, token, diagnostic, or lexer behavior.
