## 1. Concrete Syntax Model

- [x] 1.1 Implement the `SyntaxTree` actor with the closed first-slice node kinds, immutable nodes,
      original-token leaves, expected-kind missing leaves, and source-owned derived spans.
- [x] 1.2 Add `SyntaxTree` traversal that returns concrete token leaves in exact source order and
      distinguishes nodes, tokens, and missing tokens without casts or class hierarchies.
- [x] 1.3 Implement the `ParseDiagnostic` actor with stable `PAR0001` missing-token and `PAR0002`
      unexpected-region data, concise messages, source-owned spans, and deterministic ordering fields.
- [x] 1.4 Test empty and concrete node spans, missing-token insertion spans, error-node spans,
      immutable children, and original token-object traversal.

## 2. First Function Parser

- [x] 2.1 Implement immutable parser state, trivia-aware significant-token lookahead, concrete token
      consumption, and local progress guarantees over a `Lexer.LexicalResult`.
- [x] 2.2 Parse the exact source-file, public function, empty parameter-list, named return-type,
      block, return-statement, and decimal-integer-expression grammar into the specified node shape.
- [x] 2.3 Implement expected-token insertion with empty spans and one `PAR0001` diagnostic without
      consuming unrelated concrete tokens.
- [x] 2.4 Implement maximal unexpected-token error regions with bounded synchronization at the
      expected kind, next structurally valid kind, enclosing right brace, or EOF and one `PAR0002`
      diagnostic per region.
- [x] 2.5 Return the lexical result, root tree, and ordered readonly parser diagnostics as ordinary
      pure data without throwing or failing an Effect for source mistakes.
- [x] 2.6 Add human-readable valid and malformed fixtures covering the accepted program, dense
      trivia, missing name, missing right brace, unexpected punctuation, empty input, wholly unrelated
      input, invalid UTF-8 bytes, and repeated deterministic parsing.
- [x] 2.7 Test exact node shape, token flattening, byte reconstruction, lexical-diagnostic retention,
      recovery positions, finite progress, and deterministic parser diagnostics for every fixture.

## 3. Public Package Boundary

- [x] 3.1 Export only the new `SyntaxTree`, `ParseDiagnostic`, and `Parser` namespaces from the
      compiler root and add their explicit package subpaths.
- [x] 3.2 Extend the compiler README with the first grammar, parsing example, concrete-syntax
      terminology, recovery contract, and the explicit boundary before semantic AST or HIR work.
- [x] 3.3 Add a Changesets entry and extend release-candidate validation to pack and import every
      new root and deep export without source files or undeclared runtime dependencies.

## 4. Hidden Docs Inspector

- [x] 4.1 Add `@silklang/compiler` as a docs workspace runtime dependency and create the direct
      `/docs/labs/syntax-inspector` route outside the Fumadocs content and search trees.
- [x] 4.2 Build the client-side inspector around in-memory text, UTF-8 source creation, lexing, and
      parsing with the accepted fixture restored on each page load.
- [x] 4.3 Render an accessible concrete tree hierarchy with node kinds, token kinds, escaped exact
      slices, half-open byte spans, missing-token expectations, and separate lexical and parser
      diagnostic collections.
- [x] 4.4 Verify the page remains responsive for empty text, a missing right brace, unexpected ASCII
      punctuation, and unsupported non-ASCII UTF-8 bytes without persistence or network requests.
- [x] 4.5 Confirm the route is absent from normal navigation, package sidebars, generated docs search,
      and published compiler package files, and visually smoke-test the direct route in the local docs
      production build.

## 5. Verification

- [x] 5.1 Run strict OpenSpec validation and the focused compiler typecheck and test commands.
- [x] 5.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and
      `pnpm release:candidate`, fixing every introduced failure and reporting any proven pre-existing
      failure exactly.
