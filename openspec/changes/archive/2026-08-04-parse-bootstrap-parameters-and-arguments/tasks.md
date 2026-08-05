## 1. Extend the concrete vocabulary

- [x] 1.1 Add colon and comma token kinds, lexer recognition, exact-span tests, and public documentation
- [x] 1.2 Add concrete parameter declaration, identifier expression, and argument-list node shapes with explicit ordered children
- [x] 1.3 Update compiler fixtures and tree helpers for parameter and argument traversal without changing token ownership

## 2. Parse the value-carrying syntax slice

- [x] 2.1 Parse empty or comma-separated typed parameter declarations with trivia preserved
- [x] 2.2 Parse bare-identifier return expressions and calls containing decimal-integer or identifier arguments
- [x] 2.3 Implement missing-token and unexpected-token recovery at parameter, argument, function, and declaration boundaries
- [x] 2.4 Add losslessness, source-order, malformed-list, progress, and multi-function parser tests

## 3. Preserve the semantic boundary

- [x] 3.1 Publish exact concrete parameter counts on function declarations
- [x] 3.2 Represent bare-identifier expression meaning as explicitly unavailable until parameter resolution exists
- [x] 3.3 Preserve top-level call resolution while retaining arguments as unchecked syntax
- [x] 3.4 Add semantic tests proving deferred states do not invent bindings, values, compatibility, or diagnostics

## 4. Make the slice visible

- [x] 4.1 Add valid identity, multiple-parameter, and malformed-list presets to the hidden Syntax Inspector
- [x] 4.2 Render parameter and argument branches, token slices, spans, counts, recovery, and deferred semantic labels accessibly
- [x] 4.3 Verify the inspector interactively at desktop and narrow viewports and keep the page absent from navigation and search

## 5. Document and verify

- [x] 5.1 Update compiler README grammar, public examples, changeset, and release-candidate validation
- [x] 5.2 Run focused compiler and docs tests, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`
