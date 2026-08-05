## 1. Extend the concrete vocabulary

- [ ] 1.1 Add colon and comma token kinds, lexer recognition, exact-span tests, and public documentation
- [ ] 1.2 Add concrete parameter declaration, identifier expression, and argument-list node shapes with explicit ordered children
- [ ] 1.3 Update compiler fixtures and tree helpers for parameter and argument traversal without changing token ownership

## 2. Parse the value-carrying syntax slice

- [ ] 2.1 Parse empty or comma-separated typed parameter declarations with trivia preserved
- [ ] 2.2 Parse bare-identifier return expressions and calls containing decimal-integer or identifier arguments
- [ ] 2.3 Implement missing-token and unexpected-token recovery at parameter, argument, function, and declaration boundaries
- [ ] 2.4 Add losslessness, source-order, malformed-list, progress, and multi-function parser tests

## 3. Preserve the semantic boundary

- [ ] 3.1 Publish exact concrete parameter counts on function declarations
- [ ] 3.2 Represent bare-identifier expression meaning as explicitly unavailable until parameter resolution exists
- [ ] 3.3 Preserve top-level call resolution while retaining arguments as unchecked syntax
- [ ] 3.4 Add semantic tests proving deferred states do not invent bindings, values, compatibility, or diagnostics

## 4. Make the slice visible

- [ ] 4.1 Add valid identity, multiple-parameter, and malformed-list presets to the hidden Syntax Inspector
- [ ] 4.2 Render parameter and argument branches, token slices, spans, counts, recovery, and deferred semantic labels accessibly
- [ ] 4.3 Verify the inspector interactively at desktop and narrow viewports and keep the page absent from navigation and search

## 5. Document and verify

- [ ] 5.1 Update compiler README grammar, public examples, changeset, and release-candidate validation
- [ ] 5.2 Run focused compiler and docs tests, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`
