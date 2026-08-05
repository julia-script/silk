## 1. Parse recursive argument expressions

- [x] 1.1 Route call arguments through the shared integer, identifier, and call-expression parser
- [x] 1.2 Preserve nested argument, call, delimiter, trivia, and token spans losslessly
- [x] 1.3 Bound damaged-inner-call recovery at sibling arguments, enclosing calls, blocks, and following declarations
- [x] 1.4 Add parser fixtures and deterministic tests for single, sibling, damaged, and representative deep nested calls

## 2. Preserve the transitional semantic boundary

- [x] 2.1 Add the explicit unavailable nested-argument semantic shape with exact syntax provenance
- [x] 2.2 Make enclosing call contracts unavailable without inventing mappings or semantic diagnostics
- [x] 2.3 Add semantic tests proving existing flat expressions are unchanged and nested syntax is not misclassified

## 3. Make nested syntax visible

- [x] 3.1 Add valid and malformed nested-call presets to the hidden Syntax Inspector
- [x] 3.2 Render the nested semantic placeholder and phase-owned diagnostics without implying evaluation
- [x] 3.3 Verify both concrete nesting and recovery manually at desktop and narrow viewports

## 4. Document and verify the parser increment

- [x] 4.1 Update the compiler grammar documentation for recursive call arguments and the transitional semantic boundary
- [x] 4.2 Add a compiler changeset and packed release-candidate coverage for nested CST shape and deep imports
- [x] 4.3 Run focused parser, semantic, and docs tests, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`
