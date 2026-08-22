## Why

`Parser.ts` is a 3,274-line god-module holding expression, type, statement, declaration, and import grammars in one file threaded through a single `State`/recovery core. Two grammar seams (service/interface declarations and callable-contract tails) are already copy-pasted, and lookahead predicates re-parse tokens their target production parses again.

## What Changes

- **Split `Parser.ts` into per-grammar actors** (`Parser/Expression.ts`, `Parser/Type.ts`, `Parser/Statement.ts`, `Parser/Declaration.ts`, `Parser/Import.ts`) over a shared `internal/ParseState.ts`.
- **Merge `parseServiceDeclaration`/`parseInterfaceDeclaration`** into one `parseServiceLikeDeclaration(keyword)`.
- **Merge the callable-contract tail** shared by `parseServiceOperation` and `parseFunctionDeclaration`.
- **Route all lookahead through one trivia-skipping `peek(n)`** and convert `startsPatternBindingStatement`/`startsAssignmentStatement` into token-lookahead predicates instead of throwaway parses.
- **Unify keyword classification** in `Lexer.ts` (single `keywordSpellings` table; delete the manual per-byte block).

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

<!-- none: behavior-preserving refactor (skip_specs) -->

## Impact

`Parser.ts` shrinks to dispatch; the five sub-modules are internal imports only, not public subpaths. `Lexer.ts` keyword-table change is behavior-preserving. Spans and diagnostics must stay byte-identical. `skip_specs: true`.
