## 1. Complete operation ownership

- [x] 1.1 Move remaining nonprimitive operations to their owners, update callers, and verify static evaluation and stdlib resolution suites.
- [x] 1.2 Regenerate source and documentation catalogs and verify generated-file checks.

## 2. Audit canonical imports

- [x] 2.1 Replace unnecessary aliases in the self-hosted compiler and verify its existing tests.
- [x] 2.2 Review every alias in embedded programs, comments, Markdown, fixtures, and source; update ordinary uses and record justified remaining cases in an audit artifact.
- [ ] 2.3 Document the canonical import convention in the reference style guide and verify examples.

## 3. Integration validation

- [ ] 3.1 Run pnpm typecheck, pnpm format:check, pnpm lint, pnpm test, pnpm check, and pnpm release:candidate; record exact outcomes and remaining failures.
