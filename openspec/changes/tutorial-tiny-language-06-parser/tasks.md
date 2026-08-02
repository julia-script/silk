## 1. Complete grammar

- [ ] 1.1 Add immutable `Program` and function-definition data.
- [ ] 1.2 Extend parsing for `fn`, parameter lists, name references, direct calls, arguments, `<`, `>`, and `if/then/else`.
- [ ] 1.3 Reject duplicate parameters, incomplete productions, and trailing tokens with source-spanned typed parse errors.

## 2. Tests and lesson

- [ ] 2.1 Add a full AST snapshot for `score.tiny` and focused tests for calls, nested conditionals, missing `else`, and trailing syntax.
- [ ] 2.2 Create Lesson 6 with the confirmed EBNF, milestone output, parser failure recovery, and reduced scaffolding.
- [ ] 2.3 Add `score.tiny` to the example fixtures and update numbered navigation.

## 3. Verification

- [ ] 3.1 Run consumer typecheck and all lexer/parser tests.
- [ ] 3.2 Confirm parser output contains no LLVM handles or lowering decisions.
- [ ] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.


