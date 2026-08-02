## 1. AST and parser implementation

- [ ] 1.1 Add immutable discriminated-union data for literal, name, unary, binary, call, and conditional expressions.
- [ ] 1.2 Implement primary, unary, and precedence-climbing arithmetic parsing without a parser dependency.
- [ ] 1.3 Pin left associativity and explicit precedence for `+`, `-`, `*`, and `/`.

## 2. Learning assets

- [ ] 2.1 Add tests for `1 + 2 * 3`, `(1 + 2) * 3`, `10 - 3 - 2`, and `-2 * 3`.
- [ ] 2.2 Create the AST tree and token-cursor/minimum-precedence walkthrough with text alternatives.
- [ ] 2.3 Create Lesson 5 and its observable AST checkpoint, then update series navigation.

## 3. Verification

- [ ] 3.1 Run consumer typecheck and the parser/AST tests.
- [ ] 3.2 Confirm the lesson explains that parsing fixes calculation order before LLVM lowering.
- [ ] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.


