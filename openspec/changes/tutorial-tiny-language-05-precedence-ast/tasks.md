## 1. AST and parser implementation

- [x] 1.1 Add immutable discriminated-union data for literal, name, unary, binary, call, and conditional expressions.
- [x] 1.2 Implement primary, unary, and precedence-climbing arithmetic parsing without a parser dependency.
- [x] 1.3 Pin left associativity and explicit precedence for `+`, `-`, `*`, and `/`.

## 2. Learning assets

- [x] 2.1 Add tests for `1 + 2 * 3`, `(1 + 2) * 3`, `10 - 3 - 2`, and `-2 * 3`.
- [x] 2.2 Create the AST tree and token-cursor/minimum-precedence walkthrough with text alternatives.
- [x] 2.3 Create Lesson 5 and its observable AST checkpoint, then update series navigation.

## 3. Verification

- [x] 3.1 Run consumer typecheck and the parser/AST tests.
- [x] 3.2 Confirm the lesson explains that parsing fixes calculation order before LLVM lowering.
- [x] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.
