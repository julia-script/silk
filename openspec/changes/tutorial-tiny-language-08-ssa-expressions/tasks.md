## 1. Expression lowering

- [x] 1.1 Implement private recursive expression lowering returning `Value.Input`.
- [x] 1.2 Map `+`, `-`, `*`, `/`, and unary `-` to signed LLVM operations and `FunctionBody.negate`.
- [x] 1.3 Lower `<` and `>` with signed `icmp` predicates and zero-extend `i1` results to language-level `i32`.

## 2. SSA instruction

- [x] 2.1 Create Lesson 8 with the mutable-source/versioned-value contrast and AST-to-SSA mapping.
- [x] 2.2 Add an operator-to-LLVM table and explain typed one-assignment instruction results without introducing PHI or formal dominance.
- [x] 2.3 Add IR/native checkpoints for precedence and comparison normalization.

## 3. Tests and verification

- [x] 3.1 Add IR and native tests for arithmetic precedence, unary negation, signed division, and comparison results.
- [x] 3.2 Run consumer typecheck and compiler tests against the packed package.
- [x] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.
