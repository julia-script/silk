## 1. Expression lowering

- [ ] 1.1 Implement private recursive expression lowering returning `Value.Input`.
- [ ] 1.2 Map `+`, `-`, `*`, `/`, and unary `-` to signed LLVM operations and `FunctionBody.negate`.
- [ ] 1.3 Lower `<` and `>` with signed `icmp` predicates and zero-extend `i1` results to language-level `i32`.

## 2. SSA instruction

- [ ] 2.1 Create Lesson 8 with the mutable-source/versioned-value contrast and AST-to-SSA mapping.
- [ ] 2.2 Add an operator-to-LLVM table and explain typed one-assignment instruction results without introducing PHI or formal dominance.
- [ ] 2.3 Add IR/native checkpoints for precedence and comparison normalization.

## 3. Tests and verification

- [ ] 3.1 Add IR and native tests for arithmetic precedence, unary negation, signed division, and comparison results.
- [ ] 3.2 Run consumer typecheck and compiler tests against the packed package.
- [ ] 3.3 Run `pnpm check` and `pnpm release:candidate`, reporting any pre-existing failure separately.


