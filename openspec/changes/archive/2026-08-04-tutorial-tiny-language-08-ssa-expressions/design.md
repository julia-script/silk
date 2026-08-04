## Context

Literal lowering works, but Tiny arithmetic and comparisons require recursive value production. This is the first lesson where named instruction results appear, making an explicit SSA model necessary. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Lower all guided unary, arithmetic, and comparison expressions.
- Teach SSA as the relationship between expression nodes and immutable typed results.
- Keep every language expression normalized to `i32`.

**Non-Goals:**

- Teach formal SSA construction, dominance, or PHI nodes.
- Optimize or constant-fold expressions.
- Add `%`; it remains the transfer exercise.

## Decisions

### Use one private recursive expression-lowering function returning `Value.Input`

Constants and SSA results share the public operand input type, avoiding a parallel wrapper hierarchy.

### Map `/` to `sdiv` and comparisons to signed predicates

Tiny's only numeric type is signed `i32`; unsigned operations would contradict the language contract.

### Zero-extend comparison `i1` results to `i32`

A uniform expression type allows comparisons to be returned, passed, or later used as truthy conditions without a separate type checker.

### Teach SSA through an AST-to-result diagram

Every result name corresponds to one expression node; mutable-source comparison is illustrative but Tiny itself still has no assignment.

## Risks / Trade-offs

- [Risk] Learners infer SSA names control evaluation order → Reiterate that the AST and instruction dependencies establish order.
- [Risk] Comparison widening feels arbitrary → Tie it directly to the single-type Tiny semantic contract.
- [Risk] Signed overflow semantics are overexplained → State the tutorial does not add no-wrap flags and defer overflow policy.

## Migration Plan

Extend `Compiler.ts`, add expression IR/native tests and SSA assets, and add Lesson 8. The public compiler result remains textual IR.

