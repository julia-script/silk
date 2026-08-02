## Context

Tiny comparisons yield `i32`, but `if` requires control flow and one resulting expression value. The current compiler has no block graph construction beyond one entry block. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Lower nested expression-valued conditionals correctly.
- Teach CFG predecessors, minimal dominance intuition, and PHI selection at the point of use.
- Keep function-body validation responsible for graph completeness.

**Non-Goals:**

- Teach loops, general SSA placement algorithms, switch instructions, or advanced dominance theory.
- Represent booleans as a second Tiny type.
- Expose block management to parser or AST data.

## Decisions

### Compare the lowered `i32` condition against zero to produce `i1`

This implements the confirmed truthiness rule and satisfies `conditionalBranch` without changing expression types.

### Create true, false, and merge blocks before emitting the conditional branch

Destination handles exist before use and the final CFG is visible in construction order.

### Compile each branch at its current insertion point, then branch from its actual terminal block to the outer merge

This supports nested conditionals whose lowering ends in an inner merge block.

### Use a per-body counter for block names and a sealed `i32` PHI

Unique names aid readable IR; sealing validates exactly one incoming value for each predecessor.

## Risks / Trade-offs

- [Risk] A branch expression emits a terminator unexpectedly → Tiny expressions other than `if` never terminate; nested `if` returns at its merge, preserving the outer branch contract.
- [Risk] PHI feels like an imperative assignment → Describe it only as predecessor-indexed value selection.
- [Risk] Missing insertion-point changes corrupt later instructions → Add nested-condition snapshots and lean on transactional body validation.

## Migration Plan

Extend expression lowering, add CFG/PHI diagrams and conditional tests, and add Lesson 10. Rollback removes the conditional lowering branch while earlier expression forms remain functional.

