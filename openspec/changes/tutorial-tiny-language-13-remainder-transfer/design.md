## Context

The guided compiler is complete. The final lesson must test independent transfer by extending the existing operator pipeline while withholding the finished implementation from the main path. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Require the learner to change every stage genuinely affected by `%`.
- Provide objective success criteria and recovery hints without solution-equivalent scaffolding.
- Consolidate the compiler architecture and point toward recursion and the optional playground.

**Non-Goals:**

- Add `%` to the guided core before the exercise.
- Introduce user-defined operators or a generalized precedence DSL.
- Implement or host the playground in this change.

## Decisions

### Assign `%` the same precedence and associativity as `*` and `/`

This creates a meaningful parser test while following conventional arithmetic expectations.

### Lower `%` with signed `srem`

It matches Tiny's signed `i32` division semantics; `urem` would be inconsistent.

### Keep a complete reference solution only in validation assets

The lesson lists touched stages, tests, and hints but does not provide the final patch.

### Use both parser-only and native success criteria

A runtime result alone could pass despite incorrect precedence; AST and IR checks demonstrate full transfer.

## Risks / Trade-offs

- [Risk] Naming the required stages makes the exercise too guided → State interfaces and criteria but omit code structure within each function.
- [Risk] Hidden validation diverges from prose → Derive it from the exact public grammar and run it in CI.
- [Risk] Learners finish without consolidating concepts → Require a short explanation of why unrelated resolver/control-flow code did not change.

## Migration Plan

Add Lesson 13, exercise fixtures, and hidden reference validation. Keep the reference solution out of the published step-by-step content. Rollback removes only the transfer lesson and its validation assets.

