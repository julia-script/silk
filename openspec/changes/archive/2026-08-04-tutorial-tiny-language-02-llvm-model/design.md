## Context

Lesson 1 names LLVM as the intermediate target, but the audience has no compiler background. This lesson must establish the minimum IR vocabulary needed by Lesson 7 while deliberately deferring SSA and PHI details. See `proposal.md` for motivation.

## Goals / Non-Goals

**Goals:**

- Give learners a correct mental model of LLVM's role in this tutorial.
- Teach them to read a minimal valid `main` function before generating it.
- Make artifact and tool boundaries explicit.

**Non-Goals:**

- Survey LLVM history, optimization, targets, or APIs.
- Teach SSA, dominance, or PHI nodes before their point of use.
- Generate IR through the package.

## Decisions

### Center the lesson on one annotated `main` function

A concrete IR specimen anchors module, signature, type, block, instruction, and terminator terminology. A glossary-first lesson would be harder to retain.

### Separate artifacts by producer and consumer

A compact table will show Tiny source, textual IR, bitcode, object file, and executable alongside the tool that creates and consumes each one.

### Describe LLVM as compiler infrastructure rather than a VM

This prevents the common assumption that `.ll` or `.bc` is directly executed in the tutorial.

### Defer SSA and PHI details

The lesson may say instruction results are named values, but assignment-once semantics belongs in Lesson 8 and merge semantics in Lesson 10.

## Risks / Trade-offs

- [Risk] The lesson becomes a theory detour → Cap it at the concepts required to read the minimal function and finish with an artifact-labeling exercise.
- [Risk] Simplified backend language implies Clang is LLVM itself → State precisely that Clang is the external driver used here to compile and link generated LLVM IR.
- [Risk] Terminology drifts from LLVM output → Validate all annotated snippets with Clang.

## Migration Plan

Add the numbered LLVM-model page and link it between Lessons 1 and 3. The page is additive and can be removed independently if the series is rolled back.
