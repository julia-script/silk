## Why

The existing LLVM tutorial begins at lowering, so a compiler-curious learner does not first see the complete journey from Tiny source to a native executable. A destination-first orientation is needed to make every later lesson feel like progress toward one coherent result.

## What Changes

- Add the opening lesson for the Tiny compiled-language tutorial.
- Preview the final `abs`/`score`/`main` program, its major intermediate artifacts, and exit code `20`.
- Introduce the source → tokens → AST → LLVM IR → Clang → executable pipeline.
- Define the responsibility split between the Tiny frontend, `@silklang/llvm`, and Clang.
- Provide a static pipeline diagram and an artifact-matching checkpoint that do not depend on the playground.

## Capabilities

### New Capabilities

None. This change adds documentation and tutorial-example material, so the change opts out of behavioral specs with `skip_specs: true`.

### Modified Capabilities

None.

## Impact

Adds the first section and pipeline assets for the new tutorial. It affects package documentation and tutorial validation fixtures only; no runtime API changes are required.

