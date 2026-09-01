## Why

Learners cannot reason about generated IR if LLVM is introduced only as a collection of builder calls. They need a compact mental model of LLVM, its artifacts, and the structure of a valid function before writing lowering code.

## What Changes

- Add a dedicated lesson explaining what LLVM is and what it is not.
- Teach frontend, IR, backend, and linker responsibilities using the Tiny toolchain.
- Distinguish textual LLVM IR, bitcode, object files, executables, and JIT execution.
- Annotate a minimal `main` function and introduce modules, signatures, types, blocks, instructions, and terminators.
- Add a comprehension checkpoint that asks learners to label IR and order the artifact pipeline.

## Capabilities

### New Capabilities

None. This change adds documentation and tutorial-example material, so the change opts out of behavioral specs with `skip_specs: true`.

### Modified Capabilities

None.

## Impact

Adds conceptual documentation and an annotated IR asset. It reuses the existing public behavior documentation and introduces no code or API changes.
