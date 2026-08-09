## Why

The lexer produced one instance of closed numeric codes, awkward owned-sequence reads, and allocation growth, but one consumer is not enough to design general language or library features. A small stack bytecode VM applies independent, recognizable low-level pressure to the same seams while also exercising bounded execution, invalid programs, and an allocated execution trace.

## What Changes

- Add a visible stack bytecode VM written in ordinary Silk. It consumes a runtime-sized borrowed bytecode slice, executes against a bounded fixed operand stack, and returns an owned growable trace plus diagnostics.
- Differentially compare valid, branching, and malformed programs with a small canonical TypeScript reference VM.
- Exercise evaluator, native LLVM, and direct WebAssembly parity; fresh-process determinism; and typed allocation failure at every trace or diagnostic growth ordinal.
- Require only general language, effect, allocator, and `silk.vector` mechanisms—no VM-, opcode-, or stack-specific compiler primitive.
- Record categorized findings and explicitly compare them with the lexer findings before promoting constants/enums, shared Vector reads, or performance work.
- Keep installing a VM, adding self-hosting work, and preemptively designing the findings' possible solutions out of scope.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-language-pressure-programs`: Add a second recognizable pressure program with differential VM semantics, allocated owned observations, cross-engine resource evidence, and cross-program finding comparison.

## Impact

- Adds a visible example under `examples/language-pressure/stack-vm` and compiler acceptance/determinism tests.
- Reuses runtime slices, fixed arrays, integer operations, loops, effects, allocator provision, `Vector`, evaluator tracing, LLVM, and direct WebAssembly.
- May expose general compiler defects required to complete the program; broader language and stdlib designs remain separate follow-ups justified by the combined findings.
- No public compiler API, package export, dependency, host service, or runtime scheduler change is planned.
