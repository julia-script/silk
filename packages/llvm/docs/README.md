# `@silk-effect/llvm` documentation

`@silk-effect/llvm` constructs LLVM IR and emits LLVM assembly or bitcode without loading Zig,
native LLVM libraries, or external executables at runtime.

## Start here

New to compilers? Start with
[Build Tiny, a compiled language](./tutorials/tiny-language/01-meet-tiny.md). The 13-lesson series
begins with source text and will build a lexer, parser, AST, LLVM lowering pipeline, and native
executable from start to finish.

Already familiar with compiler frontends? Follow
[Build a tiny expression compiler](./tutorials/tiny-expression-compiler.md). It starts directly at
LLVM lowering and ends with a real function emitted as textual IR and bitcode.

## Solve a task

- [How to declare globals, aliases, and functions](./how-to/declarations.md)
- [How to build branching control flow](./how-to/control-flow.md)
- [How to emit memory, atomic, and intrinsic operations](./how-to/memory-atomics-intrinsics.md)
- [How to add debug metadata](./how-to/debug-metadata.md)
- [How to emit and validate LLVM output](./how-to/output.md)

## Look up behavior

- [Actor reference](./reference/actors.md) maps each public module to its responsibility.
- [Behavior and guarantees](./reference/behavior.md) records defaults, ownership constraints,
  transactions, errors, and output properties.
- The exported declarations contain complete TSDoc for individual functions, options, and return
  types.

## Understand the design

- [Why the builder is Effect-native](./explanation/effect-native-builder.md)
- [Why text and bitcode share one model](./explanation/text-and-bitcode.md)

## Scope

The package builds LLVM module state and emits `.ll` text or `.bc` bytes. It does not provide a
JIT, optimizer pipeline, object-code backend, linker, filesystem integration, or process runner.
Those operations belong at an application boundary around the emitted output.
