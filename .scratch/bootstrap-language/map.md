# Chart the Silk Effect bootstrap language

## Destination

Produce an implementation-ready specification for the smallest credible Silk Effect bootstrap
language—its semantics, execution model, memory model, compiler architecture, and staged route to a
native self-hosting compiler—clear enough to hand each implementation stage to OpenSpec.

## Notes

- Domain: low-level systems language design and compiler bootstrapping.
- Treat [`VISION.md`](../../VISION.md) and [`CONTEXT.md`](../../CONTEXT.md) as standing constraints
  and vocabulary; do not restate or weaken them in tickets.
- Consult the `grilling` and `domain-modeling` skills for decisions, `prototype` for concrete syntax,
  and `research` for facts outside the repository.
- The compiler is the feature-driving bootstrap program. The milestone is single-threaded and may
  use LLVM for native code generation and linking, but not Node.js or TypeScript at runtime.
- Plan decisions only. Implementation begins through later OpenSpec changes after this map is clear.

## Decisions so far

- [Determine the practical LLVM native target matrix](issues/05-research-llvm-native-target-matrix.md)
  — Require stage-2 self-hosting on Arm64 macOS, x86-64 Linux, and Arm64 Linux; defer Windows and
  WebAssembly hosts while smoke-testing their object emission.

## Not yet specified

- The generated-code predictability and performance threshold beyond semantic correctness, which
  depends on the compiler pipeline and the division of work with LLVM.
- The minimum source-level debugging and source-map story, which depends on the runtime and emitted
  artifact model.
- The exact backend-neutrality constraints worth preserving for a later direct WebAssembly backend,
  which depend on the bootstrap compiler's intermediate representations.

## Out of scope

- Effect integration across WebAssembly or native FFI; it is a later convenience, not the language's
  purpose or a bootstrap milestone requirement.
- Concurrency, atomics, async scheduling, networking, and a user-facing FFI.
- Package registries, dependency solving, a production build system, and a full language server.
- Implementing a direct WebAssembly backend; the bootstrap architecture should merely avoid an
  unnecessary LLVM lock-in where doing so is cheap and clear.
