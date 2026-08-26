# Silk Effect

Silk Effect is an unreleased low-level systems language built around explicit memory and execution
control, typed failures, replaceable service requirements, deterministic cleanup, and
tooling-friendly semantics. The repository is a strict TypeScript, ESM-only monorepo containing the
stage-0 compiler, runtime-facing libraries, editor tooling, and language-pressure programs.

Silk currently runs one source program through a lossless frontend, module and type analysis, HIR,
ownership and cleanup planning, specialization, MIR, and three differential execution paths:

- the logical MIR evaluator;
- native code and WebAssembly emitted through LLVM; and
- a direct WebAssembly backend that does not require an external LLVM toolchain.

The implemented bootstrap language includes modules and visibility, scalar families, typed scalar
constants, structs, fixed arrays, runtime slices, structural unions and exhaustive matching,
generics, first-class callables and pipelines, mutation, structured loops, recursion, affine
ownership, shared and exclusive borrowing, deterministic `Drop`, explicit allocation, static text
and byte data, and typed `Effect` computations with explicit service requirements. Canonical
`Result`, `Effect`, and `Vector` operations are ordinary navigable Silk source rather than
compiler-recognized library names. Portable semantic logging is likewise source-defined:
`Effect.log` and its level-specific aliases submit one complete borrowed `string` and a closed
`LogLevel` through an explicit replaceable `Logger`, with stdout and deterministic in-memory
providers supplied by the standard library.
Portable whole-file interaction is source-defined as well: normalized provider-absolute `Path`
values and a seven-operation mutable `FileSystem` service can be implemented by application-owned
native, browser, Wasm, or test providers without introducing ambient storage or host imports.

This is still an alpha language. APIs may break freely, and concurrency, networking, a package
registry, broad FFI, and self-hosting remain future work chosen from executable evidence rather
than compatibility commitments.

## Packages

- [`@silk-effect/compiler`](packages/compiler) — the stage-0 compiler, evaluator, LLVM backend,
  direct WebAssembly backend, embedded Silk standard library, and supported analysis facade.
- [`@silk-effect/compiler-cli`](packages/compiler-cli) — project initialization, checking,
  multi-target builds, native execution, formatting, and documentation generation through `silk`.
- [`@silk-effect/llvm`](packages/llvm) — Effect-native LLVM IR construction and deterministic text
  and bitcode emission.
- [`@silk-effect/wasm`](packages/wasm) — deterministic WebAssembly module construction and binary
  emission.
- [`@silk-effect/language`](packages/language) — lexer-driven CodeMirror support and the Silk
  TextMate grammar consumed by Shiki and VS Code-compatible editors.
- [`@silk-effect/lsp`](packages/lsp) — project-aware diagnostics, hover, navigation, completion,
  inlay hints, symbols, and canonical formatting over stdio.
- [`@silk-effect/documentation`](packages/documentation) — CommonMark documentation interpretation,
  semantic links, examples, highlighting, and deterministic documentation models.
- [`silk-language`](packages/vscode) — the private Cursor/VS Code extension package.

## Evidence and direction

The compiler is exercised by seven familiar algorithms plus Silk-written lexer and bounded stack
VM pressure programs. Their acceptance gates compare evaluation, native LLVM, and direct
WebAssembly behavior, including allocation-failure rollback and fresh-process determinism.

The current direction and committed work live in [`roadmaps/project.md`](roadmaps/project.md).
Detailed implemented behavior lives under [`openspec/specs`](openspec/specs); completed changes and
their design records live under [`openspec/changes/archive`](openspec/changes/archive).

## Development

```sh
pnpm install
pnpm dev
pnpm build
pnpm check
pnpm release:candidate
```

`pnpm dev` runs package compilers in watch mode alongside the documentation app. `pnpm build`
creates a dependency-ordered production build of every workspace package and app.

Effect-returning tests use `it.effect` from `@effect/vitest`; pure tests use ordinary `it` with
`assert`. Package-facing changes require a Changesets entry and a validated packed release
candidate.

## License

[MIT](LICENSE) © 2026 Julia Ortiz
