# `@silk-effect/compiler`

`@silk-effect/compiler` is the Effect-native stage-0 compiler for Silk. It accepts arbitrary source
bytes, preserves lossless syntax and recovery facts, resolves a complete module closure, and
realizes valid programs through HIR, ownership, specialization, target layout, MIR, evaluation, and
backend emission.

The package deliberately exposes one supported consumer surface: `Analysis`. Individual phase
actors remain importable where their immutable data types are part of an answer, but tools should
not assemble a second compiler by invoking phases directly.

```ts
import { Analysis } from '@silk-effect/compiler'
import * as Effect from 'effect/Effect'

const program = Effect.gen(function* () {
  const frontend = yield* Analysis.ofSource(
    'memory/example',
    new TextEncoder().encode(`pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(42) }`),
  )
  const snapshot = Analysis.realize(frontend)

  console.log(Analysis.diagnostics(snapshot)) // []
  console.log(Analysis.evaluate(snapshot)._tag) // Completed
})
```

## Compiler pipeline

A realized analysis snapshot makes these deterministic artifacts available:

1. source files, tokens, lossless syntax, and unified diagnostics;
2. module closure, declaration index, visibility, and name resolution;
3. typed HIR and semantic occurrences for editor tooling;
4. ownership, borrow, move, and cleanup facts;
5. reachable generic/callable instances and target-aware layouts;
6. backend-neutral structured MIR;
7. logical evaluation traces; and
8. LLVM or direct WebAssembly output.

`Analysis.make` and `Analysis.ofSource` stop after the resilient frontend. Missing or damaged source
becomes queryable diagnostics while unrelated facts remain available. `Analysis.realize` derives a
new immutable runtime snapshot. Evaluation and code generation reject snapshots with source errors
before invoking a backend or toolchain.

The MIR evaluator is the semantic oracle used by differential tests. It is not the production
runtime: native builds use deterministic LLVM bitcode and a pinned Clang toolchain, while the direct
WebAssembly backend emits instantiable modules without external tools.

`Driver.compile` owns artifact-producing builds. Before it resolves project imports, it validates
the compiler, generated catalog, every packaged standard-library source, and the sealed intrinsic
inventory against the normalized graph published by `ToolchainIntegrity.installed()`. After the
reachable program is known, it validates only the selected providers and runtime implementations
that program needs. A damaged or mixed installation returns `ToolchainFailed`; an unavailable
target, unresolved entry, source rejection, backend failure, and external process failure remain
distinct outcomes. Successful artifacts retain the exact toolchain graph digest.

## Implemented language surface

The bootstrap language currently includes:

- canonical modules, imports, visibility, and project source roots;
- boolean, signed and unsigned integer, `usize`, and floating scalar families;
- literal-only explicitly typed scalar constants;
- nominal structs, fixed arrays, runtime-sized borrowed slices, structural unions, and exhaustive
  matching;
- ordinary and effectful functions, first-class callables, automatic data-first sections, and
  pipelines;
- generics and finite kinded failure/requirement rows;
- mutable bindings, transactional place replacement, structured loops, and runtime recursion;
- affine values, moves, shared and exclusive borrows, restricted deterministic `Drop`, allocator
  capabilities, raw storage, and lexical slots;
- immutable UTF-8 `string` views, owned standard-library `String` values, byte literals, and
  escaped triple-quoted multiline text; and
- lazy typed `Effect` computations with failure recovery, retry, shared/exclusive/owned service
  provision, ordinary source-defined combinators, and complete-message semantic logging through an
  explicit replaceable `Logger` service.

The compiler-shipped standard library lives as canonical `.silk` files under [`stdlib/silk`](stdlib/silk).
`Result`, Effect transformations, Option, and the generic growable `Vector<T>` compile through the
same declaration, ownership, specialization, and lowering paths as user code. `Logger`,
`Effect.log`, its level-specific aliases, `StdoutLogger`, and `InMemoryLogger` use those same paths:
callers submit a closed `LogLevel` and complete borrowed `string` messages, while providers own
formatting, retention, and physical output strategy.
Owned `Bytes`, normalized provider-absolute `Path`, allocation-free `FileError`, and the seven-
operation mutable `FileSystem` service are also ordinary source. No platform provider is selected by
an import; native, browser, test, and Wasm applications explicitly provide their implementation.
Native applications may construct the ordinary `silk.os_filesystem.OsFileSystem` provider with an
owned absolute `string` root. Its compiler boundary is limited to unsafe native-only handle intrinsics;
evaluator hosts opt in through the exported `OsFileSystemHost.Provider`, and direct WebAssembly
receives no implicit imports or filesystem ABI.

See the [standard-library string reference](stdlib/README.md#string-and-string) for the distinction
between borrowed `string`, owned `String`, and byte views.

This remains an unreleased subset. It does not yet commit to enums, concurrency, networking, a
general FFI, a package registry, or self-hosting.

## Source resolution and project analysis

Compilation requests carry one explicit root `SourceFile`. Imports use canonical, extensionless,
case-sensitive module identities relative to a compiler-provided source root. `SourceResolver` is
an Effect service: browser and editor tools may provide `SourceResolver.memory`, while filesystem
access belongs to the host boundary rather than the compiler core.

`ProjectAnalysis` analyzes the union closure of synchronized roots once and returns immutable root
views that share syntax, declaration, semantic, tooling, and diagnostic facts. Revising a project
reuses byte-identical syntax and publishes conservative `SyntaxCorrespondence` for structurally
unique unchanged subtrees; semantic facts are recomputed for the complete current revision.

## Editor and documentation facts

The frontend snapshot owns editor semantics as compiler data rather than LSP protocol values.
Semantic occurrences, hover subjects, completion candidates, definitions, document symbols, and
inferred type hints use the same canonical identities as compilation. Standard-library navigation
points to the shipped `.silk` source rather than virtual intrinsic files.

`///` declaration documentation and leading `//!` module documentation remain lossless source
tokens. `DocBlock` attaches them to modules, declarations, type parameters, parameters, fields,
implementations, and implementation operations. Markdown parsing and generated documentation
models belong to `@silk-effect/documentation` so ordinary compilation does not pay that cost.

## Byte and span conventions

- Source is arbitrary bytes and need not be valid UTF-8.
- `SourceFile.make` copies its input and attaches a caller-provided logical identity.
- `SourceSpan` is an owner-qualified half-open byte range `[start, end)`.
- Empty spans represent positions; EOF is `[sourceLength, sourceLength)`.
- A source only returns bytes for a span with the same identity and in-bounds offsets.

Ordinary source errors remain typed facts. Lexer and parser recovery always make progress, preserve
the original token stream, and keep later declarations independently analyzable. Operational
resolver failures stay in Effect's typed error channel; artifact-producing driver outcomes retain
target, distribution, backend, and external-tool failures as separate structured data.
