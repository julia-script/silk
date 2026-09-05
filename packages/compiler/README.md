# `@silklang/compiler`

`@silklang/compiler` is the Effect-native stage-0 compiler for Silk. It accepts arbitrary source
bytes, preserves lossless syntax and recovery facts, resolves a complete module closure, and
realizes valid programs through HIR, ownership, specialization, target layout, MIR, and LLVM
backend emission.

The package deliberately exposes one supported compilation surface: `Analysis`. Individual phase
actors remain importable where their immutable data types are part of an answer, but tools should
not assemble a second compiler by invoking phases directly. Reusable project discovery, file-backed
source resolution, source-entry identity, target selection, and inspector projections also live
here because the CLI, language server, and editor applications share them.

Every supported actor namespace at the package root has the same explicit subpath. Prefer the
subpath when one actor is the dependency of a module; the root remains available for compact
embedding entry points. For example, match identities and deterministic static values are stable
public compiler facts:

```ts
import * as Match from '@silklang/compiler/Match'
import * as StaticValue from '@silklang/compiler/StaticValue'
```

A small set of operational host entry points remain subpath-only and are not actor namespaces at
the root. Compiler-internal transition and realization modules are not part of either public
surface.

```ts
import { Analysis } from '@silklang/compiler'
import * as Effect from 'effect/Effect'

const program = Effect.gen(function* () {
  const frontend = yield* Analysis.ofSource(
    'memory/example',
    new TextEncoder().encode(`pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(42) }`),
  )
  const snapshot = Analysis.realize(frontend)

  console.log(Analysis.diagnostics(snapshot)) // []
  const artifact = yield* Analysis.codegen(snapshot, { mode: 'release' })
  console.log(artifact._tag) // LlvmBitcodeArtifact
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
7. LLVM bitcode for native or WebAssembly targets.

`Analysis.make` and `Analysis.ofSource` stop after the resilient frontend. Missing or damaged source
becomes queryable diagnostics while unrelated facts remain available. `Analysis.realize` derives a
new immutable runtime snapshot. Code generation rejects snapshots with source errors before
invoking the LLVM backend or toolchain.

Native acceptance tests compare real process outcomes with independently pinned expectations.
Native builds and WebAssembly builds both use deterministic LLVM bitcode and the pinned LLVM
toolchain; compile-time execution remains isolated in `StaticEvaluation`.

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
owned absolute `string` root. Its compiler boundary is limited to unsafe native-only handle
intrinsics. LLVM-to-WebAssembly receives no implicit imports or filesystem ABI.

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
models belong to `@silklang/docgen` so ordinary compilation does not pay that cost.

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

## Native final-artifact cache eligibility

Native final-artifact lookup and publication require complete accounting of every link-affecting
input, including selected tool contents, implicit platform inputs, and resolved named libraries.
The current toolchain model cannot establish that completeness. Native requests therefore perform
the requested link or archive operation and report its actual result, ignoring existing final
cache entries and publishing none. Explicit object/archive hashing and ordered input encoding
remain available, but a key alone does not establish eligibility. Command spelling and version
text do not establish complete tool identity, and unresolved inputs are not reproducible inputs.

Backend-emission reuse, runtime-object caching, and the existing LLVM-to-WebAssembly cache policy
remain independent of this native final-artifact admission rule.
