# @silk-effect/compiler

## 1.0.0

### Major Changes

- bdcf065: Replace preloaded source maps with an explicit root `SourceFile` and the injectable
  `SourceResolver` Effect service. Module closure and analysis construction are now Effectful,
  resolver failures remain recoverable tooling facts, and codegen plus the compiler driver reject
  invalid frontends before artifact production.

### Minor Changes

- ba6feaf: Add the first lossless concrete syntax tree, deterministic parser diagnostics, and recoverable
  parser for one public bootstrap function.
- a833de9: Replace singular semantic fields with ordered function facts, closed declaration-name lookup, and
  duplicate-name diagnostics for every parsed bootstrap declaration.
- 4401f57: Add the root-confined native `OsFileSystem` provider, opaque affine OS handles, injected evaluator
  host protocol, and reachable-only native filesystem runtime.
- f6a5065: Introduce the bootstrap compiler package with immutable byte-oriented sources, source-owned spans,
  tokens, recoverable lexical diagnostics, and a deterministic lexer for the first Silk syntax.
- 85a554c: Resolve top-level zero-argument calls, propagate declared result types, and diagnose unknown function names.
- acf5ffb: Analyze nested call arguments as recursive semantic expression facts and report a precise temporary
  evaluation boundary when a reachable nested expression is encountered.
- 73d140b: Parse lossless zero-argument call expressions and publish closed integer-or-unresolved-call return facts.
- 510d841: Enforce reachable-only target availability for sealed compiler intrinsics and expose deterministic executable intrinsic inventories.
- 5a2a409: Evaluate nested call expressions left to right with exact recursive results, blocked reasons,
  cycle detection, and deterministic trace provenance.
- c6ce42b: Add a WebAssembly backend satisfying the nominal `Backend` service, emitting structured control
  flow recovered from MIR's branch diamonds, trapping arithmetic checks, and the `name` custom
  section for debug builds.
- bf43d61: Add owned normalized `Path`, allocation-free portable filesystem values and failures, and the
  source-defined seven-operation mutable `FileSystem` service with ordinary recursive and existence
  helpers across evaluator, native LLVM, and direct WebAssembly providers.
- 5b1a75d: Add source-defined portable semantic logging with explicit Logger requirements, complete borrowed
  messages, typed failures, and stdout and deterministic in-memory providers across evaluator,
  native LLVM, and direct WebAssembly execution.
- dac7519: Add raw `///` and `//!` documentation source facts, lazy CommonMark documentation models,
  formatter-neutral experimental JSON generation through `silk doc`, rich editor highlighting, and
  signature-plus-documentation hover at both declarations and references.
- 0b44301: Resolve bootstrap parameter declarations and bare identifier expressions within their owning function, including exact identities, declared types, lookup outcomes, provenance, and deterministic diagnostics.
- f8c0803: Publish ordered call arguments, positional target-parameter mappings, explicit call-contract outcomes, and deterministic wrong-arity diagnostics.
- 373c4d8: Add immutable semantic facts, declaration lookup, and phase-owned diagnostics for the first
  bootstrap function, including exact positive `I32` integer analysis.
- bb74192: Parse recursively nested call arguments as lossless concrete syntax while preserving an explicit
  unavailable semantic boundary until recursive call analysis is added.
- c7151ca: Parse one or more public bootstrap functions as ordered lossless concrete branches while keeping
  semantic analysis explicitly limited to the first declaration.
- 09a0b73: Add closed bootstrap evaluation for reachable `main: I32` programs, with exact results, positional parameter frames, deterministic traces, and bounded recursive-cycle outcomes.
- 2260aa5: Parse typed bootstrap parameters, bare identifier expressions, and integer or identifier call arguments as lossless recoverable syntax.

### Patch Changes

- Updated dependencies [03f1f0a]
- Updated dependencies [d8ad798]
- Updated dependencies [689d5ad]
- Updated dependencies [c3c5b2a]
- Updated dependencies [0cc388f]
  - @silk-effect/llvm@1.0.0
  - @silk-effect/wasm@0.0.1
