# Chart the Silk Effect bootstrap language

Status: complete

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

## Decisions

- [Choose the ownership, lifetime, and scoped-allocation model](issues/01-ownership-lifetimes-and-scoped-allocation.md)
  — Use affine whole-value ownership, lexical shared/exclusive borrows, self-contained allocation
  owners, and synchronous infallible Drop, without named lifetime scopes or provider-dependent
  results in the bootstrap milestone.
- [Define the bootstrap type system and value model](issues/02-bootstrap-type-system-and-values.md)
  — Use nominal structs, normalized structural unions, finite monomorphized generics, actor-module
  functions with type-owned conformance witnesses, lexical slices, typed unsafe pointers, and
  mode-aware exhaustive matching without general subtyping, methods, overloads, or truthiness.
- [Define function contracts, services, and failure propagation](issues/03-function-contracts-services-and-failures.md)
  — Use eager ordinary functions plus lazy typed Effect expressions and functions, owned abortive typed failures,
  selective row-subtracting handlers, access-and-role-qualified nominal service requirements,
  Effect specialization with captured or per-run providers, contract-row-polymorphic callbacks,
  witness-table service dispatch, and explicit tagged failure returns with a closed native entry.
- [Define modules, visibility, imports, and name resolution](issues/04-modules-visibility-and-name-resolution.md)
  — Use inert one-file modules with path-derived identity, explicit namespace or selective imports,
  flat non-shadowing name resolution, private-by-default visibility, type-owned conformances,
  declaration-resolved cyclic imports, and no runtime module initialization.
- [Determine the practical LLVM native target matrix](issues/05-research-llvm-native-target-matrix.md)
  — Require stage-2 self-hosting on Arm64 macOS, x86-64 Linux, and Arm64 Linux; defer Windows and
  WebAssembly hosts while smoke-testing their object emission.
- [Design the bootstrap compiler pipeline and intermediate representations](issues/06-bootstrap-compiler-pipeline.md)
  — Use a tooling-resilient batch pipeline with lossless syntax, generic semantic HIR, monomorphic
  backend-neutral MIR, generic ownership checking, root-driven emission, swappable backend and
  linker services, direct LLVM bitcode plus external Clang, native line debugging, and measurable
  determinism and performance gates.
- [Define the minimum runtime and standard library](issues/07-minimum-runtime-and-standard-library.md)
  — Use role-qualified self-contained allocation with a system provider, nominal bytes/text/path
  values, vectors and deterministic hash collections, a closed constant subset, four narrow host
  services over a private caller-buffer C shim, pure diagnostic rendering, and a closed native entry
  while deferring arenas with escaping outputs, concurrency, networking, general FFI, streaming I/O,
  and broader library families.
- [Prototype the bootstrap language syntax](issues/08-prototype-bootstrap-syntax.md)
  — Use `effect {}`, `effect fn`, `run`, and explicit `return`; compact `!` failure and `?`
  requirement rows; qualified data-first actor operations with built-in pipe insertion; Effect
  specialization, flattening, and per-run providers; capture-derived reuse; strict non-escape; and
  guarded recursive effects that lower without universal interpreter overhead.
- [Define the staged self-hosting build and acceptance procedure](issues/09-self-hosting-build-and-acceptance.md)
  — Use a content-addressed stage-0-to-stage-2 build with a native fixed-point rebuild, hermetic
  recipes and explicit trust boundaries, traceable cross-stage conformance, byte-identical release
  artifacts, three native hosts, exact ABI/debug/performance/resource gates, and atomic promotion.

## Result

The bootstrap-language decision map is complete and ready to be decomposed into staged OpenSpec
changes. Each implementation change must preserve the accepted dependencies between these issues
rather than reopening them implicitly.

## Out of scope

- Effect integration across WebAssembly or native FFI; it is a later convenience, not the language's
  purpose or a bootstrap milestone requirement.
- Concurrency, atomics, async scheduling, networking, and a user-facing FFI.
- Package registries, dependency solving, a production build system, and a full language server.
- Implementing a direct WebAssembly backend; the bootstrap architecture should merely avoid an
  unnecessary LLVM lock-in where doing so is cheap and clear.
- Built-in shared ownership, stored borrows, named scopes, and owned-region aggregates; the bootstrap
  compiler should use affine owned collections and stable identifiers instead.
