## Why

The owned-allocation substrate shipped, but nothing yet proves it can carry a *useful* owned
sequence. Raw allocation only demonstrates the boundary; the bootstrap language still cannot express
runtime-sized owned output, which is the shape every real compiler pass produces. The deliberate
handoff in `add-self-contained-owned-allocation` was to leave `Vector<T>` and the scanner proof to
this change, so the substrate's claim — that collection policy belongs in Silk rather than the
compiler — gets tested by an actual collection and an actual pass.

Two prerequisites are missing and were confirmed by probing the current compiler rather than assumed:

- Parametric conformances do not exist. `impl<T> Drop for Vector<T>` fails at the parser
  (`PAR0001: Expected ForKeyword`) and at semantic analysis (`SEM0083: the capability and provider
  must resolve to concrete nominal types`). Concrete instantiations like `impl Drop for Vector<I32>`
  already work, as do generic structs, generic fields, and `RawBuffer.from<T>` under a type
  parameter. Without a parametric form, a generic owned sequence would need one hand-written `Drop`
  per element type — which is not a standard library.
- There is no standard-library distribution mechanism. Every `.silk` file in the repository is a
  test fixture; no prelude, manifest, or implicit module set exists. `Vector<T>` cannot be "in the
  standard library" until some module is reachable without being pasted into user source.

## What Changes

- Add parametric conformances: `impl<T> Cap for Type<T>`, with type parameters bound across the
  conformance so one declaration serves every instantiation. Monomorphization discovers concrete
  `Drop` hooks and operations through the existing finite worklist.
- Add a minimal standard-library module set that user code can reach without vendoring source, so
  `Vector<T>` lives somewhere real. Scope is the smallest thing that makes the sequence importable —
  not a general package ecosystem.
- Implement `Vector<T>` entirely in Silk over `Allocator`, `Allocation`, `RawBuffer<T>`, `Slot<T>`,
  and restricted `Drop`: create, append with geometric growth, read by checked index, length and
  capacity, move-out, and deterministic release. No compiler-known vector behavior and no MIR,
  evaluator, or backend primitive named after a collection.
- Make growth correct under failure: allocate the replacement buffer, move exactly the initialized
  elements, commit only after success, then drop the old buffer. A failed growth leaves the original
  vector with its prior elements and capacity intact and leaks nothing.
- Prove the whole stack with a scanner written in Silk that borrows runtime-sized source bytes as
  `&[U8]` and returns an owned `Vector<Token>`, exercising growth across at least one reallocation,
  typed `OutOfMemory` failure, partial-initialization rollback, early `drop`, and deterministic
  cleanup identically in the evaluator, LLVM, and direct Wasm.
- Keep the no-privilege boundary: no iterable/iterator abstraction is required, no bulk byte-memory
  primitive is added unless this workload demonstrates a concrete need, and no compiler phase
  branches on `Vector`.

## Capabilities

### New Capabilities

- `bootstrap-silk-stdlib`: A reachable standard-library module set — resolution, module identity, and
  determinism for library code that ships with the compiler rather than with user source.
- `bootstrap-owned-sequence`: The `Vector<T>` contract as ordinary Silk — growth, failure atomicity,
  checked access, ownership transfer, and deterministic release, with no compiler-known behavior.

### Modified Capabilities

- `bootstrap-instances`: Discover conformance witnesses, Drop hooks, and operations through
  parametric conformances, binding conformance type parameters during monomorphization.
- `bootstrap-syntax`: Parse and losslessly reproduce a type-parameter list on `impl` declarations,
  with recovery that keeps a malformed parametric conformance from cascading.
- `bootstrap-semantic-facts`: Publish conformance facts that carry bound type parameters, and reject
  ill-formed parametric conformances (unbound, unused, or duplicated parameters) with precise causes.
- `bootstrap-declaration-index`: Index parametric conformances and validate restricted `Drop` hooks
  whose self type is a generic instantiation.
- `bootstrap-compiler-driver`: Gate the sequence and scanner through differential execution across
  all three engines, fresh-process determinism, and failure-ordinal sweeps over growth and rollback.
- `bootstrap-syntax-inspector`: Add scanner and vector presets — growth, exhaustion, rollback, and
  early drop — to the unified `/labs` workbench.

## Impact

Adds a type-parameter list to `impl` syntax and threads bound conformance parameters through the
declaration index, semantic facts, and monomorphization; existing non-parametric conformances keep
their meaning and encoding. Introduces the first non-fixture Silk source shipped with the compiler,
which adds a module-resolution surface and new determinism obligations. Adds no new MIR operation,
evaluator value, or backend intrinsic: `Vector<T>` and the scanner are expected to compile through
the substrate as it stands, and any gap found there is a finding about the substrate rather than a
license to add a collection primitive. Artifact encodings and goldens gain parametric-conformance
forms.
