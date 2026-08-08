# Tasks — add-silk-vector-and-scanner

## 1. Parametric conformance syntax

- [x] 1.1 Parse an optional contextual type-parameter list on `impl` declarations with local recovery; extend Parser tests for lossless round-trip and malformed-list recovery
- [x] 1.2 Format parametric conformances canonically; extend Formatter goldens
- [x] 1.3 Extend TextMate/CodeMirror highlighting for the impl parameter list; update language tests and the VS Code grammar

## 2. Parametric conformance semantics

- [x] 2.1 Index parametric conformances with bound parameter lists; validate Drop hook shape with parameters in scope; defer the Copy prohibition to instantiation for parametric impls only
- [x] 2.2 Reject unbound, duplicate, and undeclared impl parameters with deterministic diagnostics; add DeclarationIndex/Elaboration tests for each rejection and for parameter binding across hook signatures
- [x] 2.3 Substitute concrete arguments into parametric conformances during instance discovery so each reachable instantiation yields one normalized concrete witness or hook instance; add Instances tests covering two element types under one impl
- [x] 2.4 Fire the Copy-cannot-Drop rejection at instantiation with the instantiated type in the message; test with an element type that makes the provider Copy
- [x] 2.5 Verify witness dispatch and Drop cleanup through a parametric conformance execute identically to a hand-written concrete conformance on all three engines
- [x] 2.6 Elaborate Drop hook bodies as ordinary functions with canonical identities and the impl's type parameters in scope (discovered gap: hook bodies were never elaborated, so hooks never executed despite the archived spec requiring it)
- [x] 2.7 Add a hook-bearing cleanup plan variant in Ownership that runs the hook before field cleanup
- [x] 2.8 Discover reachable Drop hooks in Instances and plan their layouts and calling shapes
- [x] 2.9 Execute hook-bearing cleanup in the evaluator, LLVM, and Wasm, including run-propagation releases
- [x] 2.10 Pin hook-before-fields, exactly-once, on fallthrough, typed failure, and early drop across all three engines

## 3. Standard-library mechanism

- [x] 3.1 Embed stdlib `.silk` sources in the compiler package and resolve their imports through module closure with a reserved canonical namespace
- [x] 3.2 Report a deterministic collision diagnostic when a user module shadows a stdlib identity
- [x] 3.3 Gate stdlib-importing artifacts through fresh-process determinism; extend Driver tests
- [x] 3.4 Confirm stdlib source compiles with zero privilege: an injected library defect surfaces as an ordinary diagnostic attributed to the library module (test with a temporarily broken fixture copy)

## 4. Vector in Silk

- [x] 4.1 Add a compiler-known place-swap operation (read old value, write replacement, one statement, place stays initialized) through elaboration, ownership, and lowering onto existing ReadPlace/WritePlace ops; delta-spec it
- [x] 4.2 Add unsafe non-consuming Slot.copy for Copy element types through all three engines, rejected at instantiation for non-Copy elements; delta-spec it
- [x] 4.3 Add whole-member match bindings (`Member name => ...`) through pattern syntax, match analysis, ownership, and all three engines, so union members and `Layout | LayoutOverflow` results are extractable; delta-spec it (discovered gap: runtime-count allocation is otherwise inexpressible)
- [x] 4.4 Write the vector stdlib module: empty construction without allocation, append with geometric growth and commit-after-success, checked get, length, capacity, parametric Drop destroying initialized elements before the buffer
- [x] 4.5 Prove growth atomicity: acceptance test where injected failure at the replacement-allocation ordinal leaves the original vector observable and leak-free
- [x] 4.6 Prove release order: Drop-bearing element type records destruction order; elements before buffer, uninitialized capacity untouched
- [x] 4.7 Prove ownership transfer and early drop across all three engines
- [x] 4.8 Verify no vector-shaped operation appears in MIR, evaluator traces, or backend output for a vector-using program

## 5. Scanner acceptance

- [x] 5.1 Write the scanner acceptance program: borrow `&[U8]` source, return owned `Vector<Token>`, sized to force at least two reallocations
- [x] 5.2 Differential gate: identical token sequences and exit values in evaluator, native, and Wasm
- [x] 5.3 Failure-ordinal sweep over every scanner allocation: typed `OutOfMemory`, rollback, zero leaks at each ordinal (reuse the dispatch-change sweep harness)
- [x] 5.4 Fresh-process determinism over the scanner and its stdlib imports

## 6. Inspector

- [x] 6.1 Add `/labs` presets: growing append, failed growth with preserved vector, destruction order, early drop, and the scanner program (source identical to the tested acceptance program)
- [x] 6.2 Extend labs projections for any new canonical fact forms (parametric conformance facts); update presets tests

## 7. Gates and closure

- [ ] 7.1 `pnpm typecheck`, `pnpm test`, `pnpm lint` clean at repo root
- [ ] 7.2 `openspec validate add-silk-vector-and-scanner --strict` passes
- [ ] 7.3 Update `roadmaps/project.md`: move this item to Now/complete with evidence, promote the next item
- [ ] 7.4 Sync delta specs and archive the change
