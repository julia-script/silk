# Tasks — add-silk-vector-and-scanner

## 1. Parametric conformance syntax

- [x] 1.1 Parse an optional contextual type-parameter list on `impl` declarations with local recovery; extend Parser tests for lossless round-trip and malformed-list recovery
- [x] 1.2 Format parametric conformances canonically; extend Formatter goldens
- [x] 1.3 Extend TextMate/CodeMirror highlighting for the impl parameter list; update language tests and the VS Code grammar

## 2. Parametric conformance semantics

- [ ] 2.1 Index parametric conformances with bound parameter lists; validate Drop hook shape with parameters in scope; defer the Copy prohibition to instantiation for parametric impls only
- [ ] 2.2 Reject unbound, duplicate, and undeclared impl parameters with deterministic diagnostics; add DeclarationIndex/Elaboration tests for each rejection and for parameter binding across hook signatures
- [ ] 2.3 Substitute concrete arguments into parametric conformances during instance discovery so each reachable instantiation yields one normalized concrete witness or hook instance; add Instances tests covering two element types under one impl
- [ ] 2.4 Fire the Copy-cannot-Drop rejection at instantiation with the instantiated type in the message; test with an element type that makes the provider Copy
- [ ] 2.5 Verify witness dispatch and Drop cleanup through a parametric conformance execute identically to a hand-written concrete conformance on all three engines

## 3. Standard-library mechanism

- [ ] 3.1 Embed stdlib `.silk` sources in the compiler package and resolve their imports through module closure with a reserved canonical namespace
- [ ] 3.2 Report a deterministic collision diagnostic when a user module shadows a stdlib identity
- [ ] 3.3 Gate stdlib-importing artifacts through fresh-process determinism; extend Driver tests
- [ ] 3.4 Confirm stdlib source compiles with zero privilege: an injected library defect surfaces as an ordinary diagnostic attributed to the library module (test with a temporarily broken fixture copy)

## 4. Vector in Silk

- [ ] 4.1 Write the vector stdlib module: empty construction without allocation, append with geometric growth and commit-after-success, checked get, length, capacity, parametric Drop destroying initialized elements before the buffer
- [ ] 4.2 Prove growth atomicity: acceptance test where injected failure at the replacement-allocation ordinal leaves the original vector observable and leak-free
- [ ] 4.3 Prove release order: Drop-bearing element type records destruction order; elements before buffer, uninitialized capacity untouched
- [ ] 4.4 Prove ownership transfer and early drop across all three engines
- [ ] 4.5 Verify no vector-shaped operation appears in MIR, evaluator traces, or backend output for a vector-using program

## 5. Scanner acceptance

- [ ] 5.1 Write the scanner acceptance program: borrow `&[U8]` source, return owned `Vector<Token>`, sized to force at least two reallocations
- [ ] 5.2 Differential gate: identical token sequences and exit values in evaluator, native, and Wasm
- [ ] 5.3 Failure-ordinal sweep over every scanner allocation: typed `OutOfMemory`, rollback, zero leaks at each ordinal (reuse the dispatch-change sweep harness)
- [ ] 5.4 Fresh-process determinism over the scanner and its stdlib imports

## 6. Inspector

- [ ] 6.1 Add `/labs` presets: growing append, failed growth with preserved vector, destruction order, early drop, and the scanner program (source identical to the tested acceptance program)
- [ ] 6.2 Extend labs projections for any new canonical fact forms (parametric conformance facts); update presets tests

## 7. Gates and closure

- [ ] 7.1 `pnpm typecheck`, `pnpm test`, `pnpm lint` clean at repo root
- [ ] 7.2 `openspec validate add-silk-vector-and-scanner --strict` passes
- [ ] 7.3 Update `roadmaps/project.md`: move this item to Now/complete with evidence, promote the next item
- [ ] 7.4 Sync delta specs and archive the change
