## 1. Specification

- [x] 1.1 Define API, failure precedence, bounds and assurance; verify strict OpenSpec validation.

## 2. Implementation

- [x] 2.1 Implement private ChaCha20/Poly1305 and public seal/open; verify component and composed known answers plus distinct rejection boundaries.
- [x] 2.2 Register the module and document all public items; generate and verify API documentation and executable examples.
- [x] 2.3 Add shared native acceptance and small LLVM-to-Wasm witness with fixture provenance; verify deterministic output, unchanged destinations and independent oracle comparison.

## 3. Verification

- [x] 3.1 Inspect native debug/optimized and Wasm generated output; record secret-work observations and assurance limits.
- [ ] 3.2 Run required repository checks and release candidate; obtain independent correctness and exact-diff test-economics approval with measured costs.
- [ ] 3.3 Commit approved issue changes and provide the coordinator evidence for draft-PR handoff.
