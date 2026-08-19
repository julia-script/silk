## 1. Unsafe callable contracts

- [ ] 1.1 Parse and format `unsafe fn` and `unsafe effect fn` declarations.
- [ ] 1.2 Add unsafe qualification to resolved callable contracts, semantic facts, and deterministic encodings.
- [ ] 1.3 Require lexical acknowledgement on invocation while preserving all ordinary checks.
- [ ] 1.4 Share acknowledgement handling and diagnostics with sealed intrinsic calls.

## 2. First-class propagation and conformance

- [ ] 2.1 Preserve qualification through callable values, generic substitution, returns, storage, and HIR/MIR.
- [ ] 2.2 Preserve qualification through generalized partial application and staged sections.
- [ ] 2.3 Enforce safe/unsafe implementation compatibility for interface and service operations.
- [ ] 2.4 Add language-service hover, signature, completion, and semantic-highlighting support.

## 3. Verification

- [ ] 3.1 Add ordinary/effectful calls, acknowledgement, missing acknowledgement, values, sections, generics, storage, and conformance tests.
- [ ] 3.2 Prove ownership, Effect, requirement, target, and cleanup diagnostics remain active inside unsafe contexts.
- [ ] 3.3 Update canonical specs, language docs, diagnostics, and low-level stdlib examples.
- [ ] 3.4 Run typecheck, Biome, full tests, native acceptance only where low-level target behavior is involved, and `pnpm check`.
