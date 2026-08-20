## 1. Ordinary text values

- [x] 1.1 Remove `SEM0094` text compatibility exceptions and resolve string values through ordinary type facts.
- [x] 1.2 Route string references and byte views through generalized provenance and lexical borrowing.
- [x] 1.3 Preserve distinct text and binary logical types in semantic facts, HIR/MIR, debug metadata, and presentation.

## 2. Scalar traversal

- [x] 2.1 Add or expose the minimal checked scalar conversion required to construct valid `char` values.
- [x] 2.2 Rewrite canonical UTF-8 traversal to use ordinary byte views and checked conversion.
- [x] 2.3 Align literal inference, length, indexing, equality, evaluation, LLVM, and Wasm.

## 3. Verification

- [x] 3.1 Add ASCII/non-ASCII traversal, invalid scalar, borrow lifetime, binary distinction, call, return, and equality tests.
- [x] 3.2 Add cross-engine and debug-presentation coverage at the appropriate tiers.
- [x] 3.3 Update stdlib source/generated artifacts, diagnostics, canonical specs, and language docs.
- [x] 3.4 Run typecheck, Biome, full tests, native acceptance where metadata is target-specific, and `pnpm check`.
