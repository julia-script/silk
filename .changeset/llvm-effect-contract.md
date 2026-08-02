---
'@silk-effect/llvm': major
---

Replace the generic `SilkError` export with the LLVM-specific `LlvmError` contract, make expected
validation and state-transition failures typed, guarantee scoped cleanup for function-body builds,
and add data-first and pipeable immutable flag transformations.
