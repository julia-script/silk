---
'@silk-lang/wasm': patch
---

Add exception handling and branch hinting to `@silk-lang/wasm`: exception tags as a new
importable/exportable entity kind, the `exnref` reference type, `throw`/`throw_ref`/`try_table`
with all four catch-clause kinds, tag names in the name section, and optional likely/unlikely
hints on `br_if`/`if` emitted as the `metadata.code.branch_hint` custom section and text
annotations. Legacy exception handling remains permanently excluded.
