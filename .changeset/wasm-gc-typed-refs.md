---
'@silklang/wasm': patch
---

Add GC and typed function references to `@silklang/wasm`, completing the Chrome-unflagged
feature surface: parameterized reference types with abstract and concrete heap types, struct
and array types in canonicalized recursive groups with declared supertypes, subtype-aware
validation, the GC/cast/typed-call instruction set, and type names. `ValType.RefType` changes
shape to `{ nullable, heapType }` with the classic shorthands preserved; baseline modules emit
byte-identical output.
