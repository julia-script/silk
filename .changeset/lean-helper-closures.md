---
'@silklang/compiler': patch
---

The `memcpy`, `memmove`, `memcmp` and `bcmp` compiler-support sources no longer import `silk.i32`, so
an uncached native build realizes them over `silk.pointer` and `silk.option` alone instead of
re-elaborating the numeric, format and allocator modules. Their executable code is unchanged; debug
source offsets shift.
