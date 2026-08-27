---
'@silk-lang/wasm': patch
---

Add the bulk instruction families to `@silk-lang/wasm`: fixed-width SIMD and relaxed SIMD
(`v128.const`, shuffles, lane operations, and the full `0xFD` family), threads (shared memories
and the `0xFE` atomic family with exact-alignment validation), and memory64 (64-bit addressed
memories and tables threaded through validation, limits, and both emitters). Baseline modules
emit byte-identical output; the pinned oracle validates and round-trips an exhaustive
per-opcode fixture.
