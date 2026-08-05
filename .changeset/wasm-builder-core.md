---
'@silk-effect/wasm': patch
---

Introduce `@silk-effect/wasm`: Effect-native WebAssembly module construction with instructions as
plain data, handle-based index spaces resolved at emission, full specification validation at
define and emit time, and deterministic `.wat` text and `.wasm` binary output verified against a
pinned `wasm-tools` oracle. Baseline: core 2.0 plus tail calls, extended constant expressions,
and multiple memories.
