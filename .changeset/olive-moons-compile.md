---
'@silklang/compiler': minor
---

Add a WebAssembly backend satisfying the nominal `Backend` service, emitting structured control
flow recovered from MIR's branch diamonds, trapping arithmetic checks, and the `name` custom
section for debug builds.
