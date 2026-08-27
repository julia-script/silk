---
'@silk-lang/compiler': major
---

Replace preloaded source maps with an explicit root `SourceFile` and the injectable
`SourceResolver` Effect service. Module closure and analysis construction are now Effectful,
resolver failures remain recoverable tooling facts, and codegen plus the compiler driver reject
invalid frontends before artifact production.
