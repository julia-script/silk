---
'@silklang/compiler': minor
'@silklang/cli': minor
'@silklang/lsp': minor
---

Remove the provisional runtime evaluator, evaluator host and inspection APIs, and independent
direct-WebAssembly backend. Runtime execution now uses LLVM-native artifacts or LLVM-to-Wasm,
while compile-time `StaticEvaluation` remains a separate compiler facility. Remove the obsolete
backend registry, manifest selector, and CLI `--backend` option now that LLVM is the sole runtime
implementation family. LSP inspection no longer exposes evaluator actions or evaluation parameters.
