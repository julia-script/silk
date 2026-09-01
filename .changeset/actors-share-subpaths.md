---
'@silklang/compiler': major
'@silklang/lsp': major
---

Align the compiler and language-server actor surfaces. Add deep imports for compiler `Match` and
`TargetConstant`, and remove the internal `LocalSharedLifecycle`, `MirNormalization`, and
`WorkspaceCatalog` namespaces from package roots.
