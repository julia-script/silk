---
'@silklang/compiler': minor
'@silklang/docgen': patch
---

Make zero-root project analyses unrepresentable. `ProjectAnalysis.make` and `ProjectAnalysis.revise`
now require a non-empty root list; `ProjectAnalysis.roots`, `ModuleClosure.ProjectClosure.rootModules`
and the standard-library `manifest` are typed non-empty; and `ProjectAnalysis.primary` records the
first root's view. Documentation's `Project.fromProjectAnalysis` no longer throws, and
`SourceCatalog.fromProject` reads the recorded view instead of probing for one.
