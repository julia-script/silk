---
'@silklang/compiler': minor
'@silklang/docgen': patch
---

Make zero-root projects unrepresentable. `ModuleClosure.ProjectRequest.roots`,
`ProjectAnalysis.make` and `ProjectAnalysis.revise` now require a non-empty root list, and the
`ModuleClosureError` reason `EmptyRoots` is removed. `ModuleClosure.ProjectClosure.rootModules`,
`ProjectAnalysis.roots` and the standard-library `manifest` are typed non-empty, and
`ProjectAnalysis.primary` records a view of the shared project facts rooted at the first canonical
root. Documentation's `Project.fromProjectAnalysis` no longer throws, and
`SourceCatalog.fromProject` reads the recorded view instead of probing for one.
