## Why

`CompilationRequest.root` currently carries a `SourceFile`, so callers must load the entry while the front end loads its imports. Root selection should identify a module; the same replaceable resolver should supply bytes and origin for every module in the compilation.

## What Changes

- **BREAKING** Change `CompilationRequest.root` to a canonical module string, and project request roots to canonical module strings.
- Resolve application, project, and composition roots inside the front end through the same namespace policy and resolution cache as imports. Remove caller-supplied root seeding and duplicate reserved-root verification reads.
- Distinguish missing requested roots from operational resolver failures without inventing source spans or publishing a successful single-root snapshot without its root.
- Update CLI entry preparation, analysis conveniences, editor overlays, tests, scripts, and documentation to supply root bytes through resolvers.
- Preserve canonical ordering, source origins, syntax reuse, selected-import discovery, and partial recovery for failed imports.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-module-closure`: Resolve named roots through the source resolver and define unavailable-root behavior.
- `bootstrap-source-resolution`: Apply replaceable resolution and per-load outcome sharing to roots as well as imports.
- `bootstrap-silk-stdlib`: Replace the obsolete caller-supplied root collision diagnostic with exclusive toolchain resolution.
- `bootstrap-project-analysis`: Accept root identities and obtain current revision bytes through the resolver.

## Impact

Compiler request and frontend APIs (`ModuleClosure`, `Frontend`, `ModuleSelection`, `Analysis`, `ProjectAnalysis`, `Driver`, and component realization), CLI entry selection and workflows, LSP workspace analysis, documentation generation, in-memory/browser clients, fixtures, and compiler reference documentation. This is a coordinated breaking change with no source-valued request compatibility path. No new external dependency or Silk syntax is required.
