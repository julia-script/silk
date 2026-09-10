## Why

JUL-121 needs platform façades to select declarations and imports before ordinary semantic analysis. Statement selection cannot prevent loading incompatible modules or publish mutually exclusive declarations under one name.

## What Changes

- Add module `static if` declaration groups, with ordinary static Silk conditions and explicit selective import publication.
- Resolve condition dependencies on demand after unconditional package configuration bootstrap. Parse all loaded branches; admit only selected declarations and imports.
- Expose profile-specific selected surfaces, condition dependencies and inactive source ranges through generic compiler analysis.
- **BREAKING**: replace unconditional closure/index assumptions and the statement-only static-selection restriction, migrating all affected consumers and reference documentation.

## Capabilities

### New Capabilities

- `module-static-selection`: declaration selection, dependency demand, availability cycles, selective publication and profile-specific semantic identity.

### Modified Capabilities

- `bootstrap-name-resolution`: allow explicit selective publication while keeping ordinary imports private.
- `static-evaluation`: admit module declaration selection while retaining ordinary bounded static evaluation and statement semantics.

## Impact

Parser, syntax/formatting, module closure, declaration collection, name resolution, static evaluation coordination, frontend/project analysis, incremental reuse, generated diagnostics, reference and compiler tests. Consumes merged JUL-120. Full editor presentation, provider migration, native requirements and new artifact roots remain owned by JUL-122/JUL-125.
