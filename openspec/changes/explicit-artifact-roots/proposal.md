## Why

JUL-125 needs selected source runtimes, private retention roots, and logical native dependencies without treating every public declaration as an artifact root. The current executable/library switch cannot describe these compositions or a durable relocatable object.

## What Changes

- Separate durable native artifact form from emission stage, application/runtime composition, and loader-entry policy.
- Resolve default/named/none runtime requests from explicit build descriptors, bind `import Intrinsic.application` to the application module, and admit explicit private retention roots.
- Add sealed native requirement clauses on foreign declarations and selected modules, plus artifact requirements in typed build configuration.
- Collect scoped requirements, merge compatible hard constraints with all origins, and expose a deterministic logical plan before physical supply resolution.
- Retain selected roots through optimization and verify no-runtime objects, archives, modules and custom source compositions on all three native targets.
- **BREAKING**: replace executable/library instance-root policy and migrate project, driver, analysis, cache and tooling consumers together.

## Capabilities

### New Capabilities

- `artifact-root-planning`: explicit application/runtime/retention roots, loader policy, scoped logical requirements and plan identity.

### Modified Capabilities

None. Existing hosted startup implementation remains owned by JUL-130; supply discovery by JUL-126 and implicit helper/libm migration by JUL-127.

## Impact

Project/driver configuration, selected closure discovery, parser/formatter, instance discovery, MIR/native retention, artifact emission/linking, caches, plan inspection, diagnostics and reference documentation. No SDK discovery, arbitrary source linker flags, source-owned hosted startup migration, or LTO admission is included.
