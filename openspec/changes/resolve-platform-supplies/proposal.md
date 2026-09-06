## Why

Native finalization currently delegates physical input discovery to Clang after logical planning.
The compiler cannot explain or validate the resulting SDK, glibc, CRT, interpreter, or transitive
link inputs; JUL-148 therefore correctly excludes native final artifacts from cache admission.
JUL-126 establishes the immutable supply boundary required by the native OS integration plan.

## What Changes

- **BREAKING**: replace ambient native finalization with explicit, native, and automatic supply
  requests, deterministic pin precedence, and immutable tool/component provenance. Managed supplies
  are recognized and rejected; this change never installs anything.
- Resolve Darwin SDK/libSystem/framework and GNU glibc/CRT/interpreter capabilities separately;
  reject incompatible targets, deployments, missing components, and accidental installation mixes.
- Freeze selected driver/linker identities and environment, resolve ordered named and recursive
  inputs, then execute the concrete final link command. Retain linker archive/weak/import semantics.
- Feed complete content accounting into the existing final-cache admission rule. Runtime C objects
  account for their actual consumed headers; compiler semantic/object identities stay path independent.
- Expose supplies and plans through compiler/build/CLI inspection and add required pinned native
  conformance lanes for Darwin ARM64 and GNU/Linux x86-64 and ARM64.

## Capabilities

### New Capabilities

- `platform-supplies`: typed supply selection, component compatibility, frozen provenance, physical
  link closure, stage identities, diagnostics, and required conformance.

### Modified Capabilities

- `bootstrap-native-toolchain`: finalization consumes resolved inputs and commands; native final
  caching requires complete input accounting, and C object caching accounts for consumed headers.

## Impact

Compiler Node toolchain boundary, pure public supply/plan actors, Driver, project/build configuration,
CLI reporting and inspection, package exports, native CI, tests, and prescriptive reference.
Logical source selection, standard-library OS operations, hosted/raw startup, helper implementations,
managed downloads, and additional libc families remain outside this change.
