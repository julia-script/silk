## Why

The tutorial must behave like an npm consumer guide even before the package is published. A reproducible consumer-shaped scaffold is needed so subsequent lessons are tested against public package boundaries rather than repository internals.

## What Changes

- Add the consumer-project setup lesson and starter scaffold.
- Document future npm installation commands and the pre-release packed-package substitution.
- Verify Node, pnpm, and Clang prerequisites.
- Render an empty module through public `Builder` and `IrText` subpaths.
- Provide recovery guidance for package resolution, internal imports, and missing native tools.

## Capabilities

### New Capabilities

None. This change adds documentation and tutorial-example material, so the change opts out of behavioral specs with `skip_specs: true`.

### Modified Capabilities

None.

## Impact

Adds a consumer-shaped example project, setup documentation, and a smoke check. Validation tooling will need to install the packed local package; the LLVM package API remains unchanged.
