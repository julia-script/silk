## Why

JUL-122 removes the second platform-availability policy in the standard-library manifest and integrity graph. The compiler's selected source surface must also govern documentation and editor candidates for one identified compilation profile.

## What Changes

- **BREAKING**: remove manifest layer/provider-target fields, provider selection indexes, import prohibitions and provider-only integrity components; preserve exact source and intrinsic implementation integrity.
- Select the seven existing native providers through ordinary module static conditions, preserving their existing native operations and keeping them absent from LLVM-to-Wasm surfaces.
- Build profile-identified source catalogs and documentation from canonical compiler selection, including selected public re-exports.
- Complete editor profile changes, stale-result rejection, catalog invalidation and inactive-range presentation using the existing project profile normalizer.
- Define and validate descriptive platform catalog provenance records independently of availability selection and library ABI manifests.

## Capabilities

### New Capabilities

- `selected-source-tooling`: source-owned provider availability and profile-consistent catalogs, documentation and editor results.
- `platform-catalog-provenance`: descriptive authority, declaration evidence, fixture and drift-review records.

### Modified Capabilities

None. The new requirements replace the implementation's manifest policy; existing ordinary-source and canonical-source integrity requirements remain in force.

## Impact

Compiler standard-library sources, embedding/integrity generation, catalog APIs, editor inventory and worker lifecycle, documentation generation, tests and prescriptive reference. No new OS operations, provider defaults, physical supplies or runtime composition are introduced; those remain with their assigned downstream tickets.
