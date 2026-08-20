## Why

A compiler, standard-library catalog, intrinsic inventory, and target runtime can currently be assembled without one authoritative compatibility check. Mismatches then surface as missing declarations, bad lowering, or operational failures far from the owning boundary.

## What Changes

- Give compiler, catalog, intrinsic inventory, runtime support, and target-provider artifacts stable identities and content digests.
- Validate the matched set before analysis, emission, or execution uses it.
- Classify missing source, malformed distribution, unsupported target, unresolved entry, and operational runtime failure at their owning boundary.
- Keep target availability derived from reachable intrinsics and distinguish an incompatible toolchain from a valid program unsupported on one target.
- Expose deterministic structured diagnostics to CLI, embeddings, and language tooling.

## Capabilities

### Modified Capabilities

- `bootstrap-compiler-driver`: validate and retain matched toolchain identities before compilation.
- `bootstrap-silk-stdlib`: publish catalog and source digests with layer metadata.
- `bootstrap-intrinsic-boundary`: publish the sealed intrinsic inventory identity.
- `bootstrap-native-toolchain`: verify runtime and target-provider compatibility.
- `silk-cli-workflows`: report configuration, target, entry, and operational failures distinctly.
- `bootstrap-diagnostics`: encode deterministic integrity failure data and provenance.

## Impact

Depends on `align-entry-termination-reporting`, `make-modules-catalogs-and-imports-explicit`, and `add-source-unsafe-callable-contracts`. It affects distribution generation, compiler startup, target planning, CLI/embedding outcomes, diagnostics, and tests. It adds no network update mechanism, package manager, or backward-compatibility negotiation.
