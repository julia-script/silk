## Why

JUL-120 requires packages to expose typed compile-time configuration and permits different artifacts on the same target to coexist without sharing configuration-dependent results. Target ordinals and separate backend settings cannot express that contract.

## What Changes

- Introduce versioned machine descriptions and one immutable logical compilation profile.
- Add unconditional package-owned parameter declarations, ordinary Silk defaults and validation, typed external bindings, deterministic bootstrap dependency resolution, and origin-bearing diagnostics.
- Propagate canonical profile identity through static evaluation, compilation and tooling.
- **BREAKING**: remove `Intrinsic.targetProfile`, source `Profile`, ordinal dispatch, and all superseded profile-owned paths; update consumers and documentation together.
- Specify F1 of the Target/source foundation milestone under the finalized Native OS Integration Plan, D-014, D-021 and SPEC-01. Conditional source closure, full ABI, resolved artifact roots and physical supplies remain in JUL-121/123/125/126.

## Capabilities

### New Capabilities

- `compilation-profiles`: logical domains, canonical identity, validation, tooling selection, and primitive fact evidence.
- `package-static-configuration`: unconditional source schemas, bindings, bootstrap, admitted values, precedence, privacy, and diagnostics.

### Modified Capabilities

- `static-evaluation`: canonical profile identity replaces ordinal target identity.
- `bootstrap-target-layout`: canonical targets are machine descriptions within complete profiles; host selection belongs at the application edge.
- `silk-project-manifest`: named profiles and typed binding transport.
- `silk-cli-workflows`: shared logical selectors and explicit optimization shorthand.
- `bootstrap-native-toolchain`: completed logical optimization/debug facts own object arguments.

## Impact

Compiler target, declaration, static evaluation, analysis, module closure, driver, project and diagnostics actors; CLI and language-server profile selection; standard-library target wrappers; generated intrinsic catalogs; prescriptive reference; tests and examples. No compatibility adapters or new external dependencies are intended.
