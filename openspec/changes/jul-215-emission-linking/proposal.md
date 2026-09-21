# Why

Backend emission is already MIR-only, but native object materialization, cache reuse, plan
validation, and final linking are exposed as low-level `NativeToolchain` calls inside the driver.
Their artifact and lifetime boundaries should be independently callable.

# What Changes

- Add `ObjectEmission.materialize` over a backend artifact, resolved toolchain, profile, and
  caller-owned build scope.
- Add `Linker.link` over a complete physical link plan, build scope, destination, artifact kind,
  and explicit cache policy.
- Return link-plan/cache metadata with the final artifact.
- Keep helper/runtime/source preparation outside the linker.
- Remove the superseded `NativeToolchain.emitObject` and `NativeFinalizer` entry points.

# Capabilities

## New Capabilities

- `object-emission`: native object materialization with inventory/helper metadata.
- `artifact-linking`: plan-driven final linking with explicit lifetime and cache policy.

# Impact

Driver orchestration, native toolchain internals, package exports, native acceptance fixtures,
phase traces, compiler docs, and the architecture reference.
