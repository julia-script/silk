# Tasks

## 1. Object Emission

- [x] 1.1 Add `ObjectEmission.materialize` with explicit toolchain, profile, backend artifact, and build-scope lifetime.
- [x] 1.2 Preserve object inventory, helper capability, command, target, and scope metadata and migrate the driver.

## 2. Linking

- [x] 2.1 Add `Linker.link` over a complete physical plan, destination, artifact kind, scope, and closed cache policy.
- [x] 2.2 Move plan validation, final-cache reuse/write, execution, durable commit, and returned link metadata into the linker.
- [x] 2.3 Keep helper/runtime/native-input preparation outside the linker and delete superseded public entry points.

## 3. Verification

- [x] 3.1 Migrate native toolchain, layout oracle, driver acceptance, cache, cleanup, and failure fixtures.
- [x] 3.2 Update root/subpath exports, operation traces, compiler documentation, and architecture examples.
