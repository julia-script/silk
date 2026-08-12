# @silk-effect/compiler-cli

## 1.0.0

### Major Changes

- bdcf065: Replace the single `silk compile` workflow with the project-oriented `build`, `check`, and `run`
  commands backed by discoverable `silk.toml` manifests and deterministic artifact paths. Rename the
  direct-file escape hatch to `build-exe`, add project, planning, workflow, and process actors as
  public exports, and add `smol-toml` for strict manifest decoding.

### Minor Changes

- 72ab550: Add the `silk` command line interface built on Effect's CLI primitives. `silk compile` runs the
  whole driver pipeline over a rooted module graph and emits a native executable, with source-root,
  target, profile,
  pinned Clang, save-temps, and per-phase timing flags. Diagnostics are rendered with file, line,
  and column for every loaded module. Source rejection exits `1`; operational source-resolution
  failure exits `2`, and neither failure commits an artifact.
- dac7519: Add raw `///` and `//!` documentation source facts, lazy CommonMark documentation models,
  formatter-neutral experimental JSON generation through `silk doc`, rich editor highlighting, and
  signature-plus-documentation hover at both declarations and references.

### Patch Changes

- Updated dependencies [ba6feaf]
- Updated dependencies [a833de9]
- Updated dependencies [4401f57]
- Updated dependencies [f6a5065]
- Updated dependencies [85a554c]
- Updated dependencies [acf5ffb]
- Updated dependencies [73d140b]
- Updated dependencies [510d841]
- Updated dependencies [5a2a409]
- Updated dependencies [c6ce42b]
- Updated dependencies [bf43d61]
- Updated dependencies [5b1a75d]
- Updated dependencies [bdcf065]
- Updated dependencies [dac7519]
- Updated dependencies [0b44301]
- Updated dependencies [f8c0803]
- Updated dependencies [373c4d8]
- Updated dependencies [bb74192]
- Updated dependencies [c7151ca]
- Updated dependencies [09a0b73]
- Updated dependencies [2260aa5]
  - @silk-effect/compiler@1.0.0
  - @silk-effect/documentation@0.1.0
