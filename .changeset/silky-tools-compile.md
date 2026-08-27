---
'@silk-lang/compiler-cli': minor
---

Add the `silk` command line interface built on Effect's CLI primitives. `silk compile` runs the
whole driver pipeline over a rooted module graph and emits a native executable, with source-root,
target, profile,
pinned Clang, save-temps, and per-phase timing flags. Diagnostics are rendered with file, line,
and column for every loaded module. Source rejection exits `1`; operational source-resolution
failure exits `2`, and neither failure commits an artifact.
