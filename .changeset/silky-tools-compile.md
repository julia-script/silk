---
'@silk-effect/compiler-cli': minor
---

Add the `silk` command line interface built on Effect's CLI primitives. `silk compile` runs the
whole driver pipeline over one source file and emits a native executable, with target, profile,
pinned Clang, save-temps, and per-phase timing flags. Diagnostics are rendered with file, line,
and column, and a failing run exits non-zero. Single-file input is deliberate until the compiler
resolves imports against the filesystem.
