# @silk-effect/compiler-cli

The `silk` command line interface for the Silk Effect bootstrap compiler.

## Usage

```bash
silk compile main.silk -o ./main
```

```
FLAGS
  --output, -o string    Destination path for the linked executable (default: a.out).
  --target choice        Compilation target. Defaults to the host target.
  --profile choice       Optimization profile: debug, release, release-with-debug.
  --clang string         Path to the pinned Clang used for object emission and linking.
  --save-temps           Keep the build scope intermediates for inspection.
  --timings              Print the per-phase timing and memory report.
```

The command exits `0` on a successful compilation and `1` when any phase reports failure.
Diagnostics are printed as `path:line:column: severity[CODE] message`.

## Scope: one file

The compiler resolves imports only among the sources it is handed and has no filesystem module
resolution yet, so `compile` accepts exactly one source file. The file's name, minus every
extension, becomes the module identity (`main.silk` → `main`), which must be made of letters,
digits, underscores, or hyphens.

An `import` inside that file therefore reports an unknown module rather than searching the disk.
Multi-file projects are a compiler change — filesystem resolution in `ModuleClosure` — after which
this command grows a directory or manifest input.

## Toolchain

Object emission and linking shell out to Clang. The path is never discovered from `PATH` by the
compiler itself; pass `--clang` to pin it (defaults to `clang`).
