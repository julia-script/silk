# @silk-effect/compiler-cli

The project-oriented command line interface for the Silk Effect bootstrap compiler.

## Project setup

A Silk project is rooted by a `silk.toml` manifest:

```toml
[package]
name = "hello"
root = "src/Main.silk"
```

`package.name` starts with a lowercase letter and contains only lowercase letters, digits, and
hyphens. `package.root` is resolved relative to the manifest. By default, the entry file's
directory is also its source root. A wider root can be selected explicitly:

```toml
[package]
name = "hello"
root = "src/app/Main.silk"
source-root = "src"
```

Project commands search the working directory and its ancestors for the nearest `silk.toml`.
`--manifest-path <path>` selects one exact manifest instead and disables upward discovery.

## Commands

### `silk format`

Format every exact `.silk` file beneath the project source root, including files that are not
reachable from the project entry:

```bash
silk format
silk format src/model src/Draft.silk
silk format --manifest-path ./examples/hello/silk.toml
```

Positional files and directories restrict the selection. Directories are searched recursively;
duplicate paths are removed, symlinked directories are not followed, and selections resolving
outside the source root are rejected. Files are processed and reported in canonical path order.

Formatting is strict about concrete syntax. A file with a lexical error, parser error, missing
token, or unexpected-token region is reported and left unchanged, while other selected files are
still processed. Name-resolution and type errors do not prevent formatting.

The canonical source representation has no style options:

- 100-column layout target and two-space indentation
- spaces instead of tabs and LF line endings
- no trailing whitespace and exactly one final newline
- compact lists when they fit; otherwise one item per line with a trailing comma
- exactly one blank line between top-level declarations
- at most one author-supplied blank line inside blocks
- preserved line and documentation comments, except terminal spaces and tabs

`///` lines immediately preceding a declaration or struct field without a blank line form its
documentation block. Comments are never reflowed and may exceed the width target.

#### How to enforce formatting in CI

Run the same selection without writing files:

```bash
silk format --check
```

The check is complete when the command exits `0`. Exit `1` means at least one file needs formatting
or contains damaged syntax. Exit `2` means project discovery, path selection, source storage, or a
write operation failed.

### `silk check`

Analyze the entire reachable module graph without invoking the backend, Clang, or the linker and
without creating `.silk` build output.

```bash
silk check
silk check --manifest-path ./examples/hello/silk.toml
```

### `silk build`

Build the nearest project as a native executable:

```bash
silk build
silk build --release
silk build --profile release-with-debug
```

Artifacts have a deterministic location:

```text
.silk/build/<target>/<profile>/<package-name>
```

### `silk run`

Build for the host and run the resulting executable. Arguments after `--` are passed literally to
the program, and stdin, stdout, and stderr are inherited:

```bash
silk run
silk run --release -- --verbose input.txt
```

Cross-target execution is rejected before compilation. A successful build followed by program
execution returns the program's exact exit status.

### `silk build-exe`

Compile an explicitly selected root source without a manifest. This is the low-level escape hatch
for scripts, compiler experiments, and one-off files:

```bash
silk build-exe main.silk -o ./main
silk build-exe ./src/app/Main.silk --source-root ./src -o ./main
```

`build-exe` supports `--source-root`, `--output`/`-o`, `--target`, `--profile`, `--clang`,
`--save-temps`, and `--timings`. The former `silk compile` command was removed; there is no
compatibility alias.

## Shared project options

`build`, `check`, and `run` accept:

- `--manifest-path <path>` — select an exact manifest.
- `--target <target>` — select a native target; the host is the default.
- `--profile <debug|release|release-with-debug>` — select the compilation profile.
- `--release` — shorthand for `--profile release`; it conflicts with a different explicit profile.

`format` accepts `--manifest-path` but does not accept target, profile, or release options because
it does not perform semantic analysis or compilation.

## Module resolution

Imports resolve from the project source root, never from the importing file's directory.
`import compiler.Syntax` requests exactly `<source-root>/compiler/Syntax.silk`. Resolution does not
probe alternate extensions, index files, parent directories, or case-folded names.

A missing imported file is a recoverable source diagnostic. Permission, I/O, and equivalent access
failures are operational resolver failures. `check` retains all safe frontend facts for tooling;
`build` and `run` stop before backend work when resolution or source validation fails.

## Exit behavior

- `0` — checking or building succeeded.
- `1` — source diagnostics or a missing/invalid entry rejected the program.
- `2` — project configuration, source storage, target, backend, or toolchain operation failed.
- `silk format` — `0` for success, `1` for check drift or damaged syntax, and `2` for project,
  selection, storage, or write failures.
- `silk run` — after a successful build, the compiled program's exact exit status.

Failed builds do not commit the requested executable. Diagnostics from every loaded module use the
form `path:line:column: severity[CODE] message`.

## Toolchain

Project workflows currently use `clang` from the process environment. `build-exe --clang <path>`
can pin a specific executable. Automatic toolchain discovery and management are not implemented.

## Planned, not yet supported

The CLI intentionally exposes no placeholder commands for these future capabilities:

- `test` and a language-level test model
- `clean`
- `new` and `init`
- `doc`
- target discovery/listing
- incremental and shared build caching
- multi-package workspaces
- dependency and package management
- stable machine-readable output
- language-server command integration
- toolchain installation, discovery, and version management

Their eventual interfaces should be designed from real compiler and project requirements rather
than reserved prematurely in the command surface.
