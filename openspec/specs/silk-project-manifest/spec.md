# silk-project-manifest Specification

## Purpose

Defines how Silk language-tool commands discover, validate, and materialize one reproducible project configuration from `silk.toml`.

## Requirements

### Requirement: Minimal project manifest

The system SHALL accept a UTF-8 `silk.toml` containing a `[package]` table with required `name`, `version`, and `root` strings and an optional `source-root` string. The package name SHALL be a portable lowercase identifier beginning with an ASCII letter and continuing with lowercase letters, digits, or hyphens. The version SHALL be a valid semantic version. Manifest paths SHALL be interpreted relative to the manifest directory.

#### Scenario: Load the minimal manifest

- **WHEN** `silk.toml` declares `[package]`, `name = "hello"`, `version = "0.1.0"`, and `root = "src/main.silk"`
- **THEN** the project name and version are retained, the entry is the canonical manifest-relative `src/main.silk`, and its directory is the source root

#### Scenario: Use an explicit source root

- **WHEN** the package root is `src/app/Main.silk` and `source-root = "src"`
- **THEN** the project source root is `src` and the root module identity is `app/Main`

#### Scenario: Reject an invalid manifest

- **WHEN** TOML is malformed, a required field is absent, the package name or version is invalid, the entry is not an exact `.silk` file, or the entry escapes its source root
- **THEN** project loading fails with a typed project error that identifies the manifest and reason

### Requirement: Upward project discovery

Without an explicit manifest path, the system SHALL search the current directory and then each parent directory for the nearest `silk.toml`. With `--manifest-path`, the system SHALL load only the named file and SHALL NOT search elsewhere.

#### Scenario: Discover a parent project

- **WHEN** a command starts within a nested directory below a project
- **THEN** it uses the nearest ancestor `silk.toml`

#### Scenario: Prefer the nearest nested project

- **WHEN** both a directory and one of its ancestors contain `silk.toml`
- **THEN** discovery selects the manifest in the nearer directory

#### Scenario: Explicit manifest is absent

- **WHEN** `--manifest-path` names a missing file
- **THEN** project loading fails without falling back to upward discovery

#### Scenario: No project exists

- **WHEN** upward discovery reaches the filesystem root without finding `silk.toml`
- **THEN** the command fails with guidance to create a manifest or pass `--manifest-path`

### Requirement: Deterministic project artifact layout

The system SHALL derive each artifact destination as `<output-dir>/<backend>/<target>/<profile>/<package>` below the manifest directory, appending `.wasm` for `wasm32-unknown-unknown`. The default output directory SHALL be the visible `build` directory. Backend and target path segments SHALL use their resolved canonical identifiers, including resolution of `host` to the canonical host triple, and required parent directories SHALL be created before compilation.

#### Scenario: Build a debug host artifact

- **WHEN** package `hello` is built through `llvm` for resolved target `aarch64-apple-darwin` with profile `debug`
- **THEN** its destination is `<project>/build/llvm/aarch64-apple-darwin/debug/hello`

#### Scenario: Keep two Wasm backends distinct

- **WHEN** package `hello` is built for `wasm32-unknown-unknown` through both `llvm` and `wasm`
- **THEN** the artifacts are `<project>/build/llvm/wasm32-unknown-unknown/debug/hello.wasm` and `<project>/build/wasm/wasm32-unknown-unknown/debug/hello.wasm`

#### Scenario: Replan the same build

- **WHEN** the same project, backend, targets, and profile are planned repeatedly
- **THEN** every canonical entry, source root, root module identity, and destination is identical and ordered deterministically

### Requirement: Optional build defaults

The manifest SHALL accept an optional `[build]` table with `backend`, `targets`, `output-dir`, and
`native-libraries`. Backend identifiers SHALL be `llvm` or `wasm`; targets SHALL be a non-empty ordered array of canonical target identifiers or the portable `host` selector; `output-dir` SHALL be a non-empty manifest-relative directory; and `native-libraries` SHALL be an array of library names that the native link step passes as structured `-l<name>` arguments, each a non-empty string without path separators, whitespace, NUL, or a leading `-`. An invalid `native-libraries` value SHALL fail project loading with a typed project error naming the manifest and reason, and the list SHALL be ignored for WebAssembly targets. When `[build]` or individual fields are omitted, `llvm` SHALL default to targets `["host"]`, `wasm` SHALL default to `["wasm32-unknown-unknown"]`, the output directory SHALL default to `build`, and the native library list SHALL default to empty.

#### Scenario: Apply sparse defaults

- **WHEN** the manifest contains no `[build]` table
- **THEN** project building selects backend `llvm`, target `host`, and output directory `build`

#### Scenario: Select multiple targets

- **WHEN** `[build]` declares `backend = "llvm"` and `targets = ["host", "wasm32-unknown-unknown"]`
- **THEN** target selectors retain their declared order and `host` resolves to the canonical current-host triple before planning

#### Scenario: Reject an incompatible batch

- **WHEN** `[build]` selects backend `wasm` with a native target
- **THEN** project planning fails before creating or replacing any artifact

#### Scenario: Deduplicate target selectors

- **WHEN** multiple selectors resolve to the same canonical target
- **THEN** that target is built once at the position of its first selector

#### Scenario: Load native libraries

- **WHEN** `silk.toml` declares `[build]` with `native-libraries = ["c", "m"]`
- **THEN** the project build configuration retains `["c", "m"]` in order and the native link command contains `-lc` and `-lm`

#### Scenario: Reject a flag disguised as a library

- **WHEN** `native-libraries = ["-Wl,--export-dynamic"]`
- **THEN** project loading fails with a typed error naming `build.native-libraries`
