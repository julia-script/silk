## MODIFIED Requirements

### Requirement: Deterministic project artifact layout

The system SHALL derive each artifact destination below
`<output-dir>/<backend>/<target>/<profile>/`. Executables SHALL use the package name, WebAssembly
modules SHALL append `.wasm`, shared libraries SHALL use the target platform's conventional shared
library prefix and suffix, and static libraries SHALL use the target platform's conventional static
archive prefix and suffix. Backend and target segments SHALL use resolved canonical identifiers,
including resolution of `host`, and required parent directories SHALL be created before compilation.

#### Scenario: Build a debug host artifact

- **WHEN** package `hello` is built as an executable through LLVM for `aarch64-apple-darwin` with profile `debug`
- **THEN** its destination is `<project>/build/llvm/aarch64-apple-darwin/debug/hello`

#### Scenario: Build host libraries

- **WHEN** package `hello` is built as shared and static libraries for an Apple host
- **THEN** their filenames are `libhello.dylib` and `libhello.a` beneath their backend, target, and profile directory

#### Scenario: Keep two Wasm backends distinct

- **WHEN** package `hello` is built for `wasm32-unknown-unknown` through both `llvm` and `wasm`
- **THEN** the artifacts are `<project>/build/llvm/wasm32-unknown-unknown/debug/hello.wasm` and `<project>/build/wasm/wasm32-unknown-unknown/debug/hello.wasm`

#### Scenario: Replan the same build

- **WHEN** the same project, backend, targets, profile, and artifact kind are planned repeatedly
- **THEN** every canonical entry, source root, root module identity, and destination is identical and ordered deterministically

### Requirement: Optional build defaults

The manifest SHALL accept an optional `[build]` table with `backend`, `targets`, `output-dir`,
`artifact`, and `native-link-inputs`. Artifact values SHALL be `executable`, `shared-library`, or
`static-library`, defaulting to `executable`; native library kinds SHALL require backend `llvm` and
native targets. `native-link-inputs` SHALL be an ordered array of inline tables, each containing
exactly one input form: `object`, `static-archive`, `search-path`, or `framework`, or a `library`
paired with mode `static` or `dynamic`. Paths SHALL be non-empty manifest-relative paths that do not
escape the project and SHALL materialize as absolute paths. Names SHALL be non-empty and SHALL NOT
contain whitespace, NUL, path separators, or a leading hyphen. Invalid or unknown shapes SHALL fail
loading with a typed project error; WebAssembly planning SHALL reject non-empty native link inputs.

#### Scenario: Apply sparse defaults

- **WHEN** the manifest contains no `[build]` table
- **THEN** project building selects backend `llvm`, target `host`, artifact `executable`, output directory `build`, and no native link inputs

#### Scenario: Select multiple targets

- **WHEN** `[build]` declares `backend = "llvm"` and `targets = ["host", "wasm32-unknown-unknown"]`
- **THEN** target selectors retain their declared order and `host` resolves to the canonical current-host triple before planning

#### Scenario: Reject an incompatible batch

- **WHEN** `[build]` selects backend `wasm` with a native target
- **THEN** project planning fails before creating or replacing any artifact

#### Scenario: Deduplicate target selectors

- **WHEN** multiple selectors resolve to the same canonical target
- **THEN** that target is built once at the position of its first selector

#### Scenario: Load structured link inputs

- **WHEN** `native-link-inputs` declares an object, static archive, dynamic library, search path, and framework
- **THEN** the project retains the corresponding immutable tagged values in order and resolves every path relative to the manifest

#### Scenario: Load native libraries

- **WHEN** `native-link-inputs` declares dynamic libraries `c` and `m`
- **THEN** the project retains two ordered Library values with Dynamic mode and the native link plan contains `-lc` and `-lm`

#### Scenario: Reject an ambiguous input

- **WHEN** one native-link-input table declares both `object` and `library`
- **THEN** project loading fails with a typed error naming `build.native-link-inputs`

#### Scenario: Reject a raw flag

- **WHEN** a native-link-input name begins with `-` or an unknown `flags` field is present
- **THEN** project loading fails without exposing an arbitrary linker-flag channel

#### Scenario: Reject a flag disguised as a library

- **WHEN** a native-link-input library name is `-Wl,--export-dynamic`
- **THEN** project loading fails with a typed error naming `build.native-link-inputs`

#### Scenario: Reject a library kind for Wasm

- **WHEN** the manifest selects `artifact = "shared-library"` with a WebAssembly backend or target
- **THEN** project planning fails before creating or replacing any artifact
