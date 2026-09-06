## ADDED Requirements

### Requirement: Project profiles carry typed package bindings

The manifest SHALL support a named default through build.profile, named logical profile inputs under profiles, project-tier build.bindings and profile-tier bindings. Each binding SHALL identify package, module and parameter and carry the tagged serializable value/provenance transport defined in the compilation-profile reference. Named profiles SHALL select an explicit canonical target. Complete request overrides and project-profile selection SHALL be mutually exclusive. Unknown profile names and invalid logical inputs SHALL produce structured project diagnostics before compilation. Existing physical output and native link paths SHALL remain outside logical profile identity.

#### Scenario: Select a named profile

- **WHEN** build.profile names a declared profile
- **THEN** compiler and tooling requests use that profile's logical facts and bindings

#### Scenario: Reject an unknown profile

- **WHEN** a request names a profile absent from the manifest
- **THEN** selection reports the unknown name and manifest origin without falling back to host defaults

## MODIFIED Requirements

### Requirement: Deterministic project artifact layout

The system SHALL derive each artifact destination below
`<output-dir>/llvm/<target>/<optimization>/`. Executables SHALL use the package name, WebAssembly
modules SHALL append `.wasm`, shared libraries SHALL use the target platform's conventional shared
library prefix and suffix, and static libraries SHALL use the target platform's conventional static
archive prefix and suffix. Target segments SHALL use resolved canonical identifiers, including
resolution of `host`, and required parent directories SHALL be created before compilation.

#### Scenario: Build a debug host artifact

- **WHEN** package `hello` is built as an executable through LLVM for `aarch64-apple-darwin` with optimization mode `debug`
- **THEN** its destination is `<project>/build/llvm/aarch64-apple-darwin/debug/hello`

#### Scenario: Build host libraries

- **WHEN** package `hello` is built as shared and static libraries for an Apple host
- **THEN** their filenames are `libhello.dylib` and `libhello.a` beneath their LLVM, target, and optimization directory

#### Scenario: Build WebAssembly through LLVM

- **WHEN** package `hello` is built for `wasm32-unknown-unknown`
- **THEN** the artifact is `<project>/build/llvm/wasm32-unknown-unknown/debug/hello.wasm`

#### Scenario: Replan the same build

- **WHEN** the same project, complete profile or target shorthand, and artifact kind are planned repeatedly
- **THEN** every canonical entry, source root, root module identity, and destination is identical and ordered deterministically
