## MODIFIED Requirements

### Requirement: Project-oriented command surface

The root `silk` command SHALL expose `init`, `build`, `check`, `clean`, `format`, `run`, and `build-exe`. `init` SHALL accept an optional path and package-name override. Project compilation commands SHALL accept a shared optional `--manifest-path`, repeatable `--target`, named `--profile`, complete `--profile-input`, and `--optimization` selection. Repeated command-line targets SHALL replace rather than append to manifest targets. `--release` SHALL select the release optimization mode and SHALL conflict with an explicitly different `--optimization`. Named/full profiles SHALL conflict with target and optimization flags.

#### Scenario: Display root help

- **WHEN** a user requests `silk --help`
- **THEN** help lists initialization, project workflows, formatting, and direct-file compilation with their distinct purposes

#### Scenario: Replace manifest targets

- **WHEN** a manifest selects two targets and the user passes one or more `--target` flags
- **THEN** the command uses only the ordered command-line target selectors

#### Scenario: Select release shorthand

- **WHEN** a project command receives `--release` without `--optimization`
- **THEN** it uses the `release` optimization mode

#### Scenario: Reject conflicting profiles

- **WHEN** a project command receives `--release --optimization debug`
- **THEN** it fails before loading or compiling the project

### Requirement: Project build

`silk build` SHALL validate the complete target, artifact-kind, and structured native-link
batch before compilation, then compile sequentially in first-seen canonical target order. It SHALL
honor the selected logical profile's artifact form, or the manifest artifact for target shorthand, and deterministic
target filename. Each target SHALL commit independently; every valid target SHALL be attempted and
a failure SHALL NOT remove another target's artifact. Successful library outcomes SHALL report the
primary library, generated C header, and generated ABI manifest. Ordinary project builds SHALL NOT
require source-root, exact output, Clang, archive-tool, temporary-artifact, or timing flags.

#### Scenario: Build a shared library

- **WHEN** an LLVM project selects a native target and `artifact = "shared-library"`
- **THEN** `silk build` forwards the library kind and structured link inputs and reports the committed platform library, `<package>.h`, and `<package>.abi.json` paths

#### Scenario: Build a static library

- **WHEN** an LLVM project selects a native target and `artifact = "static-library"`
- **THEN** `silk build` forwards the static-library kind and reports the committed archive, `<package>.h`, and `<package>.abi.json` paths

#### Scenario: Build multiple valid targets

- **WHEN** an executable LLVM project selects `host` and `wasm32-unknown-unknown`
- **THEN** both targets are built in declared order to distinct LLVM/target/optimization destinations without C-library companions

#### Scenario: Retain an independent success

- **WHEN** one valid target succeeds and another valid target is rejected
- **THEN** the successful artifact and any companions remain committed, no partial failed destination exists, and the summary reports both outcomes

#### Scenario: Reject the batch during preflight

- **WHEN** any target, artifact kind, or native link input combination is incompatible
- **THEN** the command exits two before compiling any target or creating any new destination

### Requirement: Project run

`silk run` SHALL require executable artifact kind, use the selected named/full profile or resolve the host at the application edge for target shorthand, build it
through LLVM, then execute the result with inherited
standard input, output, and error and all arguments after `--`. A selected library or foreign-target profile SHALL fail preflight. Without a named/full selection, a manifest library kind SHALL also fail rather than being overridden or executed.

#### Scenario: Run an executable project

- **WHEN** a project configures executable artifact kind and selects the host
- **THEN** `silk run` builds and executes only the canonical host artifact

#### Scenario: Run with a multi-target manifest

- **WHEN** an executable project configures `host` and `wasm32-unknown-unknown`
- **THEN** `silk run` builds and executes only the canonical host artifact

#### Scenario: Refuse a library project

- **WHEN** a project configured as a shared or static library invokes `silk run`
- **THEN** the command fails before compilation with a clear non-executable-artifact error

#### Scenario: Preserve program failure

- **WHEN** the built host executable exits with a non-zero status
- **THEN** `silk run` exits with that same status rather than treating it as a compiler failure

### Requirement: Explicit direct-file compilation

`silk build-exe <source>` SHALL retain direct-file controls for source root, output, target, complete profile input, optimization mode, Clang path, saved temporary artifacts, and timing reports. The former `silk compile` command SHALL NOT be registered.

#### Scenario: Compile one rooted graph directly

- **WHEN** `silk build-exe src/app/Main.silk --source-root src -o app` is invoked
- **THEN** the rooted module graph is compiled to `app` using the same strict compiler behavior as project builds

#### Scenario: Request the removed command

- **WHEN** a user invokes `silk compile`
- **THEN** command parsing reports that no such subcommand exists
