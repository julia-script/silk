# silk-cli-workflows Specification

## Purpose

Defines the user-visible Silk language-tool commands for project checking, building, running, and explicit direct-file compilation.

## Requirements

### Requirement: Project-oriented command surface

The root `silk` command SHALL expose `init`, `build`, `check`, `clean`, `format`, `run`, and `build-exe`. `init` SHALL accept an optional path and package-name override. Project compilation commands SHALL accept a shared optional `--manifest-path`, one `--backend`, repeatable `--target`, and profile selection. Repeated command-line targets SHALL replace rather than append to manifest targets. `--release` SHALL select the release profile and SHALL conflict with an explicitly different `--profile`.

#### Scenario: Display root help

- **WHEN** a user requests `silk --help`
- **THEN** help lists initialization, project workflows, formatting, and direct-file compilation with their distinct purposes

#### Scenario: Replace manifest targets

- **WHEN** a manifest selects two targets and the user passes one or more `--target` flags
- **THEN** the command uses only the ordered command-line target selectors

#### Scenario: Select release shorthand

- **WHEN** a project command receives `--release` without `--profile`
- **THEN** it uses the `release` profile

#### Scenario: Reject conflicting profiles

- **WHEN** a project command receives `--release --profile debug`
- **THEN** it fails before loading or compiling the project

### Requirement: Project check

`silk check` SHALL load the project and complete the compiler's recoverable analysis facade once for every resolved configured target without invoking a backend, Clang, linking, or artifact creation. It SHALL process targets in deterministic selector order and print target-qualified diagnostics for every loaded source while retaining all tooling facts the facade makes available.

#### Scenario: Check a valid multi-target project

- **WHEN** all reachable project sources pass frontend analysis for every selected target
- **THEN** `silk check` exits zero and creates no build artifact

#### Scenario: Check target-specific damage

- **WHEN** reachable sources are valid for one selected target but contain a target-dependent semantic error for another
- **THEN** `silk check` reports the failing canonical target, completes the remaining target checks, and exits one

#### Scenario: Check encounters storage failure

- **WHEN** a required source cannot be read operationally
- **THEN** `silk check` reports the resolver failure and exits two

### Requirement: Project build

`silk build` SHALL validate the complete backend, target, artifact-kind, and structured native-link
batch before compilation, then compile sequentially in first-seen canonical target order. It SHALL
honor the manifest's executable, shared-library, or static-library selection and deterministic
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
- **THEN** both targets are built in declared order to distinct backend-qualified destinations without C-library companions

#### Scenario: Retain an independent success

- **WHEN** one valid target succeeds and another valid target is rejected
- **THEN** the successful artifact and any companions remain committed, no partial failed destination exists, and the summary reports both outcomes

#### Scenario: Reject the batch during preflight

- **WHEN** any backend, target, artifact kind, or native link input combination is incompatible
- **THEN** the command exits two before compiling any target or creating any new destination

### Requirement: Project run

`silk run` SHALL require executable artifact kind, select exactly the resolved host target, build it
through a backend capable of producing a native executable, then execute the result with inherited
standard input, output, and error and all arguments after `--`. A manifest library kind SHALL fail
preflight rather than being overridden or executed.

#### Scenario: Run an executable project

- **WHEN** a project configures executable artifact kind and the selected backend can build the host
- **THEN** `silk run` builds and executes only the canonical host artifact

#### Scenario: Run with a multi-target manifest

- **WHEN** an executable project configures `host` and `wasm32-unknown-unknown` and the selected backend can build the host
- **THEN** `silk run` builds and executes only the canonical host artifact

#### Scenario: Refuse a library project

- **WHEN** a project configured as a shared or static library invokes `silk run`
- **THEN** the command fails before compilation with a clear non-executable-artifact error

#### Scenario: Preserve program failure

- **WHEN** the built host executable exits with a non-zero status
- **THEN** `silk run` exits with that same status rather than treating it as a compiler failure

#### Scenario: Refuse a non-runnable backend

- **WHEN** the selected backend cannot produce a host executable
- **THEN** `silk run` fails before compilation with a clear backend compatibility error

### Requirement: Project clean

`silk clean` SHALL remove the artifacts the manifest output directory holds and SHALL accept the shared optional `--manifest-path`. It SHALL NOT remove a file the build did not write, and it SHALL exit zero when the output directory does not exist.

#### Scenario: Remove build artifacts

- **WHEN** a project has been built and `silk clean` is invoked
- **THEN** the manifest output directory is removed, every project source file remains, and the command exits zero

#### Scenario: Clean a project that was never built

- **WHEN** `silk clean` is invoked and the manifest output directory does not exist
- **THEN** the command removes nothing and exits zero

### Requirement: Watch mode

`silk build` and `silk check` SHALL accept `--watch`. Watch mode SHALL compile again after a change to any file in the source graph and SHALL report each compilation in the same format as one command run. It SHALL keep running after a compilation that reports diagnostics, and the toolchain exit codes SHALL apply only when the user stops it.

#### Scenario: Recompile after a source change

- **WHEN** `silk check --watch` is running and a file in the source graph changes
- **THEN** the command analyzes the project again and reports the new result in the ordinary single-run format

#### Scenario: Survive a reported diagnostic

- **WHEN** a watched compilation reports a diagnostic
- **THEN** the command prints the diagnostic, keeps watching, and does not exit with the compilation's status

### Requirement: Explicit direct-file compilation

`silk build-exe <source>` SHALL retain direct-file controls for source root, output, target, profile, Clang path, saved temporary artifacts, and timing reports. The former `silk compile` command SHALL NOT be registered.

#### Scenario: Compile one rooted graph directly

- **WHEN** `silk build-exe src/app/Main.silk --source-root src -o app` is invoked
- **THEN** the rooted module graph is compiled to `app` using the same strict compiler behavior as project builds

#### Scenario: Request the removed command

- **WHEN** a user invokes `silk compile`
- **THEN** command parsing reports that no such subcommand exists

### Requirement: Stable command exit classes

Before a program is executed, language-tool commands SHALL use exit zero for complete success, one when any attempted target has source, target-dependent semantic, or backend rejection, and two when configuration, storage, or toolchain failure occurs. A batch containing both exit-one and exit-two outcomes SHALL return two. Each target SHALL prevent partial commitment of its own destination, while successful sibling target artifacts SHALL remain committed.

#### Scenario: Aggregate source rejection

- **WHEN** at least one target is rejected by source or backend validation and no operational failure occurs
- **THEN** the command completes the valid target batch and exits one

#### Scenario: Prefer operational failure

- **WHEN** one target is rejected and another encounters a toolchain failure
- **THEN** the command reports both and exits two

#### Scenario: Manifest failure status

- **WHEN** a project manifest cannot be discovered or decoded
- **THEN** the command exits two without starting a target build

### Requirement: Multi-target commands report a deterministic summary

Build and check SHALL report one outcome per resolved target in deterministic order, qualified by backend when backend work occurs, followed by success and failure counts. The summary SHALL name each successful durable destination and each failure class without allowing concurrent output interleaving.

#### Scenario: Summarize a mixed LLVM batch

- **WHEN** an LLVM batch succeeds for the host and fails for `wasm32-unknown-unknown`
- **THEN** the summary lists the host outcome first, the Wasm outcome second, and totals one success and one failure

### Requirement: Project documentation generation

The root `silk` command SHALL expose `doc` as a project-oriented workflow. `silk doc` SHALL analyze
the reachable project source closure without invoking a backend, linker, or program execution and
SHALL write deterministic experimental Silk documentation JSON to an explicit or deterministic
default destination. It SHALL include only public declarations by default and SHALL accept an
explicit option to include private declarations.

#### Scenario: Generate public documentation

- **WHEN** a valid project containing documented public and private declarations runs `silk doc`
- **THEN** the command writes deterministic JSON containing the public declarations and omitting the private declarations

#### Scenario: Generate private documentation explicitly

- **WHEN** the same project runs `silk doc --include-private`
- **THEN** the JSON contains both public and private declarations with their visibility

#### Scenario: Refuse source damage

- **WHEN** the reachable project source closure contains compiler diagnostics that make semantic documentation facts unavailable
- **THEN** `silk doc` reports the diagnostics, writes no partial destination, and exits with the existing source-rejection class
