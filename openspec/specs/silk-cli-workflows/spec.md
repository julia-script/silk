# silk-cli-workflows Specification

## Purpose

Defines the user-visible Silk language-tool commands for project checking, building, running, and explicit direct-file compilation.

## Requirements

### Requirement: Project-oriented command surface
The root `silk` command SHALL expose `init`, `build`, `check`, `format`, `run`, and `build-exe`. `init` SHALL accept an optional path and package-name override. Project compilation commands SHALL accept a shared optional `--manifest-path`, one `--backend`, repeatable `--target`, and profile selection. Repeated command-line targets SHALL replace rather than append to manifest targets. `--release` SHALL select the release profile and SHALL conflict with an explicitly different `--profile`.

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
`silk build` SHALL validate the complete backend and target batch before compilation, then compile the discovered project root and reachable modules sequentially in first-seen canonical target order. Each successful target SHALL be committed independently to its deterministic destination, a failure SHALL NOT remove another target's successful artifact, and every valid target SHALL be attempted. Ordinary project builds SHALL NOT require source-root, exact output, Clang, temporary-artifact, or timing flags.

#### Scenario: Build multiple valid targets
- **WHEN** an LLVM project selects `host` and `wasm32-unknown-unknown`
- **THEN** both targets are built in declared order to distinct backend-qualified destinations

#### Scenario: Retain an independent success
- **WHEN** one valid target succeeds and another valid target is rejected
- **THEN** the successful artifact remains committed, no partial failed destination exists, and the summary reports both outcomes

#### Scenario: Reject the batch during preflight
- **WHEN** any backend and target pair is incompatible or unavailable
- **THEN** the command exits two before compiling any target or creating any new destination

### Requirement: Project run
`silk run` SHALL select exactly the resolved host target, build it through a backend capable of producing a native executable, then execute the result with inherited standard input, output, and error and all arguments after `--`. Manifest target arrays SHALL NOT cause foreign or Wasm targets to be built during run. The command SHALL return the program's exit status after a successful build.

#### Scenario: Run with a multi-target manifest
- **WHEN** a project configures `host` and `wasm32-unknown-unknown` and the selected backend can build the host
- **THEN** `silk run` builds and executes only the canonical host artifact

#### Scenario: Preserve program failure
- **WHEN** the built host program exits with a non-zero status
- **THEN** `silk run` exits with that same status rather than treating it as a compiler failure

#### Scenario: Refuse a non-runnable backend
- **WHEN** the selected backend cannot produce a host executable
- **THEN** `silk run` fails before compilation with a clear backend compatibility error

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
