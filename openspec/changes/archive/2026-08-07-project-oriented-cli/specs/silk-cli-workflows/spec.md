## Purpose

Defines the user-visible Silk language-tool commands for project checking, building, running, and explicit direct-file compilation.

## ADDED Requirements

### Requirement: Project-oriented command surface

The root `silk` command SHALL expose `build`, `check`, `run`, and `build-exe`. Project commands SHALL accept a shared optional `--manifest-path`, compilation target, and profile selection. `--release` SHALL select the release profile and SHALL conflict with an explicitly different `--profile`.

#### Scenario: Display root help

- **WHEN** a user requests `silk --help`
- **THEN** help lists `build`, `check`, `run`, and `build-exe` with project commands distinguished from direct-file compilation

#### Scenario: Select release shorthand

- **WHEN** a project command receives `--release` without `--profile`
- **THEN** it uses the `release` profile

#### Scenario: Reject conflicting profiles

- **WHEN** a project command receives `--release --profile debug`
- **THEN** it fails before loading or compiling the project

### Requirement: Project check

`silk check` SHALL load the project and complete the compiler's recoverable analysis facade without invoking a backend, Clang, linking, or artifact creation. It SHALL print diagnostics for every loaded source using physical paths while retaining all tooling facts the facade makes available.

#### Scenario: Check a valid project

- **WHEN** all reachable project sources pass frontend analysis
- **THEN** `silk check` exits zero and creates no `.silk/build` artifact

#### Scenario: Check damaged sources

- **WHEN** reachable project sources contain syntax, import, or semantic errors
- **THEN** `silk check` reports all available diagnostics and exits one while preserving recoverable analysis

#### Scenario: Check encounters storage failure

- **WHEN** a required source cannot be read operationally
- **THEN** `silk check` reports the resolver failure and exits two

### Requirement: Project build

`silk build` SHALL compile the discovered project root and reachable modules into the deterministic project destination. Ordinary project builds SHALL NOT require source-root, output, Clang, temporary-artifact, or timing flags.

#### Scenario: Build a valid project

- **WHEN** a valid project is built for a supported native target
- **THEN** the executable is committed at the planned destination and the command exits zero

#### Scenario: Build is rejected

- **WHEN** source diagnostics reject compilation
- **THEN** diagnostics are reported, the command exits one, and no partial destination is committed

#### Scenario: Build infrastructure fails

- **WHEN** manifest loading, source storage, target resolution, object emission, or linking fails operationally
- **THEN** the failure is reported, the command exits two, and no partial destination is committed

### Requirement: Project run

`silk run` SHALL first perform the same project build, then execute the resulting host-target executable with inherited standard input, output, and error and all arguments after `--`. It SHALL return the program's exit status after a successful build.

#### Scenario: Run with arguments

- **WHEN** `silk run -- one two` successfully builds the project
- **THEN** the executable receives `one` and `two` in order and shares the terminal streams

#### Scenario: Preserve program failure

- **WHEN** the built program exits with a non-zero status
- **THEN** `silk run` exits with that same status rather than treating it as a compiler failure

#### Scenario: Refuse a foreign target

- **WHEN** `silk run` selects a target other than the host target
- **THEN** it fails before compilation with a clear target error

### Requirement: Explicit direct-file compilation

`silk build-exe <source>` SHALL retain direct-file controls for source root, output, target, profile, Clang path, saved temporary artifacts, and timing reports. The former `silk compile` command SHALL NOT be registered.

#### Scenario: Compile one rooted graph directly

- **WHEN** `silk build-exe src/app/Main.silk --source-root src -o app` is invoked
- **THEN** the rooted module graph is compiled to `app` using the same strict compiler behavior as project builds

#### Scenario: Request the removed command

- **WHEN** a user invokes `silk compile`
- **THEN** command parsing reports that no such subcommand exists

### Requirement: Stable command exit classes

Before a program is executed, language-tool commands SHALL use exit zero for success, one for source rejection, and two for project, storage, target, or toolchain failure. No failed check or build SHALL leave a newly committed requested executable.

#### Scenario: Manifest failure status

- **WHEN** a project manifest cannot be discovered or decoded
- **THEN** the command exits two

#### Scenario: Source rejection status

- **WHEN** frontend diagnostics reject check or build
- **THEN** the command exits one
