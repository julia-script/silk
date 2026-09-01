## Purpose

Defines how Silk language-tool commands discover, validate, and materialize one reproducible project configuration from `silk.toml`.

## ADDED Requirements

### Requirement: Minimal project manifest

The system SHALL accept a UTF-8 `silk.toml` containing a `[package]` table with required `name` and `root` strings and an optional `source-root` string. The package name SHALL be a portable lowercase identifier beginning with an ASCII letter and continuing with lowercase letters, digits, or hyphens. Manifest paths SHALL be interpreted relative to the manifest directory.

#### Scenario: Load the minimal manifest

- **WHEN** `silk.toml` declares `[package]`, `name = "hello"`, and `root = "src/Main.silk"`
- **THEN** the project name is `hello`, the entry is the canonical manifest-relative `src/Main.silk`, and its directory is the source root

#### Scenario: Use an explicit source root

- **WHEN** the package root is `src/app/Main.silk` and `source-root = "src"`
- **THEN** the project source root is `src` and the root module identity is `app/Main`

#### Scenario: Reject an invalid manifest

- **WHEN** TOML is malformed, required fields are absent, the package name is invalid, the entry is not an exact `.silk` file, or the entry escapes its source root
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

The system SHALL derive the executable destination as `.silk/build/<target>/<profile>/<package>` below the manifest directory, using the resolved target and selected profile, and SHALL create required parent directories before compilation.

#### Scenario: Build a debug host artifact

- **WHEN** package `hello` is built for target `aarch64-apple-darwin` with profile `debug`
- **THEN** its destination is `<project>/.silk/build/aarch64-apple-darwin/debug/hello`

#### Scenario: Replan the same build

- **WHEN** the same project, target, and profile are planned repeatedly
- **THEN** the canonical entry, source root, root module identity, and destination are identical
