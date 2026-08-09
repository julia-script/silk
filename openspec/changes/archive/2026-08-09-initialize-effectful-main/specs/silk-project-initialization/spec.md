## MODIFIED Requirements

### Requirement: Initialize a minimal executable project

`silk init [path]` SHALL create a sparse `silk.toml`, a `src/main.silk` executable entry declared as
a public zero-argument `effect fn main() -> ()`, and a `/build/` ignore rule at the selected project
directory. An omitted path SHALL select the current directory, and a missing selected directory
SHALL be created. The generated project SHALL load, check, build, and run using manifest defaults
without further configuration.

#### Scenario: Initialize a named child directory

- **WHEN** a user runs `silk init hello` and `hello` does not exist
- **THEN** the CLI creates `hello/silk.toml`, `hello/.gitignore`, and `hello/src/main.silk` as a valid effectful executable project

#### Scenario: Initialize the current directory

- **WHEN** a user runs `silk init` in an eligible existing directory
- **THEN** the CLI creates the project files in that directory rather than creating another child directory

#### Scenario: Run the generated entry

- **WHEN** the generated `src/main.silk` is built and run for the host
- **THEN** its public zero-argument effectful `main` is run once and the process exits with status `0`
