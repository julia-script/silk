## MODIFIED Requirements

### Requirement: Project-oriented command surface

The root `silk` command SHALL expose `init`, `build`, `check`, `clean`, `format`, `run`, `test`, and `build-exe`. `init` SHALL accept an optional path and package-name override. Project compilation commands SHALL accept a shared optional `--manifest-path`, repeatable `--target`, named `--profile`, complete `--profile-input`, and `--optimization` selection. Repeated command-line targets SHALL replace rather than append to manifest targets. `--release` SHALL select the release optimization mode and SHALL conflict with an explicitly different `--optimization`. Named/full profiles SHALL conflict with target and optimization flags. Test execution SHALL apply the single-host executable restrictions defined by the project-test requirements.

#### Scenario: Display root help

- **WHEN** a user requests `silk --help`
- **THEN** help lists initialization, project workflows including testing, formatting, and direct-file compilation with their distinct purposes

#### Scenario: Replace manifest targets

- **WHEN** a manifest selects two targets and the user passes one or more `--target` flags
- **THEN** the command uses only the ordered command-line target selectors

#### Scenario: Select release shorthand

- **WHEN** a project command receives `--release` without `--optimization`
- **THEN** it uses the `release` optimization mode

#### Scenario: Reject conflicting profiles

- **WHEN** a project command receives `--release --optimization debug`
- **THEN** it fails before loading or compiling the project

## ADDED Requirements

### Requirement: Project test selects one discovery root

`silk test` SHALL use existing manifest discovery and `--manifest-path` behavior. Its discovery root SHALL default to `package.root`. One optional `--root` SHALL replace that root with an exact project-relative `.silk` path, interpreted from the manifest directory and contained in the unchanged configured source root. It SHALL NOT scan directories, implicitly import test files, or require/call `main` in the discovery root. The root SHALL be resolved through the ordinary required-root source resolver.

#### Scenario: Use the package root

- **WHEN** a project runs `silk test` without `--root`
- **THEN** discovery follows the configured package root's active imports and execution enters the bundled runner

#### Scenario: Use an import-only root

- **WHEN** `silk test --root src/tests.silk` selects a file containing only imports
- **THEN** those imports define the discovery graph without requiring a test-root `main` or changing import resolution's source root

#### Scenario: Reject an unavailable required root

- **WHEN** the selected test root is missing or unreadable
- **THEN** the command reports the required-root failure and exits 2 without executing a runner or committing a partial test artifact

### Requirement: Project test builds a distinct host executable

`silk test` SHALL build and execute one final native host test executable composed from the ordinary bundled Silk runner, hosted source runtime, and explicit discovery context. It SHALL use compatible project profile/binding, optimization, and native-input settings while rejecting explicitly incompatible target/runtime/artifact selections before execution. Package library defaults SHALL NOT prevent building this separate test executable. Multi-target manifest shorthand SHALL select the host for testing; explicit target selections SHALL resolve to exactly the host. Test artifacts SHALL occupy a distinct test-purpose destination under the configured output directory and SHALL NOT replace application or library outputs. The ordinary application root's `main` SHALL NOT be invoked.

#### Scenario: Test a library project

- **WHEN** a package with a default library artifact runs `silk test`
- **THEN** it builds a separate host test executable without changing or replacing its library artifact configuration or output

#### Scenario: Reject an incompatible explicit target

- **WHEN** an explicit test target/profile requests a foreign target or multiple distinct targets
- **THEN** preflight rejects the request instead of attempting a cross-target or matrix execution

#### Scenario: Keep test output separate

- **WHEN** the same project performs an ordinary build followed by a test build
- **THEN** the ordinary artifact remains at its original destination and the test executable has a distinct destination

### Requirement: Project test transports filters without changing compilation

The command SHALL accept one optional `--file` and one optional `--filter`. It SHALL normalize the project-relative file path against the manifest/source mapping and pass both selections as runtime inputs to the source runner. A file filter SHALL NOT cause source loading, require a filesystem existence check, or select a new discovery root. Neither filter SHALL prune test compilation or enter compiled test content identity. Every discovered test SHALL satisfy the runner's compilation contract before execution begins.

#### Scenario: Filter an unreachable file

- **WHEN** `--file` names a valid project-relative source path outside the discovery root's reachable graph
- **THEN** no module is added or loaded and the runner reports zero selected tests

#### Scenario: Reject damage in a nonmatching test

- **WHEN** a discovered test has a compilation error but a name filter would select another test
- **THEN** compilation fails and no tests execute

#### Scenario: Change only the runtime selection

- **WHEN** equivalent test builds differ only in `--file` or `--filter`
- **THEN** they retain the same compiled test content identity while their runner invocation arguments differ

### Requirement: Project test preserves runner outcomes

Before execution, `silk test` SHALL use existing source-rejection status 1 and configuration/storage/toolchain status 2, without partial artifact commitment. After execution starts, the command SHALL preserve the runner's completed exit status and inherited output streams. A signal/trap SHALL retain abnormal process termination and SHALL NOT be rewritten as a successful summary or a recoverable assertion failure.

#### Scenario: Preserve a failed test run

- **WHEN** the source runner completes with status 1 after reporting typed test failures
- **THEN** `silk test` exits 1 and preserves its report

#### Scenario: Preserve a runner operational failure

- **WHEN** the source runner exits 2 due to an operational failure
- **THEN** `silk test` also exits 2 rather than reporting all tests passed
