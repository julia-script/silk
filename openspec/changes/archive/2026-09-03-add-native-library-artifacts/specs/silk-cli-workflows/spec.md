## MODIFIED Requirements

### Requirement: Project build

`silk build` SHALL validate the complete backend, target, artifact-kind, and structured native-link
batch before compilation, then compile sequentially in first-seen canonical target order. It SHALL
honor the manifest's executable, shared-library, or static-library selection and deterministic
target filename. Each target SHALL commit independently; every valid target SHALL be attempted and
a failure SHALL NOT remove another target's artifact. Ordinary project builds SHALL NOT require
source-root, exact output, Clang, archive-tool, temporary-artifact, or timing flags.

#### Scenario: Build a shared library

- **WHEN** an LLVM project selects a native target and `artifact = "shared-library"`
- **THEN** `silk build` forwards the library kind and structured link inputs and reports the committed platform library path

#### Scenario: Build a static library

- **WHEN** an LLVM project selects a native target and `artifact = "static-library"`
- **THEN** `silk build` forwards the static-library kind and reports the committed archive path

#### Scenario: Build multiple valid targets

- **WHEN** an executable LLVM project selects `host` and `wasm32-unknown-unknown`
- **THEN** both targets are built in declared order to distinct backend-qualified destinations

#### Scenario: Retain an independent success

- **WHEN** one valid target succeeds and another valid target is rejected
- **THEN** the successful artifact remains committed, no partial failed destination exists, and the summary reports both outcomes

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
