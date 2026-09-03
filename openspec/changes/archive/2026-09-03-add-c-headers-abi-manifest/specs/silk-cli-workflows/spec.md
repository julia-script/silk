## MODIFIED Requirements

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
