## ADDED Requirements

### Requirement: Inspect toolchain provenance

The docs site SHALL expose a direct-link toolchain lab presenting, for an edited program and a
selected optimization profile: the exact planned Clang commands (object emission and link) with
their structured arguments, the bitcode artifact's size, the runtime shim source, and the build
scope lifecycle the driver will follow. The browser issues no processes; the lab presents the
same planned commands the toolchain issues.

#### Scenario: Inspect planned commands

- **WHEN** a developer selects the release profile
- **THEN** the lab shows the object-emission command containing `-c` and `-O2` and the link command combining the program object, shim object, and destination

#### Scenario: Switch profiles

- **WHEN** a developer switches to the debug profile
- **THEN** the planned object command shows `-O0` with `-g` and nothing else about the orchestration changes
