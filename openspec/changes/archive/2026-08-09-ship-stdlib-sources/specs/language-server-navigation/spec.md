## MODIFIED Requirements

### Requirement: Cross-file definitions use exact analyzed sources

For a target in another module, the language server SHALL return that module's actual document URI and declaration-name range calculated from the exact analyzed bytes. An open target SHALL use its synchronized URI; a closed project module SHALL use its project file URI; a file-backed standard-library target SHALL use the URI of its canonical shipped `.silk` file.

#### Scenario: Definition in an open unsaved module

- **WHEN** a reference resolves to a declaration in an open imported module with unsaved changes
- **THEN** definition returns the open URI and range from synchronized contents

#### Scenario: Definition in a closed project module

- **WHEN** a reference resolves to a declaration loaded from a closed project file
- **THEN** definition returns the analyzed project file URI and declaration range

#### Scenario: Definition in the standard library

- **WHEN** a reference resolves to a declaration in a shipped standard-library module
- **THEN** definition returns the canonical toolchain file URI and exact declaration range

