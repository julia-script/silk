## ADDED Requirements

### Requirement: Backends preserve ordinary FileSystem service lowering

Backends SHALL lower `FileSystem` requirements, provision, calls, values, failures, and user-defined
implementations through the ordinary service, Effect, ownership, and call model. They MUST NOT add
FileSystem-shaped HIR or MIR operations, select a provider, or recognize portable actor names.

#### Scenario: Lower a user-defined provider

- **WHEN** a closed program supplies an ordinary source-defined `FileSystem`
- **THEN** native LLVM and direct Wasm lower its service calls through the same generic machinery used by other services

#### Scenario: Keep actor names unprivileged

- **WHEN** a user declares another legal service and values with equivalent shapes under different names
- **THEN** backends apply the same lowering behavior without requiring intrinsic inventory entries

### Requirement: Portable filesystem support is pay-for-use

Packaging canonical portable FileSystem source MUST NOT add filesystem runtime symbols or host imports
to an artifact. A direct-Wasm program using no filesystem or supplying a pure user-defined
implementation SHALL emit no OS filesystem import. Equivalent target, executable closure, and
provider source SHALL produce deterministic artifacts.

#### Scenario: Emit direct Wasm with a pure provider

- **WHEN** a program supplies a pure ordinary-source FileSystem and reaches no platform intrinsic
- **THEN** direct Wasm contains no OS filesystem imports

#### Scenario: Emit a program with no filesystem use

- **WHEN** canonical filesystem declarations are packaged but absent from executable closure
- **THEN** native and Wasm artifacts contain no filesystem runtime symbols or host imports

#### Scenario: Repeat portable emission

- **WHEN** the same portable filesystem program is emitted repeatedly for one target
- **THEN** its artifacts and service-call identities are byte-for-byte deterministic
