## RENAMED Requirements

- FROM: `### Requirement: The driver compiles a request to a durable executable artifact`
- TO: `### Requirement: The driver compiles a request to its explicit durable artifact kind`

## MODIFIED Requirements

### Requirement: The driver compiles a request to its explicit durable artifact kind

The driver SHALL carry an explicit artifact kind and ordered structured native link inputs through
instance discovery, target-aware lowering, backend emission, caching, and finalization. Native LLVM
requests SHALL support executable, shared-library, and static-library artifacts. Executables SHALL
retain the valid `main` entry contract and process shim; native libraries SHALL require one or more
valid explicit C exports and SHALL NOT require or synthesize `main`. WebAssembly requests SHALL
retain their module finalization paths and SHALL reject a native library kind before emission. All
configuration, entry, backend, and tool failures SHALL remain closed typed outcomes.

#### Scenario: Compile and run a native program

- **WHEN** the driver compiles an executable request with a valid `main`
- **THEN** it emits the entry shim, commits a native executable, and reports the executable artifact kind

#### Scenario: Compile a library without main

- **WHEN** the driver compiles a native shared- or static-library request whose closure has at least one valid `export "C"` function and no `main`
- **THEN** export reachability is lowered, no process-entry shim is required, and the requested native library kind is committed

#### Scenario: Reject a library without exports

- **WHEN** a native library request has no valid explicit C export root
- **THEN** the driver returns a no-entry outcome identifying the missing library export and invokes no backend or external tool

#### Scenario: Preserve structured link inputs

- **WHEN** a native request contains ordered object, archive, named-library, search-path, and framework values
- **THEN** the same immutable values reach tool planning in order without conversion to raw flags

#### Scenario: Reject a native kind for WebAssembly

- **WHEN** a request selects a native library artifact kind with a WebAssembly target or backend
- **THEN** the driver returns a typed target or backend failure before emission

#### Scenario: Produce LLVM WebAssembly

- **WHEN** the request selects WebAssembly module kind, backend `llvm`, and target `wasm32-unknown-unknown`
- **THEN** the driver commits an instantiable `.wasm` module produced by the pinned LLVM-to-Wasm path and exporting `silk_main`

#### Scenario: Produce direct WebAssembly

- **WHEN** the request selects WebAssembly module kind, backend `wasm`, and target `wasm32-unknown-unknown`
- **THEN** the driver commits the backend's validated `.wasm` bytes atomically without invoking Clang

#### Scenario: Surface an entry failure as a closed outcome

- **WHEN** an executable request's root module has no valid `main`, or a library request has no valid explicit C export
- **THEN** the driver returns a no-entry outcome carrying the discovery reason and phase report without finalizing an artifact

#### Scenario: Stop on an unsupported backend-target pair

- **WHEN** the request selects a backend, target, and artifact-kind combination outside their compatibility matrix
- **THEN** the driver returns a target-stage failure before MIR lowering, backend emission, or external tool invocation

#### Scenario: Name the failing finalization stage

- **WHEN** a selected external finalizer fails
- **THEN** the driver returns a failed outcome naming the exact finalization stage with command provenance

### Requirement: Driver outcomes identify backend and artifact kind

Every successful driver outcome SHALL retain the canonical backend identifier, target, one of
native executable, native shared library, native static library, or WebAssembly module as its
artifact kind, durable path, symbols, diagnostics, and phase report. Executable and WebAssembly
outcomes SHALL retain their entry termination contract; library outcomes SHALL identify their
explicit exported C surface without claiming process termination behavior.

#### Scenario: Report a native shared library

- **WHEN** LLVM successfully produces a durable shared library
- **THEN** the outcome identifies backend `llvm`, the canonical native target, shared-library artifact kind, destination, and ordered C exports

#### Scenario: Report a native static library

- **WHEN** LLVM successfully produces a durable static library
- **THEN** the outcome identifies backend `llvm`, the canonical native target, static-library artifact kind, destination, and ordered C exports

#### Scenario: Report a direct Wasm build

- **WHEN** the direct WebAssembly backend successfully produces a durable module
- **THEN** the outcome identifies backend `wasm`, target `wasm32-unknown-unknown`, WebAssembly module kind, its destination, structured termination contract, and no Clang phases

#### Scenario: Report an LLVM Wasm build

- **WHEN** LLVM successfully produces a durable WebAssembly module
- **THEN** the outcome identifies backend `llvm`, the canonical WebAssembly target, WebAssembly module kind, destination, structured termination contract, and the executed LLVM finalization phases
