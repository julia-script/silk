## MODIFIED Requirements

### Requirement: The driver compiles a request to a durable executable artifact

The driver SHALL orchestrate closure loading, header collection, elaboration, ownership, instance discovery, canonical target layout, MIR lowering, explicitly selected compatible backend emission, and artifact-kind-specific finalization to a requested durable destination under a fixed optimization profile. Native LLVM requests SHALL emit an object, compile the native shim, and link an executable. LLVM Wasm requests SHALL use the pinned LLVM-to-Wasm finalizer, while direct WebAssembly requests SHALL atomically commit the backend's validated module bytes without invoking Clang. The selected target-aware MIR program SHALL pass through later phases without a second layout value. An unavailable entry, unsupported backend-target pair, inconsistent layout, finalization failure, or toolchain failure SHALL surface as a closed outcome naming the failing stage and provenance, never as a thrown error.

#### Scenario: Compile and run a native program

- **WHEN** the driver compiles the nested-call corpus through LLVM to a supported host destination
- **THEN** a native executable exists there and running it exits with the evaluator's result

#### Scenario: Produce LLVM WebAssembly

- **WHEN** the request selects backend `llvm` and target `wasm32-unknown-unknown`
- **THEN** the driver commits an instantiable `.wasm` module produced by the pinned LLVM-to-Wasm path and exporting `silk_main`

#### Scenario: Produce direct WebAssembly

- **WHEN** the request selects backend `wasm` and target `wasm32-unknown-unknown`
- **THEN** the driver commits the backend's validated `.wasm` bytes atomically without invoking Clang

#### Scenario: Surface an entry failure as a closed outcome

- **WHEN** the request's root module has no valid entry
- **THEN** the driver returns a no-entry outcome carrying the discovery reason and phase report without finalizing an artifact

#### Scenario: Stop on an unsupported backend-target pair

- **WHEN** the request selects a backend and target outside their compatibility matrix
- **THEN** the driver returns a target-stage failure before MIR lowering, backend emission, or external tool invocation

#### Scenario: Name the failing finalization stage

- **WHEN** a selected external finalizer fails
- **THEN** the driver returns a failed outcome naming the exact finalization stage with command provenance

## ADDED Requirements

### Requirement: Driver outcomes identify backend and artifact kind

Every successful driver outcome SHALL retain the canonical backend identifier, target, artifact kind, durable path, symbols, diagnostics, and phase report. Reports SHALL include only phases actually executed and SHALL distinguish backend emission from artifact finalization.

#### Scenario: Report a direct Wasm build

- **WHEN** the direct WebAssembly backend successfully produces a durable module
- **THEN** the outcome identifies backend `wasm`, target `wasm32-unknown-unknown`, artifact kind WebAssembly module, its destination, and no Clang phases

#### Scenario: Report an LLVM Wasm build

- **WHEN** LLVM successfully produces a durable WebAssembly module
- **THEN** the outcome identifies backend `llvm`, the same canonical target, the WebAssembly module kind, and the executed LLVM finalization phases
