## MODIFIED Requirements

### Requirement: Driver outcomes identify backend and artifact kind

Every successful driver outcome SHALL retain the canonical backend identifier, target, one of
native executable, native shared library, native static library, or WebAssembly module as its
artifact kind, durable path, symbols, diagnostics, and phase report. Executable and WebAssembly
outcomes SHALL retain their entry termination contract; library outcomes SHALL identify their
explicit exported C surface and the durable C-header and ABI-manifest paths without claiming
process termination behavior.

#### Scenario: Report a native shared library

- **WHEN** LLVM successfully produces a durable shared library
- **THEN** the outcome identifies backend `llvm`, the canonical native target, shared-library artifact kind, destination, ordered C exports, C-header path, and ABI-manifest path

#### Scenario: Report a native static library

- **WHEN** LLVM successfully produces a durable static library
- **THEN** the outcome identifies backend `llvm`, the canonical native target, static-library artifact kind, destination, ordered C exports, C-header path, and ABI-manifest path

#### Scenario: Report a direct Wasm build

- **WHEN** the direct WebAssembly backend successfully produces a durable module
- **THEN** the outcome identifies backend `wasm`, target `wasm32-unknown-unknown`, WebAssembly module kind, its destination, structured termination contract, no C-library companions, and no Clang phases

#### Scenario: Report an LLVM Wasm build

- **WHEN** LLVM successfully produces a durable WebAssembly module
- **THEN** the outcome identifies backend `llvm`, the canonical WebAssembly target, WebAssembly module kind, destination, structured termination contract, no C-library companions, and the executed LLVM finalization phases
