## MODIFIED Requirements

### Requirement: The Backend service is a nominal contract

The `Backend` service SHALL expose a stable backend identifier, its canonical compatible targets, and one emission operation consuming the whole target-aware monomorphized MIR program plus a codegen request, producing one typed program artifact. It MUST NOT accept a second target-layout input or choose an alternate representation for a Silk type. One compilation request SHALL produce one MIR program, one backend module, and one artifact; source modules are semantic namespaces, not codegen units. Artifact finalization SHALL follow the artifact kind and selected target rather than assuming every backend result requires native object emission and linking.

#### Scenario: Emit one artifact per program

- **WHEN** a target-aware lowered program with several functions is emitted through the service
- **THEN** exactly one typed artifact results, containing every function's symbol regardless of which source modules the instances came from

#### Scenario: Finalize by artifact kind

- **WHEN** LLVM emits bitcode or the direct WebAssembly backend emits final module bytes
- **THEN** downstream orchestration can select the compatible finalization path without inspecting an implementation-specific display name

### Requirement: Backends enforce canonical target compatibility

Each backend SHALL declare a stable identifier and the canonical targets it can emit, and SHALL return a typed target-incompatibility outcome before constructing backend state when the MIR plan selects another target. `llvm` SHALL accept every supported native target and `wasm32-unknown-unknown`. The direct `wasm` backend SHALL accept only `wasm32-unknown-unknown`, consume the same planned scalar and aggregate facts as evaluation and LLVM, and MUST NOT emit WebAssembly for native-target MIR.

#### Scenario: Select either Wasm-capable backend

- **WHEN** MIR is planned for `wasm32-unknown-unknown`
- **THEN** either explicitly selected backend `llvm` or `wasm` passes compatibility validation and emits its own deterministic artifact kind

#### Scenario: Reject a native plan in the direct WebAssembly backend

- **WHEN** backend `wasm` receives MIR planned for `aarch64-apple-darwin`
- **THEN** it returns a typed target-incompatibility outcome before constructing a WebAssembly module

#### Scenario: Keep selection independent from target

- **WHEN** a caller selects backend `llvm` and target `wasm32-unknown-unknown`
- **THEN** backend resolution preserves the explicit LLVM choice rather than replacing it with the first backend supporting that target

## ADDED Requirements

### Requirement: LlvmBackend emits wasm32-compatible bitcode

For MIR planned for `wasm32-unknown-unknown`, `LlvmBackend` SHALL realize the compiler-owned 32-bit WebAssembly layout in deterministic LLVM IR and bitcode suitable for the pinned LLVM-to-Wasm finalization path. It SHALL retain the closed entry symbol `silk_main`, and identical inputs SHALL produce byte-identical IR and bitcode across fresh processes.

#### Scenario: Emit LLVM bitcode for Wasm

- **WHEN** backend `llvm` emits a valid program planned for `wasm32-unknown-unknown`
- **THEN** the artifact contains Wasm-target LLVM bitcode with exported-entry provenance for `silk_main`

#### Scenario: Repeat LLVM Wasm emission

- **WHEN** the same Wasm-target MIR and profile are emitted through LLVM in fresh processes
- **THEN** their LLVM IR and bitcode are byte-identical
