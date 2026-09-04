# bootstrap-backend Specification

## Purpose

The LLVM-backed runtime implementation family for native and WebAssembly artifacts.

## Requirements

### Requirement: LLVM is the runtime backend

The compiler SHALL expose one `llvm` backend. It SHALL consume target-aware monomorphized MIR and
emit deterministic LLVM bitcode for every supported native target and `wasm32-unknown-unknown`.
Backend selection MUST reject every other identifier before emission.

#### Scenario: Emit a native artifact

- **WHEN** valid MIR selects a supported native target
- **THEN** LLVM emission retains the target, entry contract, symbols, and compiler-planned layouts

#### Scenario: Emit a WebAssembly artifact

- **WHEN** valid MIR selects `wasm32-unknown-unknown`
- **THEN** LLVM bitcode is finalized as a WebAssembly module with the selected entry contract

### Requirement: LLVM lowering obeys compiler-owned MIR

LLVM lowering SHALL realize scalar and aggregate layout, ownership cleanup, calls, control flow,
traps, entry termination, C ABI boundaries, and target availability from MIR without inventing a
second semantic model. Textual IR is an inspection artifact and carries no compatibility promise.

#### Scenario: Reject inconsistent MIR

- **WHEN** MIR conflicts with the selected target layout or operation availability
- **THEN** verification rejects it before artifact construction

### Requirement: Runtime outcomes have independent oracles

Target-neutral runtime behavior SHALL be pinned in the shared native acceptance corpus. Intended
WebAssembly behavior SHALL be asserted through LLVM-to-Wasm tests. Lowering and ABI claims SHALL use
IR, object, symbol, relocation, disassembly, or separately compiled C-fixture evidence.

#### Scenario: Run a corpus case

- **WHEN** a native corpus program completes, traps, or reports an unhandled failure
- **THEN** its process result is compared directly with the case's independently pinned expectation
