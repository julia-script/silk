## ADDED Requirements

### Requirement: Backends realize compiler-planned callable values

Native LLVM and direct WebAssembly SHALL realize verified callable environments, capture ownership,
shared, exclusive, and consuming application, and cleanup from MIR with results and traps matching
evaluation. The backend MAY erase a non-escaping section into a direct call or choose a target-aware
code-and-environment representation, but MUST NOT change callable mode, capture lifetime,
single-evaluation order, or cleanup behavior. Neither backend SHALL require one universal heap
allocation or runtime callable interpreter.

#### Scenario: Erase a non-escaping section

- **WHEN** a callable section is constructed and immediately applied with no observable identity
- **THEN** either backend may emit a direct call while preserving the same evaluator result and provenance

#### Scenario: Store an owned callable

- **WHEN** a take-once callable with an owned capture crosses an ordinary function boundary
- **THEN** native and Wasm preserve the capture until one invocation or drop and clean it exactly once

#### Scenario: Agree on callable modes

- **WHEN** the parity corpus invokes shared, exclusive, and consuming callable environments
- **THEN** native, Wasm, and evaluation agree on results, rejected repeats, mutation, and cleanup order
