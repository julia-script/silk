# bootstrap-backend Specification

## Purpose

The LLVM-backed runtime implementation family for native and WebAssembly artifacts.

## Requirements

### Requirement: LLVM is the runtime backend

The compiler SHALL lower runtime artifacts through LLVM. It SHALL consume target-aware
monomorphized MIR and emit deterministic LLVM bitcode for every supported native target and
`wasm32-unknown-unknown`. It SHALL expose no runtime-backend selection surface.

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

#### Scenario: Realize eager selected ordinary arms

- **WHEN** verified MIR selects a statement arm that mutates state and completes normally
- **THEN** LLVM performs only that selected statement sequence in source evaluation order and continues with unit, preserving the current computation

#### Scenario: Realize a transfer inside a larger expression

- **WHEN** verified MIR transfers from a statement arm inside an argument or initializer
- **THEN** LLVM emits the corresponding enclosing exit and cleanup without later operand execution, initializer or assignment storage, or a load from an uninitialized match result

### Requirement: Runtime outcomes have independent oracles

Target-neutral runtime behavior SHALL be pinned in the shared native acceptance corpus. Intended
WebAssembly behavior SHALL be asserted through LLVM-to-Wasm tests. Lowering and ABI claims SHALL use
IR, object, symbol, relocation, disassembly, or separately compiled C-fixture evidence.

#### Scenario: Run a corpus case

- **WHEN** a native corpus program completes, traps, or reports an unhandled failure
- **THEN** its process result is compared directly with the case's independently pinned expectation

### Requirement: LLVM targets execute verified exact anonymous callable environments

LLVM-native and LLVM-to-Wasm lowering SHALL consume the verified MIR target, explicit signature,
derived mode, and finite ordered environment for an anonymous callable. Both targets SHALL preserve source
acquisition order, authored parameter order, invocation-mode checks, and exactly-once cleanup. A
backend MUST NOT introduce a universal indirect closure ABI or merge distinct source targets solely
because their signatures or environments are equal. A backend MAY eliminate a nonescaping or empty
environment only when the optimization preserves all observable identity and ownership behavior.

#### Scenario: Preserve an environment-bearing callback across LLVM targets

- **WHEN** an anonymous callable captures values in an order different from its authored parameter order
- **THEN** independently pinned native execution and LLVM-to-Wasm execution retain the verified target, operand order, result, and cleanup
