## MODIFIED Requirements

### Requirement: LlvmBackend lowers MIR to deterministic bitcode

The `LlvmBackend` SHALL lower MIR through the Silk LLVM builder — functions, blocks, literals,
moves, calls, drops, returns, branches, traps, and generated entry termination mapped onto builder
operations — and SHALL realize every scalar representation from the MIR layout table exactly. It
SHALL emit LLVM bitcode directly, without loading `libLLVM`, using the LLVM C API, or requiring a
compiler-private native FFI. The artifact SHALL record the canonical target, each instance's
deterministic symbol, the explicit machine entry symbol `silk_main`, and ordered effect-entry report
identities when present. Function order MUST NOT select the machine entry. Identical target-aware
MIR programs and requests SHALL produce byte-identical bitcode across fresh processes, gated by a
committed digest.

#### Scenario: Lower the nested-call program

- **WHEN** the target-aware `identity(identity(42))` program is emitted
- **THEN** the bitcode contains `silk_main` and the identity instance's symbol, with calls lowered as direct calls and the module naming the selected target

#### Scenario: Lower an effectful entry adapter

- **WHEN** MIR selects an effectful `Unit` entry with two reportable failures
- **THEN** bitcode contains a zero-parameter scalar `silk_main` adapter that returns `0` or the normalized one-based failure tag and the artifact records both canonical report identities

#### Scenario: Ignore function order for entry naming

- **WHEN** equivalent MIR lists non-entry functions before or after the selected user entry
- **THEN** only the explicit machine adapter or selected ordinary entry is named `silk_main`

#### Scenario: Repeat emission byte-identically

- **WHEN** the same target-aware program and request are emitted repeatedly in fresh processes
- **THEN** the bitcode bytes are identical

#### Scenario: Lower a trap body

- **WHEN** a lowered function's block ends in a trap
- **THEN** the emitted function terminates in an unreachable trap sequence rather than fabricating a return value

## ADDED Requirements

### Requirement: Direct WebAssembly closes effectful entry outcomes

The direct WebAssembly backend SHALL emit the same explicit `silk_main` adapter semantics as the
LLVM backend without adding host imports: `0` for success and the normalized one-based failure tag
for an unhandled typed failure after payload cleanup. Its artifact SHALL retain the ordered canonical
report identities.

#### Scenario: Emit an import-free effect entry

- **WHEN** a closed effectful entry is emitted directly to WebAssembly
- **THEN** the module remains import-free and `silk_main` returns its closed termination code
