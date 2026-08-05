## MODIFIED Requirements

### Requirement: The Backend service is a nominal contract

The `Backend` service SHALL expose one emission operation consuming the whole target-aware
monomorphized MIR program and a codegen request (debug or release), producing one program artifact.
It MUST NOT accept a second target-layout input or choose an alternate representation for a Silk
type. Consumers MUST NOT inspect backend identity: one compilation request produces one MIR
program, one backend module, and one artifact — source modules are semantic namespaces, not codegen
units. The relocatable-object half of the contract SHALL be fulfilled by the pinned native
toolchain orchestration, which turns the artifact's bitcode into one target object under a fixed
optimization profile.

#### Scenario: Emit one artifact per program

- **WHEN** a target-aware lowered program with several functions is emitted through the service
- **THEN** exactly one artifact results, containing every function's symbol, regardless of which source modules the instances came from

#### Scenario: Complete the object contract

- **WHEN** the artifact's bitcode is passed through the pinned toolchain's object emission for the MIR program's target
- **THEN** one relocatable object for that target and requested profile results, completing the backend contract for the compilation

### Requirement: LlvmBackend lowers MIR to deterministic bitcode

The `LlvmBackend` SHALL lower MIR through the Silk LLVM builder — functions, blocks, literals,
moves, calls, drops, returns, branches, and traps mapped onto builder operations — and SHALL realize
every scalar representation from the MIR layout table exactly. It SHALL emit LLVM bitcode directly,
without loading `libLLVM`, using the LLVM C API, or requiring a compiler-private native FFI. The
artifact SHALL record the canonical target and each instance's deterministic symbol, with the entry
instance always named `silk_main`. Identical target-aware MIR programs and requests SHALL produce
byte-identical bitcode across fresh processes, gated by a committed digest.

#### Scenario: Lower the nested-call program

- **WHEN** the target-aware `identity(identity(42))` program is emitted
- **THEN** the bitcode contains `silk_main` and the identity instance's symbol, with calls lowered as direct calls and the module naming the selected target

#### Scenario: Repeat emission byte-identically

- **WHEN** the same target-aware program and request are emitted repeatedly in fresh processes
- **THEN** the bitcode bytes are identical

#### Scenario: Lower a trap body

- **WHEN** a lowered function's block ends in a trap
- **THEN** the emitted function terminates in an unreachable trap sequence rather than fabricating a return value

### Requirement: Booleans and comparisons emit natively

The backend SHALL read the `Bool` representation from the MIR layout plan and realize its pinned
four-byte zero-or-one representation: comparisons emit `icmp` plus a zero-extension into the
four-byte destination local, and user branches reuse the existing conditional-branch emission on
the nonzero test. The backend MUST NOT substitute a one-bit stored representation. Emission SHALL
remain deterministic, and the compiled corpus SHALL agree with the interpreter on every branching
program.

#### Scenario: Emit a comparison

- **WHEN** a program comparing two integers is emitted with the canonical `Bool` layout entry
- **THEN** the textual IR contains an `icmp` and a zero-extension realizing the planned four-byte boolean local

#### Scenario: Branch natively arm by arm

- **WHEN** a branching corpus program compiles and runs
- **THEN** its native exit value equals the interpreter's result for the same condition

#### Scenario: Refuse an inconsistent Bool plan

- **WHEN** malformed MIR presents scalar facts that conflict with the selected target's canonical `Bool` profile
- **THEN** MIR verification rejects the program before backend emission rather than allowing the backend to choose a representation

## ADDED Requirements

### Requirement: Backends enforce canonical target compatibility

Each backend SHALL declare the canonical targets it can emit and SHALL return a typed
target-incompatibility outcome before constructing backend state when the MIR plan selects another
target. The existing direct WebAssembly backend SHALL accept `wasm32-unknown-unknown`, consume the
same planned `I32` and `Bool` entries as the interpreter and LLVM backend, and MUST NOT ignore the
plan or emit WebAssembly for a native-target MIR program.

#### Scenario: Emit WebAssembly for WebAssembly MIR

- **WHEN** the direct WebAssembly backend receives MIR planned for `wasm32-unknown-unknown`
- **THEN** it emits its existing deterministic module while realizing the plan's four-byte `I32` and `Bool` representations

#### Scenario: Reject a native plan in the WebAssembly backend

- **WHEN** the direct WebAssembly backend receives MIR planned for `aarch64-apple-darwin`
- **THEN** it returns a typed target-incompatibility outcome before constructing a WebAssembly module
