# bootstrap-backend Specification

## Purpose
The nominal `Backend` service: one operation consuming the whole monomorphized MIR program plus
an explicit target layout and codegen request, and the bootstrap `LlvmBackend` lowering MIR
through the Silk LLVM builder to deterministic bitcode — the seam the driver and any future
backend share, with textual LLVM IR as an inspection artifact only.
## Requirements
### Requirement: The Backend service is a nominal contract

The `Backend` service SHALL expose one emission operation consuming the whole monomorphized MIR
program, the explicit target layout, and a codegen request (debug or release), producing one
program artifact. Consumers MUST NOT inspect backend identity: one compilation request produces
one MIR program, one backend module, and one artifact — source modules are semantic namespaces,
not codegen units. The relocatable-object half of the contract SHALL be fulfilled by the pinned
native toolchain orchestration, which turns the artifact's bitcode into one target object under
a fixed optimization profile.

#### Scenario: Emit one artifact per program

- **WHEN** a lowered program with several functions is emitted through the service
- **THEN** exactly one artifact results, containing every function's symbol, regardless of which source modules the instances came from

#### Scenario: Complete the object contract

- **WHEN** the artifact's bitcode is passed through the pinned toolchain's object emission
- **THEN** one relocatable object for the requested profile results, completing the backend contract for the compilation

### Requirement: LlvmBackend lowers MIR to deterministic bitcode

The `LlvmBackend` SHALL lower MIR through the Silk LLVM builder — functions, blocks, literals,
moves, calls, drops, returns, branches, and traps mapped onto builder operations — and SHALL emit
LLVM bitcode directly, without loading `libLLVM`, using the LLVM C API, or requiring a
compiler-private native FFI. The artifact SHALL record each instance's deterministic symbol, with
the entry instance always named `silk_main`. Identical MIR programs, layouts, and requests SHALL
produce byte-identical bitcode across fresh processes, gated by a committed digest.

#### Scenario: Lower the nested-call program

- **WHEN** the lowered `identity(identity(42))` program is emitted
- **THEN** the bitcode contains `silk_main` and the identity instance's symbol, with calls lowered as direct calls

#### Scenario: Repeat emission byte-identically

- **WHEN** the same program, layout, and request are emitted repeatedly in fresh processes
- **THEN** the bitcode bytes are identical

#### Scenario: Lower a trap body

- **WHEN** a lowered function's block ends in a trap
- **THEN** the emitted function terminates in an unreachable trap sequence rather than fabricating a return value

### Requirement: Textual LLVM IR is an inspection artifact

The backend SHALL render textual LLVM IR from the same builder state for debugging and
inspection. The text is implementation-specific, is not a phase interchange format, and carries
no compatibility promise.

#### Scenario: Render IR beside bitcode

- **WHEN** a program is emitted
- **THEN** the artifact includes the rendered IR text naming the same symbols as the bitcode

### Requirement: Debug builds emit native LLVM debug metadata

When the codegen request selects a debug build, the backend SHALL emit LLVM debug metadata — a
compile unit, source file, one subprogram per function, and instruction locations whose line and
column positions are derived from the original source bytes only at emission time. Release builds
SHALL omit debug metadata.

#### Scenario: Attach debug locations

- **WHEN** a program is emitted with a debug request
- **THEN** the IR contains a compile unit, one subprogram per function, and located instructions derived from the operations' source spans

#### Scenario: Keep release builds clean

- **WHEN** the same program is emitted with a release request
- **THEN** the IR contains no debug metadata

### Requirement: The backend emits checked native arithmetic

The LLVM backend SHALL lower each MIR binary operation to overflow-checked native code: add,
subtract, and multiply through the signed with-overflow intrinsics whose overflow flag branches
to a trapping block, and divide and remainder guarded by explicit zero-divisor and
minimum-by-minus-one checks branching to a trapping block before the `sdiv`/`srem` instruction.
The emitted program's behavior SHALL agree with the interpreter across the corpus — matching exit
values for completing programs and abnormal termination for trapping ones — and emission SHALL
remain deterministic, gated by the committed bitcode digest and IR goldens.

#### Scenario: Emit a checked addition

- **WHEN** a program adding two values is emitted
- **THEN** the textual IR contains the signed add-with-overflow intrinsic and a conditional branch to a trapping block

#### Scenario: Guard a division natively

- **WHEN** a program dividing two values is emitted and run with a zero divisor
- **THEN** the native executable terminates abnormally exactly as the interpreter blocked

#### Scenario: Keep arithmetic emission deterministic

- **WHEN** the committed arithmetic fixture is emitted repeatedly in fresh processes
- **THEN** the bitcode digest and IR text equal the committed goldens byte-for-byte

