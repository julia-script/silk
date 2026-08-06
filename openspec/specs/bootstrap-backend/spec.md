# bootstrap-backend Specification

## Purpose
The nominal `Backend` service: one operation consuming a target-aware monomorphized MIR program and
a codegen request, and the bootstrap `LlvmBackend` lowering MIR through the Silk LLVM builder to
deterministic bitcode — the seam the driver and any future backend share, with textual LLVM IR as
an inspection artifact only.
## Requirements
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


### Requirement: Native emission realizes compiler-planned aggregates

The native backend SHALL lower canonical nominal MIR values, construction, projection, whole moves,
drops, parameters, and results according to the snapshot's physical layout entries and aggregate
calling shapes. Internal function signatures and calls SHALL use the compiler-selected flattened
scalar lanes in canonical field-path order. Emission MUST NOT recalculate field order, offsets,
padding, or aggregate ABI from LLVM types.

#### Scenario: Emit a native factory and projection

- **WHEN** an internal factory returns a nested struct and its caller projects a scalar field
- **THEN** emitted LLVM IR realizes the selected lane order and field identity and the executable returns the projected value

#### Scenario: Emit an empty marker

- **WHEN** an internal call passes or returns an empty struct
- **THEN** native emission preserves the logical nominal contract while emitting zero runtime lanes for that value

### Requirement: WebAssembly emission realizes compiler-planned aggregates

The direct WebAssembly backend SHALL lower the same nominal MIR operations and selected aggregate
calling shapes to WebAssembly scalar parameters, results, and locals without using engine-owned GC
struct layout. It SHALL flatten lanes by the compiler-provided canonical field paths and MUST NOT
derive a second layout or ABI from WebAssembly value types.

#### Scenario: Emit a WebAssembly aggregate call

- **WHEN** a WebAssembly snapshot contains an internal function passing and returning a nested struct
- **THEN** WAT and binary emission use the selected ordered scalar lanes and execution returns the same projected scalar as evaluation

#### Scenario: Avoid an engine-owned struct layout

- **WHEN** WebAssembly emits a nominal Silk value
- **THEN** no WebAssembly GC struct definition is treated as the authority for Silk field offsets or calling shape

### Requirement: Aggregate backend parity is exact

For every supported target, valid aggregate programs SHALL agree with MIR evaluation on the final
result and invalid or incompatible aggregate plans SHALL be rejected before artifact construction.
Equivalent emissions SHALL preserve canonical symbols, declaration order, debug provenance, and
deterministic text and binary output.

#### Scenario: Compare native, WebAssembly, and evaluation

- **WHEN** one program constructs, moves, returns, and projects nested structs
- **THEN** native execution, WebAssembly execution, and evaluation produce the same scalar result

#### Scenario: Reject a mismatched aggregate plan

- **WHEN** a backend receives a nominal MIR function whose plan lacks or contradicts its calling shape
- **THEN** emission returns a typed incompatibility outcome without creating a partial artifact

### Requirement: Native emission realizes compiler-planned arrays

Native emission SHALL realize logical array locals, parameters, results, construction, indexing,
projection, moves, and drops from the selected repeated-element layout and lane paths. Dynamic index
checks SHALL trap before selection. Emission MUST NOT recalculate stride, total size, field order,
element order, or calling shape from backend types.

#### Scenario: Execute a native dynamic index

- **WHEN** a native program indexes an array in bounds
- **THEN** its executable returns the selected value using the compiler-selected element path

### Requirement: WebAssembly emission realizes the same array plan

Direct WebAssembly emission SHALL use the same canonical lane sequence for array parameters, results,
locals, construction, and projection, including multi-value and zero-lane internal contracts. Bounds
checks SHALL have the same success and trap behavior as evaluation and native execution.

#### Scenario: Emit a zero-length internal result

- **WHEN** a WebAssembly function returns `Array<I32, 0>` internally
- **THEN** emission preserves the logical call with zero runtime results and no engine-owned array layout

### Requirement: Array backend parity is exact

For every supported target, valid indexed-array programs SHALL agree with evaluation and invalid or
incompatible plans SHALL be rejected before artifact construction. Equivalent emissions SHALL retain
stable symbols and deterministic text and binary output.

#### Scenario: Compare three engines

- **WHEN** one program constructs, passes, indexes, and projects an array of structs
- **THEN** native execution, WebAssembly execution, and MIR evaluation produce the same scalar result

### Requirement: Backends lower the compiler-owned control DAG

Every backend SHALL consume the verified structured control DAG as its sole control-flow input and
convert it to the target's required form. A backend MUST NOT infer loops, conditionals, lexical exits,
or cleanup regions from a flattened cyclic graph, and MUST preserve the DAG's operation order,
outcomes, provenance, and cleanup behavior.

#### Scenario: Lower one DAG through two backends

- **WHEN** native and WebAssembly emission receive the same nested-loop MIR structure for their selected targets
- **THEN** both consume the same canonical regions and produce behavior matching evaluation

### Requirement: Native emission linearizes structured loops

Native LLVM emission SHALL deterministically linearize loop and conditional regions into backend-local
basic blocks, branches, and loop back-edges. The resulting LLVM CFG MAY be cyclic, but no derived block
or edge SHALL leak back into MIR or alter compiler-owned region identities.

#### Scenario: Emit a native while loop

- **WHEN** a loop repeats an indexed update until its condition becomes false
- **THEN** LLVM IR contains the required header, body, exit, and back-edge blocks and the executable matches evaluation

### Requirement: WebAssembly emission preserves structured loops

Direct WebAssembly emission SHALL map DAG loop, conditional, repeat, exit, and cleanup regions into
deterministic nested `block`, `loop`, `if`, and branch forms. It MUST NOT run a CFG-restructuring pass
or synthesize a dispatch loop for control already represented structurally.

#### Scenario: Emit nested WebAssembly loops

- **WHEN** MIR contains nested loops with inner `continue` and outer `break`
- **THEN** WAT retains deterministic nested structured control and execution matches evaluation and native code

### Requirement: Backends realize mutable place writes consistently

Native and WebAssembly emission SHALL realize checked root, field, and array-element replacement from
the compiler-planned layout and selector paths. Bounds failure SHALL trap before right-hand evaluation
or commit, and valid writes SHALL preserve complete logical values and exact replacement cleanup.

#### Scenario: Compare checked array mutation

- **WHEN** a valid program updates several array elements in a loop
- **THEN** native, WebAssembly, and evaluation return the same final projected value

### Requirement: Backends realize the compiler-owned union plan

Native LLVM and direct WebAssembly emission SHALL consume the union's compiler-owned discriminant,
member tags, payload placement, padding, calling shape, and member-slot mappings without choosing a
different representation. Injection, calls, returns, struct/array storage, reads, moves, and writes
SHALL preserve the same active member and complete payload as evaluation.

#### Scenario: Emit one union through both backends

- **WHEN** a program injects a move-only nominal value, transports it through an aggregate, and widens it
- **THEN** native and WebAssembly execution agree with evaluation on the final result and cleanup

### Requirement: Backend union dispatch remains private and deterministic

A backend MAY introduce private branches or structured target constructs to remap a widening or
clean an active payload, but it SHALL derive them only from the verified logical conversion and
cleanup mappings. Such control MUST NOT alter MIR regions or leak labels, branch depths, blocks, or
numeric target tags back into compiler-owned relationships. Equivalent emissions SHALL preserve
stable symbols, text, binary bytes, and provenance.

#### Scenario: Lower active-member cleanup

- **WHEN** a union with two differently shaped move-only members reaches cleanup
- **THEN** each backend dispatches through its private target form and releases exactly the active member

#### Scenario: Repeat backend emission

- **WHEN** one union program is emitted repeatedly for a supported target
- **THEN** LLVM IR, bitcode, WAT, WebAssembly bytes, and union provenance are identical
