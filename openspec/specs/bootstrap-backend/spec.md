# bootstrap-backend Specification

## Purpose
The nominal `Backend` service: one operation consuming a target-aware monomorphized MIR program and
a codegen request, and the bootstrap `LlvmBackend` lowering MIR through the Silk LLVM builder to
deterministic bitcode — the seam the driver and any future backend share, with textual LLVM IR as
an inspection artifact only.
## Requirements
### Requirement: The Backend service is a nominal contract

The `Backend` service SHALL expose a stable backend identifier, its canonical compatible targets, and one emission operation consuming the whole target-aware monomorphized MIR program plus a codegen request, producing one typed program artifact. It MUST NOT accept a second target-layout input or choose an alternate representation for a Silk type. One compilation request SHALL produce one MIR program, one backend module, and one artifact; source modules are semantic namespaces, not codegen units. Artifact finalization SHALL follow the artifact kind and selected target rather than assuming every backend result requires native object emission and linking.

#### Scenario: Emit one artifact per program

- **WHEN** a target-aware lowered program with several functions is emitted through the service
- **THEN** exactly one typed artifact results, containing every function's symbol regardless of which source modules the instances came from

#### Scenario: Finalize by artifact kind

- **WHEN** LLVM emits bitcode or the direct WebAssembly backend emits final module bytes
- **THEN** downstream orchestration can select the compatible finalization path without inspecting an implementation-specific display name

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

### Requirement: LlvmBackend emits wasm32-compatible bitcode

For MIR planned for `wasm32-unknown-unknown`, `LlvmBackend` SHALL realize the compiler-owned 32-bit WebAssembly layout in deterministic LLVM IR and bitcode suitable for the pinned LLVM-to-Wasm finalization path. It SHALL retain the closed entry symbol `silk_main`, and identical inputs SHALL produce byte-identical IR and bitcode across fresh processes.

#### Scenario: Emit LLVM bitcode for Wasm

- **WHEN** backend `llvm` emits a valid program planned for `wasm32-unknown-unknown`
- **THEN** the artifact contains Wasm-target LLVM bitcode with exported-entry provenance for `silk_main`

#### Scenario: Repeat LLVM Wasm emission

- **WHEN** the same Wasm-target MIR and profile are emitted through LLVM in fresh processes
- **THEN** their LLVM IR and bitcode are byte-identical


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

### Requirement: Backends privately realize verified match dispatch

Native LLVM and direct WebAssembly emission SHALL consume the verified logical match region and the
compiler-owned union layout to select the active member, project its complete payload, evaluate
guards, realize pattern bindings, and join one result. A backend MAY introduce target-private
blocks, switches, comparisons, nesting, or branches, but MUST NOT change canonical case meaning,
source decision order, ownership, cleanup, or the compiler-owned DAG.

#### Scenario: Emit one exhaustive match through both backends

- **WHEN** a program consumes and destructures a two-member union with one guarded arm
- **THEN** native and WebAssembly execution select the same arm, payload, cleanup, and result as evaluation

#### Scenario: Keep backend control private

- **WHEN** LLVM uses cyclic or block control and WebAssembly uses structured nesting for the same match
- **THEN** neither representation leaks labels, block identities, branch depths, or reconstructed edges into MIR or facade relationships

### Requirement: Backend match artifacts are deterministic

Equivalent verified matches SHALL preserve canonical symbols, source provenance, LLVM IR and
bitcode, WAT, and WebAssembly bytes across fresh processes. Invalid match or cleanup metadata SHALL
be rejected before partial artifact construction.

#### Scenario: Repeat match emission

- **WHEN** one nested exhaustive match is emitted repeatedly for each supported target
- **THEN** its target-specific text, binary bytes, symbols, and match provenance are identical

### Requirement: Backends emit deterministic concrete specializations

Native LLVM and direct WebAssembly emission SHALL lower each reachable generic-origin MIR instance
as one concrete definition using its compiler-selected type layout, calling shape, and deterministic
symbol. Backends MUST NOT merge layout-distinct instances or add runtime generic dispatch.

#### Scenario: Emit layout-distinct instances
- **WHEN** MIR contains specializations whose argument types have different selected layouts
- **THEN** each backend emits distinct concrete definitions and both executions agree with evaluation

#### Scenario: Repeat specialization symbols
- **WHEN** equivalent specialized MIR is emitted in fresh processes
- **THEN** native and WebAssembly symbol identities and artifacts are deterministic

### Requirement: Backends lower compiler-planned slice shapes

Native LLVM and direct WebAssembly emission SHALL consume the target-aware logical slice type,
typed address-and-length calling shape, element stride, loan-validated operations, and structured
control DAG supplied by the compiler. Neither backend MAY specialize a slice-taking function by
source array length, flatten an unknown-length slice into fixed element parameters, or choose an
independent slice ABI.

#### Scenario: Emit one callee for distinct source lengths

- **WHEN** the same shared-slice function is called with two fixed-array lengths
- **THEN** each backend emits one callee symbol with the target-selected address-and-length signature

#### Scenario: Preserve target-specific address lanes

- **WHEN** native and Wasm backends emit the same logical slice program
- **THEN** native uses the planned pointer-width address lane and Wasm uses the planned linear-memory address lane without changing the logical MIR

### Requirement: Address-taken arrays have authoritative contiguous storage

A backend SHALL materialize each address-taken fixed-array root in contiguous storage using the
compiler-planned element layout and SHALL treat that storage as authoritative for the duration of
its loans. Reads after a potentially mutating exclusive-slice call MUST observe storage rather than
stale scalarized values. Arrays never borrowed as slices MAY retain their existing value lowering.

#### Scenario: Reload after exclusive native mutation

- **WHEN** an LLVM caller reads an array in the same block immediately after an exclusive-slice helper returns
- **THEN** the read observes the helper's stored value rather than a pre-call SSA snapshot

#### Scenario: Preserve aggregate stride

- **WHEN** a slice views aggregate elements with target padding
- **THEN** native and Wasm address each logical index using the compiler-planned element stride and field offsets

### Requirement: Wasm frames isolate address-taken locals per invocation

Direct Wasm emission SHALL reserve aligned private linear-memory frame storage for address-taken
fixed arrays on each function invocation, including recursive and nested invocations, and SHALL
restore the previous frame state on every normal structured exit. Frame exhaustion or failed memory
growth SHALL trap deterministically. This private mechanism MUST NOT appear as Silk allocation or an
allocator requirement.

#### Scenario: Keep recursive frames distinct

- **WHEN** nested or recursive Wasm calls each borrow their own local fixed array
- **THEN** every live invocation receives distinct backing storage and slice mutation cannot alias another invocation's local array

#### Scenario: Restore a frame after early return

- **WHEN** a Wasm function with address-taken locals exits through an early structured return
- **THEN** its private frame is restored exactly once before control returns to the caller

### Requirement: Slice backend artifacts remain deterministic

Equivalent slice-bearing programs and target inputs SHALL produce byte-identical native IR,
symbols, object artifacts, Wasm text, Wasm bytes, and private frame layouts across fresh processes.

#### Scenario: Repeat slice emission

- **WHEN** the multi-length and exclusive-mutation fixtures are compiled repeatedly in fresh processes
- **THEN** their native and Wasm artifacts, symbols, layout decisions, and execution results are identical

### Requirement: Backends realize the selected Usize lane exactly

Native LLVM lowering SHALL realize the compiler-selected native `Usize` lane as an unsigned 64-bit
integer and direct Wasm lowering SHALL realize the Wasm lane as `i32` with unsigned comparison,
division, remainder, and overflow behavior. Calls, parameters, returns, locals, aggregates, and
operators MUST preserve the compiler-owned calling shape. Neither backend may narrow native values
to `I32` or use signed Wasm instructions for unsigned operations.

#### Scenario: Lower unsigned Wasm comparison

- **WHEN** a Wasm-target function compares `Usize` values above the signed `i32` boundary
- **THEN** emitted code uses unsigned comparison semantics and matches logical evaluation

#### Scenario: Return a native 64-bit value

- **WHEN** a native function returns a `Usize` value above `2^32 - 1`
- **THEN** its signature and return operation preserve the selected 64-bit lane without truncation

### Requirement: Backends realize explicit typed outcomes without unwinding

Native LLVM and direct WebAssembly SHALL realize the selected tagged success/failure shape through
ordinary returns, calls, tests, and branches. They MUST NOT use C++ exceptions, platform unwinding,
`setjmp`, `longjmp`, host exception objects, or backend-selected discriminants. Success, recovery,
propagation, cleanup, and traps SHALL agree with evaluation.

#### Scenario: Execute the same recovered failure

- **WHEN** a canonical flow fixture selects its failure path and catches the exact member
- **THEN** native, Wasm, and evaluation produce the same result and cleanup order

### Requirement: Backends realize self-contained allocation and Effect parity

Native LLVM and direct Wasm SHALL realize compiler-planned Effect outcomes, allocator witness calls,
self-contained reclaim tickets, raw-buffer operations, Vector moves, and Drop order from verified MIR.
Neither backend may choose layout, turn `OutOfMemory` into a trap, recognize an allocator kind, or
introduce a lifetime scope absent from MIR.

#### Scenario: Agree on successful and exhausted growth

- **WHEN** equivalent native and Wasm programs grow a Vector successfully and under injected exhaustion
- **THEN** both match evaluator results, failure members, element state, and cleanup traces for their selected target layouts

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

### Requirement: Native and Wasm realize self-contained allocation identically

Native LLVM and direct WebAssembly SHALL lower verified general allocator witness calls,
compiler-planned target layouts, typed `OutOfMemory`, affine allocation and reclaim tickets,
RawBuffer and Slot operations, restricted Drop, and cleanup ordering from MIR. Neither backend may
recognize an allocator implementation kind, retain a provider borrow in the result, substitute a
trap for exhaustion, choose a different typed stride, add a named lifetime scope, or promise cleanup
after a trap. Physical reclamation policy may differ, but observable logical ownership, failure,
and exactly-once release MUST match evaluation.

#### Scenario: Agree on successful allocation

- **WHEN** equivalent native and Wasm programs allocate, initialize, move, and explicitly drop one typed buffer
- **THEN** both match evaluator results, target-selected layouts, initialization order, and one logical release

#### Scenario: Agree under exhaustion

- **WHEN** deterministic exhaustion rejects a requested allocation
- **THEN** native and Wasm propagate the same `OutOfMemory`, clean earlier owners in the same order, and create no release for the rejected request

#### Scenario: Preserve zero-sized identity

- **WHEN** two zero-byte allocations remain live simultaneously
- **THEN** each backend preserves two distinct affine logical owners even if their physical address representation is shared or synthetic

### Requirement: Runtime layout operations lower natively

`Layout.make` validation and `Layout.repeat` checked repetition SHALL lower in the LLVM and
direct WebAssembly backends with the evaluator's exact semantics: power-of-two alignment
validation, aligned stride rounding, and overflow classification against the selected target's
`Usize` range, producing the same tagged union members on every engine.

#### Scenario: Repeat a layout at a runtime count

- **WHEN** a program repeats an element layout by a runtime count within range and allocates the result
- **THEN** the evaluator, native, and WebAssembly runs agree on the allocation size and result

#### Scenario: Classify overflow identically

- **WHEN** the repeated size exceeds the target's `Usize` range
- **THEN** every engine produces the overflow member and no allocation occurs

### Requirement: Owning union fields release conditionally

Cleanup of a structural-union value whose members carry reclaim obligations SHALL release
exactly the live member's obligations, selected by the union tag at runtime, in both native
backends' cleanup paths.

#### Scenario: Release only the live member

- **WHEN** a dropped union currently holds its allocation-owning member
- **THEN** exactly that allocation releases once, and dropping the same union holding its empty member releases nothing
