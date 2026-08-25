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
moves, calls, drops, returns, branches, traps, and generated entry termination mapped onto builder
operations — and SHALL realize every scalar representation from the MIR layout table exactly. It
SHALL emit LLVM bitcode directly, without loading `libLLVM`, using the LLVM C API, or requiring a
compiler-private native FFI. The artifact SHALL record the canonical target, each instance's
deterministic symbol, the explicit machine entry symbol `silk_main`, and the target-neutral entry
termination contract when present. Function order MUST NOT select the machine entry. Identical target-aware
MIR programs and requests SHALL produce byte-identical bitcode across fresh processes, gated by a
committed digest.

#### Scenario: Lower the nested-call program

- **WHEN** the target-aware `identity(identity(42))` program is emitted
- **THEN** the bitcode contains `silk_main` and the identity instance's symbol, with calls lowered as direct calls and the module naming the selected target

#### Scenario: Lower an effectful entry adapter

- **WHEN** MIR selects an effectful `()` entry with two concrete failures
- **THEN** bitcode contains a zero-parameter scalar `silk_main` adapter that returns its private closed tag and the artifact records both canonical identities, public status policy, and logical-frame metadata

#### Scenario: Ignore function order for entry naming

- **WHEN** equivalent MIR lists non-entry functions before or after the selected user entry
- **THEN** only the explicit machine adapter or selected ordinary entry is named `silk_main`

#### Scenario: Repeat emission byte-identically

- **WHEN** the same target-aware program and request are emitted repeatedly in fresh processes
- **THEN** the bitcode bytes are identical

#### Scenario: Lower a trap body

- **WHEN** a lowered function's block ends in a trap
- **THEN** the emitted function terminates in an unreachable trap sequence rather than fabricating a return value

### Requirement: Direct WebAssembly closes effectful entry outcomes

The direct WebAssembly backend SHALL emit the same explicit `silk_main` adapter semantics as the
LLVM backend without adding host imports: `0` for success and the private normalized failure tag
for an unhandled typed failure after payload cleanup. Its artifact SHALL retain the target-neutral
termination contract containing public status policy, canonical identities, and logical metadata.

#### Scenario: Emit an import-free effect entry

- **WHEN** a closed effectful entry is emitted directly to WebAssembly
- **THEN** the module remains import-free and `silk_main` returns its closed termination code

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

LLVM and direct WebAssembly SHALL lower every admitted integer width and mode according to MIR. Ordinary operations trap on pinned overflow/invalid input; named wrapping, saturating, bitwise, shift, rotate, conversion, and checked-Option operations preserve their distinct behavior. Emission SHALL match evaluation and remain deterministic.

#### Scenario: Emit checked signed addition

- **WHEN** ordinary `i32` addition is emitted
- **THEN** generated code detects overflow and reaches the trap path

#### Scenario: Emit wrapping byte addition

- **WHEN** `u8.wrappingAdd` is emitted
- **THEN** both backends wrap at eight bits without the ordinary overflow trap

### Requirement: Booleans and comparisons emit natively

The backend SHALL read the `bool` representation from the MIR layout plan and realize its pinned
four-byte zero-or-one representation: comparisons emit `icmp` plus a zero-extension into the
four-byte destination local, and user branches reuse the existing conditional-branch emission on
the nonzero test. The backend MUST NOT substitute a one-bit stored representation. Emission SHALL
remain deterministic, and the compiled corpus SHALL agree with the interpreter on every branching
program.

#### Scenario: Emit a comparison

- **WHEN** a program comparing two integers is emitted with the canonical `bool` layout entry
- **THEN** the textual IR contains an `icmp` and a zero-extension realizing the planned four-byte boolean local

#### Scenario: Branch natively arm by arm

- **WHEN** a branching corpus program compiles and runs
- **THEN** its native exit value equals the interpreter's result for the same condition

#### Scenario: Refuse an inconsistent bool plan

- **WHEN** malformed MIR presents scalar facts that conflict with the selected target's canonical `bool` profile
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

- **WHEN** a WebAssembly function returns `Array<i32, 0>` internally
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
member tags, payload placement, padding, calling shape, exact executable representation plans, and
member-slot mappings without choosing a different representation. Injection, calls, returns,
struct/array storage, reads, moves, writes, invocation, and execution SHALL preserve the same active
ordinary member and complete payload as evaluation.

#### Scenario: Emit one union through both backends

- **WHEN** a program injects scalar, array, nominal, droppable, and represented executable values into unions and transports them through aggregates
- **THEN** native and WebAssembly execution agree with evaluation on results and active-payload cleanup

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
control DAG supplied by the compiler. They SHALL compute projected borrow addresses from the
compiler's ordered field and fixed-array selectors, including checked runtime indexes and
target-planned element strides, and SHALL preserve the original root as authoritative storage.
Neither backend MAY specialize a slice-taking function by source array length, flatten an
unknown-length slice into fixed element parameters, or choose an independent slice ABI.

#### Scenario: Emit one callee for distinct source lengths

- **WHEN** the same shared-slice function is called with two fixed-array lengths
- **THEN** each backend emits one callee symbol with the target-selected address-and-length signature

#### Scenario: Preserve target-specific address lanes

- **WHEN** native and Wasm backends emit the same logical slice program
- **THEN** native uses the planned pointer-width address lane and Wasm uses the planned linear-memory address lane without changing the logical MIR

#### Scenario: Agree on runtime indexed subplace mutation

- **WHEN** the parity corpus mutates `matrix[index]` through an exclusive inner-array slice
- **THEN** native, Wasm, and evaluation return the same value and trap consistently for an invalid index

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

### Requirement: Backends realize the selected usize lane exactly

Native LLVM SHALL use the selected unsigned 64-bit `usize` lane; direct WebAssembly SHALL use `i32` with unsigned semantics. Neither backend may narrow native values or choose signed instructions independently.

#### Scenario: Compare Wasm usize values

- **WHEN** values cross the signed `i32` boundary
- **THEN** WebAssembly uses unsigned comparison and matches evaluation

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
Neither backend may choose layout, turn `OutOfMemoryError` into a trap, recognize an allocator kind, or
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
allocation or runtime callable interpreter. Backends SHALL preserve capture construction order
while reordering captured lanes by explicit parameter ordinal at invocation; they MUST NOT infer
target argument order from environment field order.

#### Scenario: Erase a non-escaping section

- **WHEN** a callable section is constructed and immediately applied with no observable identity
- **THEN** either backend may emit a direct call while preserving the same evaluator result and provenance

#### Scenario: Store an owned callable

- **WHEN** a take-once callable with an owned capture crosses an ordinary function boundary
- **THEN** native and Wasm preserve the capture until one invocation or drop and clean it exactly once

#### Scenario: Agree on callable modes

- **WHEN** the parity corpus invokes shared, exclusive, and consuming callable environments
- **THEN** native, Wasm, and evaluation agree on results, rejected repeats, mutation, and cleanup order

#### Scenario: Agree on staged positional application

- **WHEN** the parity corpus executes `combine(3)(2)(1)`
- **THEN** native, Wasm, and evaluation all invoke `combine(1, 2, 3)`

### Requirement: Native and Wasm realize self-contained allocation identically

Native LLVM and direct WebAssembly SHALL lower verified general allocator witness calls,
compiler-planned target layouts, typed `OutOfMemoryError`, affine allocation and reclaim tickets,
RawBuffer and Slot operations, shared bounds-checked recursively Copy reads including structural
unions, restricted Drop, and cleanup ordering from MIR. A shared union read SHALL load the canonical
tag and complete payload lanes without writing storage, changing owner state, allocating, or
dispatching on the active member. Neither backend may recognize an allocator implementation kind,
retain a provider borrow in the result, substitute a trap for exhaustion, choose a different typed
stride, add a named lifetime scope, or promise cleanup after a trap. Physical reclamation policy may
differ, but observable logical ownership, failure, active-member identity, read values, and
exactly-once release MUST match evaluation.

#### Scenario: Agree on successful allocation

- **WHEN** equivalent native and Wasm programs allocate, initialize, move, and explicitly drop one typed buffer
- **THEN** both match evaluator results, target-selected layouts, initialization order, and one logical release

#### Scenario: Agree under exhaustion

- **WHEN** deterministic exhaustion rejects a requested allocation
- **THEN** native and Wasm propagate the same `OutOfMemoryError`, clean earlier owners in the same order, and create no release for the rejected request

#### Scenario: Preserve zero-sized identity

- **WHEN** two zero-byte allocations remain live simultaneously
- **THEN** each backend preserves two distinct affine logical owners even if their physical address representation is shared or synthetic

#### Scenario: Agree on structural-union raw-buffer reads

- **WHEN** verified MIR reads an initialized all-Copy structural union through a shared raw-buffer borrow
- **THEN** native and Wasm return the evaluator's active member and payload, enforce the same bounds trap, perform no write or allocation, and preserve later cleanup

### Requirement: Runtime layout operations lower natively

`Layout.make` validation and `Layout.repeat` checked repetition SHALL lower in the LLVM and
direct WebAssembly backends with the evaluator's exact semantics: power-of-two alignment
validation, aligned stride rounding, and overflow classification against the selected target's
`usize` range, producing the same tagged union members on every engine.

#### Scenario: Repeat a layout at a runtime count

- **WHEN** a program repeats an element layout by a runtime count within range and allocates the result
- **THEN** the evaluator, native, and WebAssembly runs agree on the allocation size and result

#### Scenario: Classify overflow identically

- **WHEN** the repeated size exceeds the target's `usize` range
- **THEN** every engine produces the overflow member and no allocation occurs

### Requirement: The WebAssembly heap reclaims released storage

Direct WebAssembly SHALL return released storage to the heap that issued it, so that repeated
acquire and release cycles keep a bounded heap. The bound SHALL hold for arbitrary interleaved
acquire and release, not only for nested ones: a release whose block is not the most recent
acquisition MUST still make that storage available to a later request. Reclaimed storage SHALL keep
the alignment guarantee the request was served under. Reclaim SHALL be driven entirely by the owner
that consumes the reclaim ticket, at the point ownership ends; the backend MUST NOT introduce a
scheduler, a garbage collector, a background task, a moving allocator, or compaction, and MUST NOT
publish a `free` operation to Silk.

Release SHALL be emitted for every cleanup plan that consumes a reclaim ticket, including one that
invokes no Drop hook. Where a plan carries both, the Drop hooks SHALL run before the storage they
own is reclaimed.

#### Scenario: Bound an interleaved allocate-and-drop loop

- **WHEN** a Wasm program repeatedly acquires several blocks and releases them in an order that is not the reverse of their acquisition
- **THEN** the final `memory.size` stays under a fixed limit that does not grow with the cycle count

#### Scenario: Reclaim a bare allocation drop

- **WHEN** an owner whose cleanup plan invokes no Drop hook and holds only a reclaim ticket is dropped
- **THEN** Wasm emits the release rather than nothing, and the block becomes available to a later request

#### Scenario: Run Drop hooks before reclaiming

- **WHEN** an owner's cleanup plan carries both a Drop hook and a reclaim ticket
- **THEN** the hook observes the storage before it is released, matching the order native LLVM produces

### Requirement: Release-count parity is distinct from memory parity

Wasm and native LLVM SHALL report equal acquire and release counts for the same program. That
property is a consequence of ownership-driven cleanup rather than of physical reclamation, and it
SHALL be pinned independently of any claim about how much memory either backend holds while running.
A test asserting equal counts MUST NOT be read as asserting equal or bounded memory, and a heap
bound MUST NOT be inferred from count parity.

#### Scenario: Agree on release counts

- **WHEN** the same allocate-and-drop program runs on Wasm and on native LLVM
- **THEN** both report the same acquire and release counts, whatever each backend does with the storage

### Requirement: Owning union fields release conditionally

Cleanup of a structural-union value whose members carry reclaim obligations SHALL release
exactly the live member's obligations, selected by the union tag at runtime, in both native
backends' cleanup paths.

#### Scenario: Release only the live member

- **WHEN** a dropped union currently holds its allocation-owning member
- **THEN** exactly that allocation releases once, and dropping the same union holding its empty member releases nothing

### Requirement: Backends emit conservative floating operations

LLVM SHALL emit ordinary float operations without implicit fast-math flags; direct WebAssembly SHALL emit corresponding `f32`/`f64` instructions. Both SHALL realize MIR comparison, classification, total order, reinterpretation, and conversion semantics consistently.

#### Scenario: Emit f64 arithmetic

- **WHEN** accepted `f64` arithmetic lowers
- **THEN** generated artifacts contain no reassociation, no-NaN, no-infinity, or equivalent promises

### Requirement: Backends emit equivalent static data

Native LLVM and direct WebAssembly SHALL realize MIR static bytes, immutable addresses, and target-selected lengths without runtime allocation. Storage coalescing MUST NOT change observable content or identity semantics.

#### Scenario: Emit reused bytes

- **WHEN** one literal is referenced multiple times
- **THEN** both backends expose the specified identical byte views whether or not storage is coalesced

### Requirement: Backends realize explicit byte writes

Native lowering SHALL call the supplied process adapter; direct WebAssembly SHALL emit the declared host import. Both SHALL preserve MIR ordering, destinations, complete bytes, and typed failures with no implicit console behavior.

#### Scenario: Emit hosted Wasm output

- **WHEN** a Wasm program writes bytes with a supplied host
- **THEN** the host receives the same bytes and ordering as evaluation

### Requirement: Backends emit indexed static-byte reads

Native LLVM and direct WebAssembly SHALL bounds-check a runtime index against the static view's
target-sized length and load the selected `u8` from immutable static storage. Both paths MUST trap
on the same invalid indices and MUST NOT allocate or copy the complete literal at runtime.

#### Scenario: Load a static byte on both targets

- **WHEN** accepted MIR indexes a static byte view at a valid runtime position
- **THEN** native and WebAssembly execution observe the same byte as evaluation

#### Scenario: Trap a backend overrun

- **WHEN** accepted MIR executes an index equal to the static view length
- **THEN** native and WebAssembly take their canonical bounds trap before reading storage

### Requirement: Backends realize the canonical transcendental contract

Native LLVM and direct WebAssembly SHALL implement MIR sine and cosine with the same canonical
range-reduction constants, operation order, rounding points, and special-value handling as
evaluation. They MUST NOT select target `libm`, ambient host imports, fast-math flags, fused
operations, or target-specific approximations that change result bits.

#### Scenario: Emit standalone Wasm trigonometry

- **WHEN** a Wasm module contains accepted sine and cosine operations
- **THEN** it instantiates without a math host import and returns the canonical bits

#### Scenario: Compare native and evaluator bits

- **WHEN** native code executes the committed transcendental conformance vectors
- **THEN** every result bit pattern matches evaluation exactly

### Requirement: Native address-root materialization is path-correct

LLVM emission SHALL keep private address storage for every address-taken mutable root valid on all
runtime control-flow paths where a post-call reload can occur. A borrow materialized on one branch
MUST NOT cause another branch to reload uninitialized or stale storage. Defining and mutating a
root SHALL preserve its complete compiler-planned lanes, active union discriminants, and cleanup
obligation without a type- or collection-specific backend branch.

#### Scenario: Skip an exclusive-borrow branch

- **WHEN** an affine mutable root is borrowed exclusively on one branch but execution takes another branch and later crosses a call
- **THEN** native execution reloads the root's original complete value rather than uninitialized address storage

#### Scenario: Take the exclusive-borrow branch

- **WHEN** execution takes the branch that passes the root by exclusive reference and the callee mutates it
- **THEN** native execution reloads the complete callee-updated value and retains exactly one cleanup obligation

#### Scenario: Compare path-sensitive affine roots across engines

- **WHEN** taken and untaken borrow cases run through evaluation, native LLVM, and direct WebAssembly
- **THEN** all three engines produce the same scalar observations and successful exactly-once cleanup outcome

### Requirement: Backends preserve ordinary FileSystem service lowering

Backends SHALL lower `FileSystem` requirements, provision, calls, values, failures, and user-defined
implementations through the ordinary service, Effect, ownership, and call model. They MUST NOT add
FileSystem-shaped HIR or MIR operations, select a provider, or recognize portable actor names.

#### Scenario: Lower a user-defined provider

- **WHEN** a closed program supplies an ordinary source-defined `FileSystem`
- **THEN** native LLVM and direct Wasm lower its service calls through the same generic machinery used by other services

#### Scenario: Keep actor names unprivileged

- **WHEN** a user declares another legal service and values with equivalent shapes under different names
- **THEN** backends apply the same lowering behavior without requiring intrinsic inventory entries

### Requirement: Portable filesystem support is pay-for-use

Packaging canonical portable FileSystem source MUST NOT add filesystem runtime symbols or host imports
to an artifact. A direct-Wasm program using no filesystem or supplying a pure user-defined
implementation SHALL emit no OS filesystem import. Equivalent target, executable closure, and
provider source SHALL produce deterministic artifacts.

#### Scenario: Emit direct Wasm with a pure provider

- **WHEN** a program supplies a pure ordinary-source FileSystem and reaches no platform intrinsic
- **THEN** direct Wasm contains no OS filesystem imports

#### Scenario: Emit a program with no filesystem use

- **WHEN** canonical filesystem declarations are packaged but absent from executable closure
- **THEN** native and Wasm artifacts contain no filesystem runtime symbols or host imports

#### Scenario: Repeat portable emission

- **WHEN** the same portable filesystem program is emitted repeatedly for one target
- **THEN** its artifacts and service-call identities are byte-for-byte deterministic

### Requirement: Backends consume validated reachable intrinsic inventories

Each backend SHALL receive the exact intrinsic inventory retained by executable planning for its
selected target. A reachable unsupported intrinsic MUST be rejected before partial artifact
construction. An unreachable restricted intrinsic MUST NOT cause the backend to link a runtime
symbol, emit an import, or bundle a host adapter.

#### Scenario: Reject before constructing an artifact

- **WHEN** executable planning finds one reachable operation unsupported by the selected backend target
- **THEN** backend execution is not entered and no partial native or Wasm artifact is returned

#### Scenario: Omit unreachable native runtime support

- **WHEN** LLVM or direct Wasm receives a validated inventory without a native-only operation
- **THEN** the emitted artifact and link plan contain no runtime symbol or import for that operation

#### Scenario: Preserve explicit backend selection

- **WHEN** the same target is supported by more than one backend
- **THEN** availability validation uses the explicitly selected backend request without silently selecting another implementation

### Requirement: Native backends link only reachable OS runtime operations

Supported native LLVM emission SHALL lower each validated reachable OS intrinsic to its compiler-owned
runtime operation and link only the required support symbols. Runtime operations SHALL preserve
opaque affine handle identity, transferred-byte counts, normalized reasons, native codes, consuming
close, root confinement, and retryable directory iteration.

#### Scenario: Emit a native whole-file reader

- **WHEN** a native executable reaches file open, repeated read, and close through `OsFileSystem`
- **THEN** LLVM emits and links those runtime operations with the same observable protocol as evaluation

#### Scenario: Omit unused OS runtime support

- **WHEN** a native program reaches no OS filesystem intrinsic
- **THEN** its artifact and link plan contain no OS filesystem runtime symbols

### Requirement: Direct Wasm receives no implicit OS filesystem ABI

Direct-Wasm emission MUST NOT lower OS filesystem intrinsics to invented imports, JavaScript shims,
WASI calls, or a built-in virtual filesystem. Generic target-availability validation SHALL reject a
reachable OS intrinsic before backend construction and SHALL allow programs whose executable closure
does not contain one.

#### Scenario: Reject reachable OS operations

- **WHEN** a direct-Wasm entry reaches `Intrinsic.osFileOpen`
- **THEN** planning reports target unavailability and no partial Wasm module is constructed

#### Scenario: Emit a user-provided portable implementation

- **WHEN** a direct-Wasm program supplies an ordinary source-defined `FileSystem` and reaches no OS intrinsic
- **THEN** emission succeeds without filesystem imports

### Requirement: Backends preserve string semantics and presentation

Native LLVM and direct WebAssembly emission SHALL realize the target plan for `string` exactly and
SHALL agree with evaluation on static text, validated runtime views, ordinary references to string
values, explicit UTF-8 bytes, byte length, `char` traversal, checked scalar conversion, exact
equality, calls, returns, and lexical ownership behavior. Debug builds and compiler inspection
artifacts SHALL retain the logical `string` and `char` identities and present valid string values as
quoted, escaped Unicode text; byte slices SHALL remain numeric binary views even when their bytes
are valid UTF-8.

#### Scenario: Compare engines on non-ASCII text

- **WHEN** a program passes a non-ASCII `string` through calls, traverses its scalars, and observes its bytes and exact equality
- **THEN** evaluation, native execution, and Wasm execution agree on all results without allocating for the view

#### Scenario: Reject invalid scalars identically

- **WHEN** checked scalar conversion receives surrogate and above-range integers
- **THEN** native and Wasm return the same `None` outcomes as evaluation

#### Scenario: Distinguish text in a debug build

- **WHEN** a debug build contains one `string` local and one `&[u8]` local with identical valid UTF-8 bytes
- **THEN** debug metadata identifies the first as UTF-8 text and the second as binary bytes

#### Scenario: Emit deterministically

- **WHEN** equivalent string-bearing programs are emitted repeatedly for one target and profile
- **THEN** native IR, bitcode, object output, Wasm text, Wasm bytes, static data, and debug metadata are deterministic

### Requirement: Native and Wasm execute suspended Effects with bounded machine stack

Native LLVM and direct WebAssembly SHALL realize target-neutral suspension as private iterative
execution boundaries whose machine-stack usage is bounded by a constant independent of the number
of active suspended logical invocations. Each active suspendable invocation SHALL occupy one
compiler-owned execution-stack frame with one statically determined maximum layout over its resume
states. Repeated suspension by that invocation SHALL reuse the frame. A suspended child SHALL
complete or suspend through the private runner, then resume its parent with the exact typed outcome
and live state. An explicit suspension origin SHALL return transfer to the private boundary; an
ordinary suspendable runner SHALL be able to complete synchronously or relay transfer. The parent
SHALL finish its frame-state transition before the driver begins the child. Neither backend MAY
depend on LLVM `musttail`, WebAssembly tail-call instructions, host exception unwinding, a
JavaScript promise, recursive host calls, a source allocator, or typed allocation failure to provide
this guarantee.

#### Scenario: Run deep non-tail suspension on native

- **WHEN** a native release artifact executes one million non-tail recursive Effect levels separated by `Effect.suspend`
- **THEN** it returns the expected result without `SIGSEGV` and without machine-stack growth proportional to the logical depth

#### Scenario: Run deep non-tail suspension on Wasm

- **WHEN** a direct Wasm artifact executes one hundred thousand non-tail recursive Effect levels separated by `Effect.suspend`
- **THEN** it returns the expected result without a host `RangeError`, an `unreachable` trap, or host-stack growth proportional to the logical depth

#### Scenario: Preserve typed failure through the private runner

- **WHEN** a deep suspended child produces a typed failure
- **THEN** native and Wasm resume and clean the same logical frames as evaluation before returning the unchanged failure member and payload

#### Scenario: Reuse a frame across repeated suspension

- **WHEN** one invocation suspends and resumes repeatedly before completing
- **THEN** backend structural evidence shows one invocation frame reused across states rather than one source allocation per suspension

#### Scenario: Trap on execution-stack exhaustion

- **WHEN** a compiled target exhausts the finite private execution stack while adding an active suspended invocation
- **THEN** it terminates through the target trap path without constructing a typed failure or consulting a source allocator

### Requirement: Suspension runner ABIs remain private and pay for use

Coroutine frame headers, resume discriminants, step results, driver loops, target function
references, and execution-stack layouts SHALL remain backend-private and unreachable from Silk
source. A compiled program whose reachable MIR contains no suspension operation and no explicit
Execution construction MUST NOT emit or link those forms, a coroutine-frame or execution-stack
path, an Execution package/drive path, or a complete-versus-pending branch, and its established
synchronous entry and Effect-call artifact shape SHALL remain unchanged. Explicitly constructing a
non-suspending Execution SHALL retain its purpose-bound erased-body package and drive lifecycle while
omitting suspension frames, dormant continuation, Wake, notification, and atomic support.

#### Scenario: Inspect a non-suspending native artifact

- **WHEN** a closed synchronous Effect program with no explicit Execution construction is compiled to native release bitcode
- **THEN** structural inspection finds its established direct Effect calls and no suspension driver, coroutine frame, resume dispatch, execution-stack helper, Execution package, drive path, or pending branch

#### Scenario: Inspect a non-suspending Wasm artifact

- **WHEN** the same closed synchronous Effect program with no explicit Execution construction is compiled to direct WebAssembly
- **THEN** structural and linkage inspection finds no suspension table, driver, coroutine-frame path, execution-stack helper, Execution package, drive path, or pending branch

#### Scenario: Preserve explicit non-suspending ownership

- **WHEN** a statically non-suspending body is explicitly constructed as an Execution
- **THEN** structural inspection finds the owned erased-body package and drive lifecycle but no nested runner, dormant continuation, Wake, notification, or atomic support

### Requirement: Backends realize finite Effect composites without allocation

Native LLVM and direct WebAssembly SHALL realize a finite Effect composite as a statically planned
tag plus storage sufficient for its largest alternative. Construction SHALL initialize only the
selected member, execution SHALL dispatch only to its runner, and cleanup SHALL release only that
member. The representation SHALL require no source or private heap allocation, and equivalent
inputs SHALL emit deterministic artifacts.

#### Scenario: Execute the same selected member across engines

- **WHEN** a closed program constructs and runs one member of a finite compatible Effect join
- **THEN** native and WebAssembly agree with evaluation on its result, failure identity, and cleanup

#### Scenario: Inspect allocation-free lowering

- **WHEN** a finite Effect composite is emitted for either backend
- **THEN** its tag, maximum static storage, dispatch, and cleanup are present without an allocation request or universal Effect interpreter

#### Scenario: Emit joined Effects deterministically

- **WHEN** equivalent joined Effect programs are compiled repeatedly
- **THEN** native and WebAssembly artifacts preserve identical alternative ordering and bytes

### Requirement: Backends privately realize verified statement-pattern dispatch

Native LLVM and direct WebAssembly emission SHALL realize every verified expression-match and
statement-pattern selection from the compiler-owned MIR member and layout plan. Both backends SHALL
preserve source-ordered selection, retained statement bindings, branch-local borrowed bindings,
move-on-both-outcomes, active-payload cleanup, and structured joins without introducing a distinct
pattern ABI or independently choosing tags.

#### Scenario: Emit shared statement patterns

- **WHEN** one program uses recursive let destructuring and both matching and mismatching if-let selections
- **THEN** native, WebAssembly, and evaluation agree on results, binding visibility, and active-payload cleanup

### Requirement: Expected request-validation failures yield BackendError

A backend SHALL model every expected caller-caused failure (invalid MIR, invalid module, invalid
target, invalid request parameters) in its typed `BackendError` channel. It SHALL NOT throw inside
an Effect generator for an expected failure.

#### Scenario: An invalid private stack page bound is a typed failure

- **WHEN** a wasm emit request specifies an invalid `privateExecutionStackPages` bound
- **THEN** the backend yields a `BackendError`, never a thrown `RangeError` defect, and error-channel mapping observes it

### Requirement: Native and Wasm realize local shared ownership identically

Native LLVM and direct WebAssembly SHALL lower verified local-shared layout, initialization, clone,
callback access, conflict, and drop operations with the evaluator's observable transition order.
Each backend MAY choose private control-block field order, padding, reclaim representation, and
physical address, but MUST use the compiler-planned target layout and MUST NOT recognize `Shared`,
Deferred, Scheduler, or a ready inbox by spelling.

Both backends SHALL use non-atomic local state, compare the bounded strong count before any clone
mutation, keep strong-count and access state independent, form at most one callback borrow, leave
active access unchanged on conflict, restore access only after normal callback return, and clean `T`
exactly once before the final allocation release. Allocation exhaustion SHALL remain the ordinary
construction failure; clone, access, suspension, and return MUST NOT acquire an allocator channel.
Clone and access MUST NOT allocate or reallocate storage privately. Their lowering MUST NOT introduce
locks, atomics, scheduler machinery, garbage-collected backing, background work, or a runtime actor
selected by source spelling.
Fatal traps SHALL retain the existing no-unwind behavior, and strong cycles SHALL remain uncollected.

#### Scenario: Agree on successful access and cleanup

- **WHEN** one acyclic program constructs, clones, accesses, mutates, and drops an affine local shared value
- **THEN** native, Wasm, and evaluation agree on results, count transitions, access ordering, one payload cleanup, and one release

#### Scenario: Agree on every nested conflict combination

- **WHEN** shared and exclusive public access are nested in all four outer/inner combinations
- **THEN** both backends select the same conflict as evaluation before forming a second reference

#### Scenario: Trap before count mutation

- **WHEN** the target strong count is exhausted
- **THEN** each backend's clone path traps before its count store and returns no partial handle

#### Scenario: Preserve two-frame typed-failure cleanup

- **WHEN** a deeper frame drops one clone during typed-failure propagation and its caller later drops the final handle
- **THEN** every engine preserves the failure payload, performs one non-last decrement, and then cleans the value before release

#### Scenario: Distinguish physical representation from parity

- **WHEN** native and Wasm choose different private block layouts or reclaim metadata
- **THEN** source observes no layout lanes or address identity and all logical ownership outcomes remain equal

#### Scenario: Keep clone and access allocation-free

- **WHEN** either backend lowers and executes clone and callback access after one successful construction
- **THEN** structural and runtime evidence shows no further allocation or reallocation, lock, atomic, scheduler, collector, or background operation

#### Scenario: Leave a strong cycle allocated

- **WHEN** external handles to a local shared cycle are dropped on native and Wasm
- **THEN** neither backend synthesizes tracing, weak release, or cycle collection

### Requirement: Native and Wasm realize verified independent execution

Native and direct-Wasm backends SHALL lower only validated independent-execution MIR and SHALL
realize execution-owned continuation storage, exact package plans, logical drive/resume dispatch,
nested transfer, external park, fixed endpoint notification, cancellation, DestroyPending, and
cleanup. Both backends SHALL keep Execution and Wake local and use no mandatory atomic operation in
the initial model. Continuation-stack exhaustion and illegal states SHALL trap under the no-unwind
contract. Backend runtime helpers, labels, physical state tags, field offsets, and segment policies
SHALL remain private and deterministic.

#### Scenario: Resume non-LIFO on native

- **WHEN** validated MIR wakes and drives two parked executions in reverse suspension order
- **THEN** native resumes each sole continuation with evaluator-equivalent results and ordered cleanup

#### Scenario: Resume non-LIFO on direct Wasm

- **WHEN** the same validated MIR is emitted to direct Wasm
- **THEN** Wasm resumes the same continuations and agrees with evaluation and native on outcomes and ownership events

#### Scenario: Keep local wake non-atomic

- **WHEN** a local-only execution and Wake program is inspected on native and Wasm
- **THEN** neither artifact introduces thread transfer, mandatory atomic instructions, or a work-stealing runtime

#### Scenario: Trap before callbacks

- **WHEN** validated test-only state reaches a Dormant/Notifying drive or stack exhaustion trap
- **THEN** both backends trap before invoking completion or suspension callbacks and promise no unwinding cleanup

#### Scenario: Preserve backend determinism

- **WHEN** equivalent validated plans are emitted repeatedly
- **THEN** runtime helper selection, resume labels, package-layout references, and artifacts are byte-identical for each target

### Requirement: Backend artifacts expose independent-execution pay-for-use evidence

Native and direct-Wasm inspection SHALL report deterministic structural presence or absence of
direct lowering, nested suspension runtime, explicit owner/package support, dormant continuation
support, wake-control support, notification support, and atomic/thread support for each complete
specialization. Evidence SHALL follow static reachability and explicit construction rather than
runtime branch outcomes or source actor names. The evidence MUST NOT prescribe byte counts,
instruction counts, field offsets, or a stable runtime ABI.

#### Scenario: Omit all suspension support

- **WHEN** a complete artifact reaches no suspension and constructs no Execution
- **THEN** inspection reports direct lowering and absence of every suspension and execution runtime slice

#### Scenario: Retain only nested suspension support

- **WHEN** a complete artifact reaches nested transfer but no explicit Execution or park
- **THEN** inspection reports the nested runner and absence of package, dormant owner, Wake, notification, and atomic support

#### Scenario: Retain explicit ownership without Wake

- **WHEN** an artifact constructs a statically non-parking Execution
- **THEN** inspection reports exact package and drive support while reporting no wake-control or external-park support

#### Scenario: Retain external parking statically

- **WHEN** an explicit Execution specialization can reach park on any path
- **THEN** inspection reports independent continuation, wake-control, and notification support even when the observed test path completes without parking

#### Scenario: Keep the local tier non-atomic

- **WHEN** all reachable Execution and Wake values remain in one local execution domain
- **THEN** inspection reports no atomic or cross-thread runtime support

### Requirement: Backends realize verified scalar enums without new runtime metadata

Wasm and native backends SHALL lower scalar enum values, parameters, results, equality, `value`, and
match dispatch through the exact integer lane selected by the MIR representation plan. Backends SHALL
NOT choose a representation, add metadata, synthesize undeclared enum inhabitants, or treat
structural-union tags as the scalar-enum public value. Equivalent verified MIR SHALL produce
observably equivalent results on every supported engine.

#### Scenario: Lower a signed enum across engines

- **WHEN** verified MIR passes an `enum(i8)` member with discriminant `-1` through a function and returns its `value`
- **THEN** Wasm and native execution both produce `-1` through the canonical `i8` calling shape

#### Scenario: Lower enum match dispatch

- **WHEN** verified MIR matches a scalar enum exhaustively
- **THEN** each backend dispatches only among declared member decisions and selects the same arm as evaluation
