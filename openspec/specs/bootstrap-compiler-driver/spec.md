# bootstrap-compiler-driver Specification

## Purpose
The end-to-end compiler driver: one orchestration path from a compilation request to a running
native executable at the requested destination, with the differential harness and byte-identical
determinism gates enforced continuously and per-phase reporting as the observability substrate
issue 09 builds on.
## Requirements
### Requirement: The driver compiles a request to a durable executable artifact

The driver SHALL orchestrate closure loading, header collection, elaboration, ownership, instance discovery, canonical target layout, MIR lowering, explicitly selected compatible backend emission, and artifact-kind-specific finalization to a requested durable destination under a fixed optimization profile. Native LLVM requests SHALL emit an object, compile the native shim, and link an executable. LLVM Wasm requests SHALL use the pinned LLVM-to-Wasm finalizer, while direct WebAssembly requests SHALL atomically commit the backend's validated module bytes without invoking Clang. The selected target-aware MIR program SHALL pass through later phases without a second layout value. An unavailable entry, unsupported backend-target pair, inconsistent layout, finalization failure, or toolchain failure SHALL surface as a closed outcome naming the failing stage and provenance, never as a thrown error.

#### Scenario: Compile and run a native program

- **WHEN** the driver compiles the nested-call corpus through LLVM to a supported host destination
- **THEN** a native executable exists there and running it exits with the evaluator's result

#### Scenario: Produce LLVM WebAssembly

- **WHEN** the request selects backend `llvm` and target `wasm32-unknown-unknown`
- **THEN** the driver commits an instantiable `.wasm` module produced by the pinned LLVM-to-Wasm path and exporting `silk_main`

#### Scenario: Produce direct WebAssembly

- **WHEN** the request selects backend `wasm` and target `wasm32-unknown-unknown`
- **THEN** the driver commits the backend's validated `.wasm` bytes atomically without invoking Clang

#### Scenario: Surface an entry failure as a closed outcome

- **WHEN** the request's root module has no valid entry
- **THEN** the driver returns a no-entry outcome carrying the discovery reason and phase report without finalizing an artifact

#### Scenario: Stop on an unsupported backend-target pair

- **WHEN** the request selects a backend and target outside their compatibility matrix
- **THEN** the driver returns a target-stage failure before MIR lowering, backend emission, or external tool invocation

#### Scenario: Name the failing finalization stage

- **WHEN** a selected external finalizer fails
- **THEN** the driver returns a failed outcome naming the exact finalization stage with command provenance

### Requirement: Driver outcomes identify backend and artifact kind

Every successful driver outcome SHALL retain the canonical backend identifier, target, artifact kind, durable path, symbols, diagnostics, and phase report. Reports SHALL include only phases actually executed and SHALL distinguish backend emission from artifact finalization.

#### Scenario: Report a direct Wasm build

- **WHEN** the direct WebAssembly backend successfully produces a durable module
- **THEN** the outcome identifies backend `wasm`, target `wasm32-unknown-unknown`, artifact kind WebAssembly module, its destination, and no Clang phases

#### Scenario: Report an LLVM Wasm build

- **WHEN** LLVM successfully produces a durable WebAssembly module
- **THEN** the outcome identifies backend `llvm`, the same canonical target, the WebAssembly module kind, and the executed LLVM finalization phases

### Requirement: The differential harness is a continuous check

The fixed corpus SHALL run through both the interpreter and native compilation-and-execution as
part of the test suite CI enforces. For completing programs the native exit status SHALL equal
the interpreter's `i32` result; trap-blocked programs SHALL terminate abnormally natively;
recursion-blocked programs SHALL still compile. A disagreement SHALL fail the build naming the
program and both sides' outcomes.

#### Scenario: Agree on completing programs

- **WHEN** every completing corpus program is interpreted and natively executed
- **THEN** each native exit status equals the interpreted result

#### Scenario: Fail on divergence

- **WHEN** a corpus program's native outcome differs from the interpreter's
- **THEN** the harness fails naming the program and the diverging outcomes

### Requirement: Determinism gates are enforced continuously

The test suite CI runs SHALL enforce the pinned gates: identical compiler, source snapshot,
target, profile, and toolchain inputs produce byte-identical syntax, HIR, and MIR textual
encodings and LLVM bitcode.

#### Scenario: Gate the four encodings

- **WHEN** the determinism suite runs
- **THEN** syntax, HIR, and MIR encodings and the bitcode digest are all byte-compared against committed goldens and repeated fresh runs

### Requirement: Every phase reports its work

Each driver run SHALL produce one report with an entry per executed phase, including the canonical
target-layout phase between instance discovery and MIR lowering: elapsed time, input and output
counts, diagnostic counts, and the engine-heap memory total observed after the phase — the
bootstrap approximation of allocator-backed totals until self-hosting owns real allocators.
Reports are observability data, not artifacts, and are exempt from byte-identity.

#### Scenario: Report the full pipeline

- **WHEN** the driver compiles any valid request for a supported target
- **THEN** the report lists every executed phase in order, including target layout between instances and MIR, with elapsed time, input and output counts, diagnostic counts, and memory totals


### Requirement: The driver compiles internal aggregate call chains

The compiler driver SHALL carry nominal struct values through declaration analysis, HIR, ownership,
runtime discovery, target layout and calling-shape selection, MIR lowering, backend emission, and
native linking while preserving the fixed scalar host entry boundary. Struct construction and
projection failures SHALL remain ordinary deterministic diagnostics and SHALL prevent only invalid
downstream work.

#### Scenario: Compile through a public factory

- **WHEN** a root module calls another module's public factory, passes the returned struct through an internal function, and returns a projected `i32`
- **THEN** the driver produces a native executable whose exit result matches MIR evaluation

#### Scenario: Refuse external raw construction

- **WHEN** a root module attempts a raw literal for another module's struct
- **THEN** the driver reports the defining-module diagnostic and performs no MIR or backend work for the invalid program

### Requirement: Aggregate differential and determinism gates remain continuous

The driver corpus SHALL include valid, invalid, nested, empty, reordered, cross-module, moved,
projected, and cleanup-bearing aggregate programs. Native execution, WebAssembly execution, and MIR
evaluation SHALL agree where applicable, and repeated fresh-process compilation SHALL preserve
diagnostics, HIR, layouts, MIR, symbols, IR, WAT, and bitcode exactly.

#### Scenario: Run the aggregate parity corpus

- **WHEN** continuous checks execute the aggregate corpus on supported targets
- **THEN** every valid program agrees across evaluation and available backends and every invalid program preserves its expected phase-owned diagnostics

### Requirement: The driver compiles fixed-array programs end to end

The driver SHALL carry array syntax, types, ownership, reachability, layout, calling shapes, MIR,
evaluation, and emission through the existing phase report while retaining the scalar host entry.
Array failures SHALL remain phase-owned closed outcomes rather than thrown host errors.

#### Scenario: Compile an array-of-structs program

- **WHEN** a valid program constructs an array of structs and returns one indexed scalar field
- **THEN** the driver produces a native executable whose result agrees with MIR evaluation

### Requirement: Array differential and determinism gates remain continuous

The driver corpus SHALL cover inferred, contextual, empty, nested, struct-element, moved, indexed,
out-of-bounds, mismatched, and unavailable-layout arrays. Repeated compilation SHALL preserve
diagnostics, HIR, layouts, MIR, symbols, IR, WAT, and binary output exactly.

#### Scenario: Run the fixed-array corpus

- **WHEN** continuous checks execute the corpus on supported targets
- **THEN** valid programs agree across available engines and invalid programs retain their expected phase-owned outcomes

### Requirement: Mutable-loop programs retain three-engine parity

The driver corpus SHALL cover immutable-write rejection, scalar and place assignment, Copy and
move-only replacement, zero and multiple iterations, nested loops, `break`, `continue`, `return`,
checked index traps, loop-header ownership failure, and cleanup. Supported programs SHALL agree
across MIR evaluation, native execution, and WebAssembly execution; invalid programs SHALL retain
their phase-owned outcomes before artifact construction.

#### Scenario: Run an array algorithm

- **WHEN** the corpus mutates and scans a fixed-size array through a structured loop
- **THEN** every available engine produces the same result and traceable control decisions

### Requirement: Control DAG artifacts are deterministic

Repeated compilation in fresh processes SHALL preserve semantic loop facts, HIR regions, ownership
fixed points, cleanup plans, MIR DAG nodes and topological encoding, evaluation traces, symbols, LLVM
IR and bitcode, WAT, and WebAssembly bytes exactly for equivalent inputs.

#### Scenario: Repeat nested-loop compilation

- **WHEN** one nested-loop program is compiled repeatedly for supported targets
- **THEN** every compiler-owned artifact is identical and backend-local control conversion is deterministic

### Requirement: Structural-union programs retain three-engine parity

The differential corpus SHALL cover canonical normalization, nominal injection, union widening,
call/return transport, struct and array containment, mutation, loop transport, move-only cleanup,
and unavailable/invalid conversions. Supported programs SHALL agree across MIR evaluation, native
execution, and WebAssembly execution; invalid programs SHALL stop at their phase-owned diagnostic
before artifact construction.

#### Scenario: Run an aggregate union program

- **WHEN** the corpus stores, passes, widens, replaces, and drops a union of nominal aggregates
- **THEN** every available engine agrees on completion, traps, active payload behavior, and cleanup

### Requirement: Structural-union artifacts are deterministic

Repeated fresh compilation SHALL preserve source and semantic union facts, normalized identities,
HIR, ownership, instance order, layouts, calling shapes, MIR mappings, traces, symbols, LLVM IR and
bitcode, WAT, and WebAssembly bytes exactly for equivalent inputs.

#### Scenario: Repeat equivalent union compilations

- **WHEN** equivalent union programs compile repeatedly for supported targets
- **THEN** every compiler-owned artifact and backend-private realization is byte-identical

### Requirement: Exhaustive-match programs retain three-engine parity

The differential corpus SHALL cover Copy, consuming, shared, and exclusive matches; precise nominal
and union scrutinees; nested field patterns; guarded fallthrough; universal coverage; exact and union
result joins; branch cleanup; loops and mutation around matches; and invalid coverage, typing,
binding, borrow, and ownership states. Supported programs SHALL agree across MIR evaluation, native
execution, and WebAssembly execution, while invalid programs SHALL stop at their phase-owned outcome.

#### Scenario: Run a consuming match algorithm

- **WHEN** the corpus loops over aggregate-contained unions, consumes one value, matches it, and returns a bound scalar
- **THEN** every available engine agrees on selected arm, result, traps, active payload behavior, and cleanup

### Requirement: Exhaustive-match artifacts are deterministic

Repeated fresh compilation SHALL preserve match syntax, facts, coverage sets, HIR regions, ownership,
instance order, layouts, MIR, traces, symbols, and backend artifacts exactly for equivalent inputs.

#### Scenario: Repeat a guarded match corpus

- **WHEN** equivalent guarded and nested matches compile repeatedly for supported targets
- **THEN** every compiler-owned artifact and backend-private realization is byte-identical

### Requirement: One compiler-shaped algorithm accepts the algorithmic language slice

The continuous compiler acceptance suite SHALL compile one unchanged multi-module Silk program that
combines cross-module declarations and calls, nominal structs, fixed arrays, checked indexing,
operators, mutable bindings, structured loops, normalized structural unions, guarded exhaustive
matching, and target-aware aggregate layout. Logical evaluation, native execution, and direct
WebAssembly execution MUST all complete with the same pinned result.

#### Scenario: Run the canonical remaining-member fold

- **WHEN** the acceptance suite compiles and runs the fixed-input remaining-member coverage fold
- **THEN** logical evaluation, native execution, and direct WebAssembly execution all return `42`

### Requirement: Composed acceptance artifacts are deterministic

The compiler SHALL retain deterministic source closure, semantic, HIR, ownership, instance, layout,
MIR, evaluation, native, and WebAssembly artifacts for the compiler-shaped acceptance program.

#### Scenario: Repeat the acceptance program in a fresh process

- **WHEN** equivalent acceptance module maps are compiled repeatedly in fresh processes
- **THEN** every compiler-owned encoding, evaluation trace, symbol set, target text, and binary hash agrees exactly

### Requirement: Differential gates cover generic specialization

The compiler driver corpus SHALL include valid inferred and explicit specializations, multiple
instances of one declaration, generic nominal layouts, recursive same-argument calls, invalid
arity and inference, and fresh-process determinism. Completing programs SHALL agree across
evaluation, native LLVM, and direct WebAssembly for their selected targets.

#### Scenario: Compare a multi-specialization program
- **WHEN** the corpus compiles and runs one declaration at two concrete argument types
- **THEN** all three engines agree on the result and the fresh-process artifacts remain identical

#### Scenario: Keep invalid inference out of lowering
- **WHEN** a corpus program cannot determine one type argument from supplied arguments
- **THEN** it produces the committed semantic diagnostic and no runtime instance, layout, or MIR function

### Requirement: Runtime slices preserve three-engine parity

The canonical multi-module coverage fold SHALL accept a shared runtime slice, use its logical
length, and be invoked with at least two distinct fixed-array lengths through one discovered
function instance. Logical evaluation, native execution, and direct Wasm execution SHALL complete
with the agreed result `42`, and their artifacts SHALL remain deterministic.

#### Scenario: Generalize the coverage fold

- **WHEN** the acceptance entry calls the same coverage fold with the reviewed minimal and complete arrays
- **THEN** instance discovery reports one fold instance and all three engines return the pinned result `42`

### Requirement: Exclusive slices preserve caller-visible mutation across engines

One compiler-shaped acceptance program SHALL pass a mutable fixed array to an ordinary helper as an
exclusive slice, replace an element through a runtime index, return, and immediately inspect the
original owner. Logical, native, and Wasm execution MUST agree on the changed value and cleanup
trace.

#### Scenario: Mutate through a helper

- **WHEN** the exclusive-slice helper replaces one move-only aggregate element and returns to its caller
- **THEN** every engine observes the replacement in the caller's array and cleans the displaced and remaining elements exactly once

### Requirement: Slice acceptance exercises failure boundaries

The compiler corpus SHALL retain deterministic negative cases for implicit decay, immutable
exclusive borrowing, conflicting argument loans, recursive slice storage or return, unsupported
standalone binding, non-Copy extraction, unrepresentable length, and runtime out-of-bounds access.

#### Scenario: Repeat invalid slice compilation

- **WHEN** each invalid slice fixture is compiled repeatedly in fresh processes
- **THEN** it yields the same phase-owned diagnostic or runtime trap without producing a successful conflicting artifact

### Requirement: usize has target-aware differential acceptance

The compiler acceptance surface SHALL compare evaluator, native, and Wasm results for `usize`
programs whose values fit 32 bits, compare evaluator and native results above 32 bits, and require
Wasm target rejection for out-of-range literals before emission. Fresh-process runs SHALL preserve
identical facts, layouts, MIR, textual artifacts, and binary artifacts for the same target.

#### Scenario: Compare the shared range

- **WHEN** a canonical fixture uses checked `usize` arithmetic entirely within the 32-bit range
- **THEN** evaluator, native execution, and Wasm execution return the same unsigned value

#### Scenario: Compare the native-only range

- **WHEN** a canonical native fixture computes a valid value above `2^32 - 1`
- **THEN** evaluator and native execution agree exactly while the Wasm-targeted counterpart is rejected before MIR

### Requirement: Effect acceptance covers both outcome branches deterministically

The compiler corpus SHALL execute Effect success, propagation, exact recovery, residual-row rejection,
ownership cleanup, and trap separation through evaluation, native, and Wasm where valid. Equivalent
fresh-process compilations SHALL preserve semantic facts, layout, MIR, text, and binary artifacts.

#### Scenario: Compare success and recovery across engines

- **WHEN** a canonical fixture is compiled once for its success input and once for its handled failure input
- **THEN** all three engines agree and repeated builds are byte-identical

#### Scenario: Reject an unresolved executable failure

- **WHEN** an ordinary entry attempts to run an Effect with a nonempty residual row
- **THEN** compilation rejects it before MIR emission and creates no executable artifact

### Requirement: Driver acceptance covers Effect and owned allocation vertically

The compiler corpus SHALL cover Effect construction versus execution, capture modes, catch, retry,
provider placement, Layout validation, allocation success and exhaustion, partial initialization,
Vector growth, explicit drop, typed-failure cleanup, and trap separation across evaluator, native,
and Wasm where valid. Fresh runs SHALL preserve every textual and binary artifact deterministically.

#### Scenario: Compile the owned-token milestone

- **WHEN** a compiler-shaped program tokenizes borrowed runtime bytes into a growable owned Vector and returns it through an Effect
- **THEN** evaluation, native, and Wasm agree on tokens, ownership, allocation failures, cleanup, target layout, and emitted artifacts

### Requirement: Driver acceptance covers first-class callables vertically

The compiler corpus SHALL cover named function values, automatic sections, callable bindings and
returns, generic higher-order functions, Copy and borrowed captures, exclusive mutation, owned
take-once capture, Effect map, flatMap, tap and logging composition, retry rejection, grouped and
ungrouped run, cleanup, and diagnostics across evaluator, native, and Wasm where valid. Fresh runs
SHALL preserve syntax, semantic facts, HIR, ownership, instances, MIR, textual artifacts, and binary
artifacts deterministically.

#### Scenario: Compile the callable Effect milestone

- **WHEN** a canonical program maps and taps an Effect through stored reusable and consuming sections
- **THEN** evaluation, native, and Wasm agree on success, effect nesting, invocation access, ownership, and cleanup

#### Scenario: Reject invalid reuse before emission

- **WHEN** the corpus invokes a take-once section twice or supplies it to a repeatable callback contract
- **THEN** compilation emits the stable ownership or callable-mode diagnostic and no conflicting runtime artifact

#### Scenario: Preserve deterministic callable artifacts

- **WHEN** equivalent callable programs compile repeatedly in fresh processes
- **THEN** generated environment identities, instance ordering, MIR, symbols, and emitted artifacts are byte-identical

### Requirement: Frontend failures gate artifact production

The driver SHALL run recoverable frontend phases far enough to collect deterministic source
diagnostics and partial resolver facts, then gate every artifact-producing phase on that frontend
result. Any error diagnostic SHALL produce a closed source-rejected outcome carrying the merged
diagnostics, loaded source catalog, and executed-phase report. Any captured source-resolution
failure SHALL fail compilation as a typed operational failure carrying the canonically ordered
failures and available frontend report. Neither case SHALL perform MIR lowering, backend emission,
object emission, shim compilation, linking, or destination commit.

#### Scenario: Reject source errors before lowering

- **WHEN** closure loading and semantic analysis complete with one or more error diagnostics
- **THEN** the driver returns a source-rejected outcome and reports no MIR, backend, object, shim, or link phase

#### Scenario: Fail operationally after partial resolution

- **WHEN** closure loading captures a typed source-resolution failure
- **THEN** the driver fails with the ordered resolution failures and invokes no artifact-producing phase

#### Scenario: Preserve tooling-style recovery before the gate

- **WHEN** one import fails while another module remains analyzable
- **THEN** the driver's frontend result retains the successful module's facts and available diagnostics before compilation stops

#### Scenario: Compile only a clean frontend

- **WHEN** source resolution succeeds and recoverable frontend phases produce no error diagnostics
- **THEN** the driver proceeds through MIR lowering, backend emission, and the requested toolchain stages

### Requirement: Allocation acceptance covers the substrate vertically

The continuous compiler corpus SHALL cover valid and invalid layout formation, role-selected
allocator provision, successful and exhausted allocation, provider access ending before result
cleanup, affine moves, typed buffers and slots, initialization and rollback, restricted-hook
rejection, explicit early drop, every structured exit, trap separation, zero-sized and over-aligned
storage, and post-failure reuse. Evaluator, native, and Wasm SHALL agree on every logical result and
cleanup trace. Fresh-process runs SHALL keep syntax, facts, ownership, HIR, instances, target layout,
MIR, traces, textual output, and binary artifacts deterministic.

#### Scenario: Compile the construction-guard milestone

- **WHEN** a canonical program allocates runtime-counted move-only slots, initializes a guarded prefix, and exits through success and injected typed failure
- **THEN** all three engines agree on values, `OutOfMemory`, hook order, exact releases, target layout, and emitted artifacts

#### Scenario: Reject unsafe misuse before artifacts

- **WHEN** source accesses a Slot safely, escapes it, consumes its live buffer, duplicates an Allocation, or declares an invalid Drop hook
- **THEN** compilation emits the responsible stable diagnostic and produces no MIR or executable artifact for that program

#### Scenario: Preserve allocation-free stability

- **WHEN** an allocation-free corpus program compiles after the substrate is added
- **THEN** it gains no allocator witness, allocation layout, reclaim ticket, Drop hook, or heap operation solely because the feature exists

### Requirement: Scanner acceptance proves the owned sequence vertically

The driver's continuous gates SHALL include a scanner written in Silk that borrows runtime-sized
source bytes as a slice and returns an owned `Vector<Token>`, growing across at least one
reallocation. The differential harness SHALL verify identical token results across the evaluator,
LLVM native execution, and instantiated Wasm; a failure-ordinal sweep over every allocation the
scanner performs SHALL confirm each injected `OutOfMemory` propagates typed, rolls back partial
initialization, and leaks nothing; and fresh-process artifact determinism SHALL cover the scanner
and its standard-library dependencies.

#### Scenario: Three engines agree on scanned tokens

- **WHEN** the scanner acceptance program tokenizes input long enough to force vector growth
- **THEN** the evaluator, native executable, and Wasm instance produce identical token sequences and exit values

#### Scenario: Exhaustion at every ordinal leaks nothing

- **WHEN** the harness injects allocation failure at each successive allocation ordinal of the scanner run
- **THEN** every run fails with typed `OutOfMemory` or completes, releases every live owner exactly once, and the native run reports no leaked allocation

#### Scenario: Scanner artifacts are deterministic

- **WHEN** the scanner acceptance program is compiled in two fresh processes
- **THEN** every published artifact, including those of imported standard-library modules, is byte-identical
